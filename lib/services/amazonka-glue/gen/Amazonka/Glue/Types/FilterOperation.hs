{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Glue.Types.FilterOperation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.FilterOperation
  ( FilterOperation
      ( ..,
        FilterOperation_EQ,
        FilterOperation_GT,
        FilterOperation_GTE,
        FilterOperation_ISNULL,
        FilterOperation_LT,
        FilterOperation_LTE,
        FilterOperation_REGEX
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype FilterOperation = FilterOperation'
  { fromFilterOperation ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern FilterOperation_EQ :: FilterOperation
pattern FilterOperation_EQ = FilterOperation' "EQ"

pattern FilterOperation_GT :: FilterOperation
pattern FilterOperation_GT = FilterOperation' "GT"

pattern FilterOperation_GTE :: FilterOperation
pattern FilterOperation_GTE = FilterOperation' "GTE"

pattern FilterOperation_ISNULL :: FilterOperation
pattern FilterOperation_ISNULL = FilterOperation' "ISNULL"

pattern FilterOperation_LT :: FilterOperation
pattern FilterOperation_LT = FilterOperation' "LT"

pattern FilterOperation_LTE :: FilterOperation
pattern FilterOperation_LTE = FilterOperation' "LTE"

pattern FilterOperation_REGEX :: FilterOperation
pattern FilterOperation_REGEX = FilterOperation' "REGEX"

{-# COMPLETE
  FilterOperation_EQ,
  FilterOperation_GT,
  FilterOperation_GTE,
  FilterOperation_ISNULL,
  FilterOperation_LT,
  FilterOperation_LTE,
  FilterOperation_REGEX,
  FilterOperation'
  #-}
