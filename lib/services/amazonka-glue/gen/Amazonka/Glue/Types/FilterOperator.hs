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
-- Module      : Amazonka.Glue.Types.FilterOperator
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.FilterOperator
  ( FilterOperator
      ( ..,
        FilterOperator_EQ,
        FilterOperator_GE,
        FilterOperator_GT,
        FilterOperator_LE,
        FilterOperator_LT,
        FilterOperator_NE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype FilterOperator = FilterOperator'
  { fromFilterOperator ::
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

pattern FilterOperator_EQ :: FilterOperator
pattern FilterOperator_EQ = FilterOperator' "EQ"

pattern FilterOperator_GE :: FilterOperator
pattern FilterOperator_GE = FilterOperator' "GE"

pattern FilterOperator_GT :: FilterOperator
pattern FilterOperator_GT = FilterOperator' "GT"

pattern FilterOperator_LE :: FilterOperator
pattern FilterOperator_LE = FilterOperator' "LE"

pattern FilterOperator_LT :: FilterOperator
pattern FilterOperator_LT = FilterOperator' "LT"

pattern FilterOperator_NE :: FilterOperator
pattern FilterOperator_NE = FilterOperator' "NE"

{-# COMPLETE
  FilterOperator_EQ,
  FilterOperator_GE,
  FilterOperator_GT,
  FilterOperator_LE,
  FilterOperator_LT,
  FilterOperator_NE,
  FilterOperator'
  #-}
