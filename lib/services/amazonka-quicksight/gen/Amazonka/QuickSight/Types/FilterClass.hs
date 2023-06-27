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
-- Module      : Amazonka.QuickSight.Types.FilterClass
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.FilterClass
  ( FilterClass
      ( ..,
        FilterClass_CONDITIONAL_VALUE_FILTER,
        FilterClass_ENFORCED_VALUE_FILTER,
        FilterClass_NAMED_VALUE_FILTER
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype FilterClass = FilterClass'
  { fromFilterClass ::
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

pattern FilterClass_CONDITIONAL_VALUE_FILTER :: FilterClass
pattern FilterClass_CONDITIONAL_VALUE_FILTER = FilterClass' "CONDITIONAL_VALUE_FILTER"

pattern FilterClass_ENFORCED_VALUE_FILTER :: FilterClass
pattern FilterClass_ENFORCED_VALUE_FILTER = FilterClass' "ENFORCED_VALUE_FILTER"

pattern FilterClass_NAMED_VALUE_FILTER :: FilterClass
pattern FilterClass_NAMED_VALUE_FILTER = FilterClass' "NAMED_VALUE_FILTER"

{-# COMPLETE
  FilterClass_CONDITIONAL_VALUE_FILTER,
  FilterClass_ENFORCED_VALUE_FILTER,
  FilterClass_NAMED_VALUE_FILTER,
  FilterClass'
  #-}
