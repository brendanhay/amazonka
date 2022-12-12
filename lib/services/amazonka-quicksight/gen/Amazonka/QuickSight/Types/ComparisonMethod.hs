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
-- Module      : Amazonka.QuickSight.Types.ComparisonMethod
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ComparisonMethod
  ( ComparisonMethod
      ( ..,
        ComparisonMethod_DIFFERENCE,
        ComparisonMethod_PERCENT,
        ComparisonMethod_PERCENT_DIFFERENCE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ComparisonMethod = ComparisonMethod'
  { fromComparisonMethod ::
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

pattern ComparisonMethod_DIFFERENCE :: ComparisonMethod
pattern ComparisonMethod_DIFFERENCE = ComparisonMethod' "DIFFERENCE"

pattern ComparisonMethod_PERCENT :: ComparisonMethod
pattern ComparisonMethod_PERCENT = ComparisonMethod' "PERCENT"

pattern ComparisonMethod_PERCENT_DIFFERENCE :: ComparisonMethod
pattern ComparisonMethod_PERCENT_DIFFERENCE = ComparisonMethod' "PERCENT_DIFFERENCE"

{-# COMPLETE
  ComparisonMethod_DIFFERENCE,
  ComparisonMethod_PERCENT,
  ComparisonMethod_PERCENT_DIFFERENCE,
  ComparisonMethod'
  #-}
