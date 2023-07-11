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
-- Module      : Amazonka.Budgets.Types.ThresholdType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Budgets.Types.ThresholdType
  ( ThresholdType
      ( ..,
        ThresholdType_ABSOLUTE_VALUE,
        ThresholdType_PERCENTAGE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The type of threshold for a notification.
newtype ThresholdType = ThresholdType'
  { fromThresholdType ::
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

pattern ThresholdType_ABSOLUTE_VALUE :: ThresholdType
pattern ThresholdType_ABSOLUTE_VALUE = ThresholdType' "ABSOLUTE_VALUE"

pattern ThresholdType_PERCENTAGE :: ThresholdType
pattern ThresholdType_PERCENTAGE = ThresholdType' "PERCENTAGE"

{-# COMPLETE
  ThresholdType_ABSOLUTE_VALUE,
  ThresholdType_PERCENTAGE,
  ThresholdType'
  #-}
