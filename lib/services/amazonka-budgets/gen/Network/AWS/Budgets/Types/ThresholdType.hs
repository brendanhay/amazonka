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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
import qualified Amazonka.Prelude as Prelude

-- | The type of threshold for a notification.
newtype ThresholdType = ThresholdType'
  { fromThresholdType ::
      Core.Text
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
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
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
