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
-- Module      : Network.AWS.GameLift.Types.ScalingAdjustmentType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.ScalingAdjustmentType
  ( ScalingAdjustmentType
      ( ..,
        ScalingAdjustmentType_ChangeInCapacity,
        ScalingAdjustmentType_ExactCapacity,
        ScalingAdjustmentType_PercentChangeInCapacity
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ScalingAdjustmentType = ScalingAdjustmentType'
  { fromScalingAdjustmentType ::
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

pattern ScalingAdjustmentType_ChangeInCapacity :: ScalingAdjustmentType
pattern ScalingAdjustmentType_ChangeInCapacity = ScalingAdjustmentType' "ChangeInCapacity"

pattern ScalingAdjustmentType_ExactCapacity :: ScalingAdjustmentType
pattern ScalingAdjustmentType_ExactCapacity = ScalingAdjustmentType' "ExactCapacity"

pattern ScalingAdjustmentType_PercentChangeInCapacity :: ScalingAdjustmentType
pattern ScalingAdjustmentType_PercentChangeInCapacity = ScalingAdjustmentType' "PercentChangeInCapacity"

{-# COMPLETE
  ScalingAdjustmentType_ChangeInCapacity,
  ScalingAdjustmentType_ExactCapacity,
  ScalingAdjustmentType_PercentChangeInCapacity,
  ScalingAdjustmentType'
  #-}
