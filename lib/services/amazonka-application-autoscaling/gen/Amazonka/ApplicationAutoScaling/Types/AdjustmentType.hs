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
-- Module      : Amazonka.ApplicationAutoScaling.Types.AdjustmentType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ApplicationAutoScaling.Types.AdjustmentType
  ( AdjustmentType
      ( ..,
        AdjustmentType_ChangeInCapacity,
        AdjustmentType_ExactCapacity,
        AdjustmentType_PercentChangeInCapacity
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype AdjustmentType = AdjustmentType'
  { fromAdjustmentType ::
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

pattern AdjustmentType_ChangeInCapacity :: AdjustmentType
pattern AdjustmentType_ChangeInCapacity = AdjustmentType' "ChangeInCapacity"

pattern AdjustmentType_ExactCapacity :: AdjustmentType
pattern AdjustmentType_ExactCapacity = AdjustmentType' "ExactCapacity"

pattern AdjustmentType_PercentChangeInCapacity :: AdjustmentType
pattern AdjustmentType_PercentChangeInCapacity = AdjustmentType' "PercentChangeInCapacity"

{-# COMPLETE
  AdjustmentType_ChangeInCapacity,
  AdjustmentType_ExactCapacity,
  AdjustmentType_PercentChangeInCapacity,
  AdjustmentType'
  #-}
