{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApplicationAutoScaling.Types.AdjustmentType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ApplicationAutoScaling.Types.AdjustmentType
  ( AdjustmentType
      ( ..,
        AdjustmentType_ChangeInCapacity,
        AdjustmentType_ExactCapacity,
        AdjustmentType_PercentChangeInCapacity
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype AdjustmentType = AdjustmentType'
  { fromAdjustmentType ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
