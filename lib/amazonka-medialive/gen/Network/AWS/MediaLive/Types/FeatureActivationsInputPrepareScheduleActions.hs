{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.FeatureActivationsInputPrepareScheduleActions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.FeatureActivationsInputPrepareScheduleActions where

import Network.AWS.Prelude

-- | Feature Activations Input Prepare Schedule Actions
data FeatureActivationsInputPrepareScheduleActions
  = FAIPSADisabled
  | FAIPSAEnabled
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText FeatureActivationsInputPrepareScheduleActions where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure FAIPSADisabled
      "enabled" -> pure FAIPSAEnabled
      e ->
        fromTextError $
          "Failure parsing FeatureActivationsInputPrepareScheduleActions from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText FeatureActivationsInputPrepareScheduleActions where
  toText = \case
    FAIPSADisabled -> "DISABLED"
    FAIPSAEnabled -> "ENABLED"

instance Hashable FeatureActivationsInputPrepareScheduleActions

instance NFData FeatureActivationsInputPrepareScheduleActions

instance ToByteString FeatureActivationsInputPrepareScheduleActions

instance ToQuery FeatureActivationsInputPrepareScheduleActions

instance ToHeader FeatureActivationsInputPrepareScheduleActions

instance ToJSON FeatureActivationsInputPrepareScheduleActions where
  toJSON = toJSONText

instance FromJSON FeatureActivationsInputPrepareScheduleActions where
  parseJSON = parseJSONText "FeatureActivationsInputPrepareScheduleActions"
