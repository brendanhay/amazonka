{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.Feature
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AlexaBusiness.Types.Feature
  ( Feature
    ( Feature'
    , FeatureBluetooth
    , FeatureVolume
    , FeatureNotifications
    , FeatureLists
    , FeatureSkills
    , FeatureNetworkProfile
    , FeatureSettings
    , FeatureAll
    , fromFeature
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype Feature = Feature'{fromFeature :: Core.Text}
                    deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                    Core.Generic)
                    deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                      Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON, Core.FromJSON,
                                      Core.ToXML, Core.FromXML, Core.ToText, Core.FromText,
                                      Core.ToByteString, Core.ToQuery, Core.ToHeader)

pattern FeatureBluetooth :: Feature
pattern FeatureBluetooth = Feature' "BLUETOOTH"

pattern FeatureVolume :: Feature
pattern FeatureVolume = Feature' "VOLUME"

pattern FeatureNotifications :: Feature
pattern FeatureNotifications = Feature' "NOTIFICATIONS"

pattern FeatureLists :: Feature
pattern FeatureLists = Feature' "LISTS"

pattern FeatureSkills :: Feature
pattern FeatureSkills = Feature' "SKILLS"

pattern FeatureNetworkProfile :: Feature
pattern FeatureNetworkProfile = Feature' "NETWORK_PROFILE"

pattern FeatureSettings :: Feature
pattern FeatureSettings = Feature' "SETTINGS"

pattern FeatureAll :: Feature
pattern FeatureAll = Feature' "ALL"

{-# COMPLETE 
  FeatureBluetooth,

  FeatureVolume,

  FeatureNotifications,

  FeatureLists,

  FeatureSkills,

  FeatureNetworkProfile,

  FeatureSettings,

  FeatureAll,
  Feature'
  #-}
