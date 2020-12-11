-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.Feature
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.Feature
  ( Feature
      ( Feature',
        All,
        Bluetooth,
        Lists,
        NetworkProfile,
        Notifications,
        Settings,
        Skills,
        Volume
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype Feature = Feature' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern All :: Feature
pattern All = Feature' "ALL"

pattern Bluetooth :: Feature
pattern Bluetooth = Feature' "BLUETOOTH"

pattern Lists :: Feature
pattern Lists = Feature' "LISTS"

pattern NetworkProfile :: Feature
pattern NetworkProfile = Feature' "NETWORK_PROFILE"

pattern Notifications :: Feature
pattern Notifications = Feature' "NOTIFICATIONS"

pattern Settings :: Feature
pattern Settings = Feature' "SETTINGS"

pattern Skills :: Feature
pattern Skills = Feature' "SKILLS"

pattern Volume :: Feature
pattern Volume = Feature' "VOLUME"

{-# COMPLETE
  All,
  Bluetooth,
  Lists,
  NetworkProfile,
  Notifications,
  Settings,
  Skills,
  Volume,
  Feature'
  #-}
