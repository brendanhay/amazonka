{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.UpdateTargetsOperatingSystem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.UpdateTargetsOperatingSystem
  ( UpdateTargetsOperatingSystem
      ( UpdateTargetsOperatingSystem',
        UpdateTargetsOperatingSystemUbuntu,
        UpdateTargetsOperatingSystemRaspbian,
        UpdateTargetsOperatingSystemAmazonLinux,
        UpdateTargetsOperatingSystemOpenwrt,
        fromUpdateTargetsOperatingSystem
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | The operating system of the cores which are the targets of an update.
newtype UpdateTargetsOperatingSystem = UpdateTargetsOperatingSystem'
  { fromUpdateTargetsOperatingSystem ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern UpdateTargetsOperatingSystemUbuntu :: UpdateTargetsOperatingSystem
pattern UpdateTargetsOperatingSystemUbuntu = UpdateTargetsOperatingSystem' "ubuntu"

pattern UpdateTargetsOperatingSystemRaspbian :: UpdateTargetsOperatingSystem
pattern UpdateTargetsOperatingSystemRaspbian = UpdateTargetsOperatingSystem' "raspbian"

pattern UpdateTargetsOperatingSystemAmazonLinux :: UpdateTargetsOperatingSystem
pattern UpdateTargetsOperatingSystemAmazonLinux = UpdateTargetsOperatingSystem' "amazon_linux"

pattern UpdateTargetsOperatingSystemOpenwrt :: UpdateTargetsOperatingSystem
pattern UpdateTargetsOperatingSystemOpenwrt = UpdateTargetsOperatingSystem' "openwrt"

{-# COMPLETE
  UpdateTargetsOperatingSystemUbuntu,
  UpdateTargetsOperatingSystemRaspbian,
  UpdateTargetsOperatingSystemAmazonLinux,
  UpdateTargetsOperatingSystemOpenwrt,
  UpdateTargetsOperatingSystem'
  #-}
