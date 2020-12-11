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
        AmazonLinux,
        Openwrt,
        Raspbian,
        Ubuntu
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | The operating system of the cores which are the targets of an update.
newtype UpdateTargetsOperatingSystem = UpdateTargetsOperatingSystem' Lude.Text
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

pattern AmazonLinux :: UpdateTargetsOperatingSystem
pattern AmazonLinux = UpdateTargetsOperatingSystem' "amazon_linux"

pattern Openwrt :: UpdateTargetsOperatingSystem
pattern Openwrt = UpdateTargetsOperatingSystem' "openwrt"

pattern Raspbian :: UpdateTargetsOperatingSystem
pattern Raspbian = UpdateTargetsOperatingSystem' "raspbian"

pattern Ubuntu :: UpdateTargetsOperatingSystem
pattern Ubuntu = UpdateTargetsOperatingSystem' "ubuntu"

{-# COMPLETE
  AmazonLinux,
  Openwrt,
  Raspbian,
  Ubuntu,
  UpdateTargetsOperatingSystem'
  #-}
