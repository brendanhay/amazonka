-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.InstancePlatform
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.InstancePlatform
  ( InstancePlatform
      ( InstancePlatform',
        LinuxUnix,
        Windows
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype InstancePlatform = InstancePlatform' Lude.Text
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

pattern LinuxUnix :: InstancePlatform
pattern LinuxUnix = InstancePlatform' "LINUX_UNIX"

pattern Windows :: InstancePlatform
pattern Windows = InstancePlatform' "WINDOWS"

{-# COMPLETE
  LinuxUnix,
  Windows,
  InstancePlatform'
  #-}
