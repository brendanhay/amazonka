{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputDeviceIPScheme
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputDeviceIPScheme
  ( InputDeviceIPScheme
      ( InputDeviceIPScheme',
        DHCP,
        Static
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Specifies whether the input device has been configured (outside of MediaLive) to use a dynamic IP address assignment (DHCP) or a static IP address.
newtype InputDeviceIPScheme = InputDeviceIPScheme' Lude.Text
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

pattern DHCP :: InputDeviceIPScheme
pattern DHCP = InputDeviceIPScheme' "DHCP"

pattern Static :: InputDeviceIPScheme
pattern Static = InputDeviceIPScheme' "STATIC"

{-# COMPLETE
  DHCP,
  Static,
  InputDeviceIPScheme'
  #-}
