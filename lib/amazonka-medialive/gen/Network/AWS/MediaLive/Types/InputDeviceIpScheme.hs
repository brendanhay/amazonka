{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputDeviceIpScheme
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.InputDeviceIpScheme
  ( InputDeviceIpScheme
    ( InputDeviceIpScheme'
    , InputDeviceIpSchemeStatic
    , InputDeviceIpSchemeDhcp
    , fromInputDeviceIpScheme
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Specifies whether the input device has been configured (outside of MediaLive) to use a dynamic IP address assignment (DHCP) or a static IP address.
newtype InputDeviceIpScheme = InputDeviceIpScheme'{fromInputDeviceIpScheme
                                                   :: Core.Text}
                                deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                Core.Generic)
                                deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                  Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                  Core.FromJSON, Core.ToXML, Core.FromXML,
                                                  Core.ToText, Core.FromText, Core.ToByteString,
                                                  Core.ToQuery, Core.ToHeader)

pattern InputDeviceIpSchemeStatic :: InputDeviceIpScheme
pattern InputDeviceIpSchemeStatic = InputDeviceIpScheme' "STATIC"

pattern InputDeviceIpSchemeDhcp :: InputDeviceIpScheme
pattern InputDeviceIpSchemeDhcp = InputDeviceIpScheme' "DHCP"

{-# COMPLETE 
  InputDeviceIpSchemeStatic,

  InputDeviceIpSchemeDhcp,
  InputDeviceIpScheme'
  #-}
