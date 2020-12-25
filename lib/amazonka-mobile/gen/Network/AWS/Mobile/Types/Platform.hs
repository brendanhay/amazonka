{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Mobile.Types.Platform
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Mobile.Types.Platform
  ( Platform
      ( Platform',
        PlatformOsx,
        PlatformWindows,
        PlatformLinux,
        PlatformObjc,
        PlatformSwift,
        PlatformAndroid,
        PlatformJavascript,
        fromPlatform
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Developer desktop or target mobile app or website platform.
newtype Platform = Platform' {fromPlatform :: Core.Text}
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

pattern PlatformOsx :: Platform
pattern PlatformOsx = Platform' "OSX"

pattern PlatformWindows :: Platform
pattern PlatformWindows = Platform' "WINDOWS"

pattern PlatformLinux :: Platform
pattern PlatformLinux = Platform' "LINUX"

pattern PlatformObjc :: Platform
pattern PlatformObjc = Platform' "OBJC"

pattern PlatformSwift :: Platform
pattern PlatformSwift = Platform' "SWIFT"

pattern PlatformAndroid :: Platform
pattern PlatformAndroid = Platform' "ANDROID"

pattern PlatformJavascript :: Platform
pattern PlatformJavascript = Platform' "JAVASCRIPT"

{-# COMPLETE
  PlatformOsx,
  PlatformWindows,
  PlatformLinux,
  PlatformObjc,
  PlatformSwift,
  PlatformAndroid,
  PlatformJavascript,
  Platform'
  #-}
