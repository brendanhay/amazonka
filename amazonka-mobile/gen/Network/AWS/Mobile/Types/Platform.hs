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
-- Module      : Network.AWS.Mobile.Types.Platform
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Mobile.Types.Platform
  ( Platform
      ( ..,
        Platform_ANDROID,
        Platform_JAVASCRIPT,
        Platform_LINUX,
        Platform_OBJC,
        Platform_OSX,
        Platform_SWIFT,
        Platform_WINDOWS
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Developer desktop or target mobile app or website platform.
newtype Platform = Platform'
  { fromPlatform ::
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

pattern Platform_ANDROID :: Platform
pattern Platform_ANDROID = Platform' "ANDROID"

pattern Platform_JAVASCRIPT :: Platform
pattern Platform_JAVASCRIPT = Platform' "JAVASCRIPT"

pattern Platform_LINUX :: Platform
pattern Platform_LINUX = Platform' "LINUX"

pattern Platform_OBJC :: Platform
pattern Platform_OBJC = Platform' "OBJC"

pattern Platform_OSX :: Platform
pattern Platform_OSX = Platform' "OSX"

pattern Platform_SWIFT :: Platform
pattern Platform_SWIFT = Platform' "SWIFT"

pattern Platform_WINDOWS :: Platform
pattern Platform_WINDOWS = Platform' "WINDOWS"

{-# COMPLETE
  Platform_ANDROID,
  Platform_JAVASCRIPT,
  Platform_LINUX,
  Platform_OBJC,
  Platform_OSX,
  Platform_SWIFT,
  Platform_WINDOWS,
  Platform'
  #-}
