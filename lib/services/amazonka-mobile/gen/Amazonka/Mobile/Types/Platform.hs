{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Mobile.Types.Platform
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Mobile.Types.Platform
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Developer desktop or target mobile app or website platform.
newtype Platform = Platform'
  { fromPlatform ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
