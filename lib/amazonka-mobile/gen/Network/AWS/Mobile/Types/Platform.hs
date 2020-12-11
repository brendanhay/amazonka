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
        Android,
        Javascript,
        Linux,
        OSx,
        Objc,
        Swift,
        Windows
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Developer desktop or target mobile app or website platform.
newtype Platform = Platform' Lude.Text
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

pattern Android :: Platform
pattern Android = Platform' "ANDROID"

pattern Javascript :: Platform
pattern Javascript = Platform' "JAVASCRIPT"

pattern Linux :: Platform
pattern Linux = Platform' "LINUX"

pattern OSx :: Platform
pattern OSx = Platform' "OSX"

pattern Objc :: Platform
pattern Objc = Platform' "OBJC"

pattern Swift :: Platform
pattern Swift = Platform' "SWIFT"

pattern Windows :: Platform
pattern Windows = Platform' "WINDOWS"

{-# COMPLETE
  Android,
  Javascript,
  Linux,
  OSx,
  Objc,
  Swift,
  Windows,
  Platform'
  #-}
