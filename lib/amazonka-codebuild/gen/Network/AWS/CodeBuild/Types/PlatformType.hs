-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.PlatformType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.PlatformType
  ( PlatformType
      ( PlatformType',
        AmazonLinux,
        Debian,
        Ubuntu,
        WindowsServer
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype PlatformType = PlatformType' Lude.Text
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

pattern AmazonLinux :: PlatformType
pattern AmazonLinux = PlatformType' "AMAZON_LINUX"

pattern Debian :: PlatformType
pattern Debian = PlatformType' "DEBIAN"

pattern Ubuntu :: PlatformType
pattern Ubuntu = PlatformType' "UBUNTU"

pattern WindowsServer :: PlatformType
pattern WindowsServer = PlatformType' "WINDOWS_SERVER"

{-# COMPLETE
  AmazonLinux,
  Debian,
  Ubuntu,
  WindowsServer,
  PlatformType'
  #-}
