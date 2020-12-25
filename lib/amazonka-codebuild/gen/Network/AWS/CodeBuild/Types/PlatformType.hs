{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        PlatformTypeDebian,
        PlatformTypeAmazonLinux,
        PlatformTypeUbuntu,
        PlatformTypeWindowsServer,
        fromPlatformType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype PlatformType = PlatformType' {fromPlatformType :: Core.Text}
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

pattern PlatformTypeDebian :: PlatformType
pattern PlatformTypeDebian = PlatformType' "DEBIAN"

pattern PlatformTypeAmazonLinux :: PlatformType
pattern PlatformTypeAmazonLinux = PlatformType' "AMAZON_LINUX"

pattern PlatformTypeUbuntu :: PlatformType
pattern PlatformTypeUbuntu = PlatformType' "UBUNTU"

pattern PlatformTypeWindowsServer :: PlatformType
pattern PlatformTypeWindowsServer = PlatformType' "WINDOWS_SERVER"

{-# COMPLETE
  PlatformTypeDebian,
  PlatformTypeAmazonLinux,
  PlatformTypeUbuntu,
  PlatformTypeWindowsServer,
  PlatformType'
  #-}
