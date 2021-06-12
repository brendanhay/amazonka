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
-- Module      : Network.AWS.CodeBuild.Types.PlatformType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.PlatformType
  ( PlatformType
      ( ..,
        PlatformType_AMAZON_LINUX,
        PlatformType_DEBIAN,
        PlatformType_UBUNTU,
        PlatformType_WINDOWS_SERVER
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype PlatformType = PlatformType'
  { fromPlatformType ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern PlatformType_AMAZON_LINUX :: PlatformType
pattern PlatformType_AMAZON_LINUX = PlatformType' "AMAZON_LINUX"

pattern PlatformType_DEBIAN :: PlatformType
pattern PlatformType_DEBIAN = PlatformType' "DEBIAN"

pattern PlatformType_UBUNTU :: PlatformType
pattern PlatformType_UBUNTU = PlatformType' "UBUNTU"

pattern PlatformType_WINDOWS_SERVER :: PlatformType
pattern PlatformType_WINDOWS_SERVER = PlatformType' "WINDOWS_SERVER"

{-# COMPLETE
  PlatformType_AMAZON_LINUX,
  PlatformType_DEBIAN,
  PlatformType_UBUNTU,
  PlatformType_WINDOWS_SERVER,
  PlatformType'
  #-}
