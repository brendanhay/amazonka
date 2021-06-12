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
-- Module      : Network.AWS.AppStream.Types.PlatformType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.PlatformType
  ( PlatformType
      ( ..,
        PlatformType_WINDOWS,
        PlatformType_WINDOWS_SERVER_2016,
        PlatformType_WINDOWS_SERVER_2019
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

pattern PlatformType_WINDOWS :: PlatformType
pattern PlatformType_WINDOWS = PlatformType' "WINDOWS"

pattern PlatformType_WINDOWS_SERVER_2016 :: PlatformType
pattern PlatformType_WINDOWS_SERVER_2016 = PlatformType' "WINDOWS_SERVER_2016"

pattern PlatformType_WINDOWS_SERVER_2019 :: PlatformType
pattern PlatformType_WINDOWS_SERVER_2019 = PlatformType' "WINDOWS_SERVER_2019"

{-# COMPLETE
  PlatformType_WINDOWS,
  PlatformType_WINDOWS_SERVER_2016,
  PlatformType_WINDOWS_SERVER_2019,
  PlatformType'
  #-}
