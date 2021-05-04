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

import qualified Network.AWS.Prelude as Prelude

newtype PlatformType = PlatformType'
  { fromPlatformType ::
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
