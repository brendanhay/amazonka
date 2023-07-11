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
-- Module      : Amazonka.AppStream.Types.PlatformType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppStream.Types.PlatformType
  ( PlatformType
      ( ..,
        PlatformType_AMAZON_LINUX2,
        PlatformType_WINDOWS,
        PlatformType_WINDOWS_SERVER_2016,
        PlatformType_WINDOWS_SERVER_2019
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PlatformType = PlatformType'
  { fromPlatformType ::
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

pattern PlatformType_AMAZON_LINUX2 :: PlatformType
pattern PlatformType_AMAZON_LINUX2 = PlatformType' "AMAZON_LINUX2"

pattern PlatformType_WINDOWS :: PlatformType
pattern PlatformType_WINDOWS = PlatformType' "WINDOWS"

pattern PlatformType_WINDOWS_SERVER_2016 :: PlatformType
pattern PlatformType_WINDOWS_SERVER_2016 = PlatformType' "WINDOWS_SERVER_2016"

pattern PlatformType_WINDOWS_SERVER_2019 :: PlatformType
pattern PlatformType_WINDOWS_SERVER_2019 = PlatformType' "WINDOWS_SERVER_2019"

{-# COMPLETE
  PlatformType_AMAZON_LINUX2,
  PlatformType_WINDOWS,
  PlatformType_WINDOWS_SERVER_2016,
  PlatformType_WINDOWS_SERVER_2019,
  PlatformType'
  #-}
