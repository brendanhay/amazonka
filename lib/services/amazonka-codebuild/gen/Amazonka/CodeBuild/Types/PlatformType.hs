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
-- Module      : Amazonka.CodeBuild.Types.PlatformType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeBuild.Types.PlatformType
  ( PlatformType
      ( ..,
        PlatformType_AMAZON_LINUX,
        PlatformType_DEBIAN,
        PlatformType_UBUNTU,
        PlatformType_WINDOWS_SERVER
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
