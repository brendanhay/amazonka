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
-- Module      : Amazonka.CodeBuild.Types.EnvironmentType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeBuild.Types.EnvironmentType
  ( EnvironmentType
      ( ..,
        EnvironmentType_ARM_CONTAINER,
        EnvironmentType_LINUX_CONTAINER,
        EnvironmentType_LINUX_GPU_CONTAINER,
        EnvironmentType_WINDOWS_CONTAINER,
        EnvironmentType_WINDOWS_SERVER_2019_CONTAINER
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype EnvironmentType = EnvironmentType'
  { fromEnvironmentType ::
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

pattern EnvironmentType_ARM_CONTAINER :: EnvironmentType
pattern EnvironmentType_ARM_CONTAINER = EnvironmentType' "ARM_CONTAINER"

pattern EnvironmentType_LINUX_CONTAINER :: EnvironmentType
pattern EnvironmentType_LINUX_CONTAINER = EnvironmentType' "LINUX_CONTAINER"

pattern EnvironmentType_LINUX_GPU_CONTAINER :: EnvironmentType
pattern EnvironmentType_LINUX_GPU_CONTAINER = EnvironmentType' "LINUX_GPU_CONTAINER"

pattern EnvironmentType_WINDOWS_CONTAINER :: EnvironmentType
pattern EnvironmentType_WINDOWS_CONTAINER = EnvironmentType' "WINDOWS_CONTAINER"

pattern EnvironmentType_WINDOWS_SERVER_2019_CONTAINER :: EnvironmentType
pattern EnvironmentType_WINDOWS_SERVER_2019_CONTAINER = EnvironmentType' "WINDOWS_SERVER_2019_CONTAINER"

{-# COMPLETE
  EnvironmentType_ARM_CONTAINER,
  EnvironmentType_LINUX_CONTAINER,
  EnvironmentType_LINUX_GPU_CONTAINER,
  EnvironmentType_WINDOWS_CONTAINER,
  EnvironmentType_WINDOWS_SERVER_2019_CONTAINER,
  EnvironmentType'
  #-}
