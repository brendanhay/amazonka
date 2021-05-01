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
-- Module      : Network.AWS.CodeBuild.Types.EnvironmentType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.EnvironmentType
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

import qualified Network.AWS.Prelude as Prelude

newtype EnvironmentType = EnvironmentType'
  { fromEnvironmentType ::
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
