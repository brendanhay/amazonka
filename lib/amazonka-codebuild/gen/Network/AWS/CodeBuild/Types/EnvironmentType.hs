{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.EnvironmentType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.EnvironmentType
  ( EnvironmentType
      ( EnvironmentType',
        WindowsContainer,
        LinuxContainer,
        LinuxGpuContainer,
        ArmContainer,
        WindowsServer2019Container
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype EnvironmentType = EnvironmentType' Lude.Text
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

pattern WindowsContainer :: EnvironmentType
pattern WindowsContainer = EnvironmentType' "WINDOWS_CONTAINER"

pattern LinuxContainer :: EnvironmentType
pattern LinuxContainer = EnvironmentType' "LINUX_CONTAINER"

pattern LinuxGpuContainer :: EnvironmentType
pattern LinuxGpuContainer = EnvironmentType' "LINUX_GPU_CONTAINER"

pattern ArmContainer :: EnvironmentType
pattern ArmContainer = EnvironmentType' "ARM_CONTAINER"

pattern WindowsServer2019Container :: EnvironmentType
pattern WindowsServer2019Container = EnvironmentType' "WINDOWS_SERVER_2019_CONTAINER"

{-# COMPLETE
  WindowsContainer,
  LinuxContainer,
  LinuxGpuContainer,
  ArmContainer,
  WindowsServer2019Container,
  EnvironmentType'
  #-}
