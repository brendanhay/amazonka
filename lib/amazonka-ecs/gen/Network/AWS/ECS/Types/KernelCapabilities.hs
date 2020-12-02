{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.KernelCapabilities
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.KernelCapabilities where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The Linux capabilities for the container that are added to or dropped from the default configuration provided by Docker. For more information on the default capabilities and the non-default available capabilities, see <https://docs.docker.com/engine/reference/run/#runtime-privilege-and-linux-capabilities Runtime privilege and Linux capabilities> in the /Docker run reference/ . For more detailed information on these Linux capabilities, see the <http://man7.org/linux/man-pages/man7/capabilities.7.html capabilities(7)> Linux manual page.
--
--
--
-- /See:/ 'kernelCapabilities' smart constructor.
data KernelCapabilities = KernelCapabilities'
  { _kcDrop ::
      !(Maybe [Text]),
    _kcAdd :: !(Maybe [Text])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'KernelCapabilities' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kcDrop' - The Linux capabilities for the container that have been removed from the default configuration provided by Docker. This parameter maps to @CapDrop@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--cap-drop@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> . Valid values: @"ALL" | "AUDIT_CONTROL" | "AUDIT_WRITE" | "BLOCK_SUSPEND" | "CHOWN" | "DAC_OVERRIDE" | "DAC_READ_SEARCH" | "FOWNER" | "FSETID" | "IPC_LOCK" | "IPC_OWNER" | "KILL" | "LEASE" | "LINUX_IMMUTABLE" | "MAC_ADMIN" | "MAC_OVERRIDE" | "MKNOD" | "NET_ADMIN" | "NET_BIND_SERVICE" | "NET_BROADCAST" | "NET_RAW" | "SETFCAP" | "SETGID" | "SETPCAP" | "SETUID" | "SYS_ADMIN" | "SYS_BOOT" | "SYS_CHROOT" | "SYS_MODULE" | "SYS_NICE" | "SYS_PACCT" | "SYS_PTRACE" | "SYS_RAWIO" | "SYS_RESOURCE" | "SYS_TIME" | "SYS_TTY_CONFIG" | "SYSLOG" | "WAKE_ALARM"@
--
-- * 'kcAdd' - The Linux capabilities for the container that have been added to the default configuration provided by Docker. This parameter maps to @CapAdd@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--cap-add@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> . Valid values: @"ALL" | "AUDIT_CONTROL" | "AUDIT_WRITE" | "BLOCK_SUSPEND" | "CHOWN" | "DAC_OVERRIDE" | "DAC_READ_SEARCH" | "FOWNER" | "FSETID" | "IPC_LOCK" | "IPC_OWNER" | "KILL" | "LEASE" | "LINUX_IMMUTABLE" | "MAC_ADMIN" | "MAC_OVERRIDE" | "MKNOD" | "NET_ADMIN" | "NET_BIND_SERVICE" | "NET_BROADCAST" | "NET_RAW" | "SETFCAP" | "SETGID" | "SETPCAP" | "SETUID" | "SYS_ADMIN" | "SYS_BOOT" | "SYS_CHROOT" | "SYS_MODULE" | "SYS_NICE" | "SYS_PACCT" | "SYS_PTRACE" | "SYS_RAWIO" | "SYS_RESOURCE" | "SYS_TIME" | "SYS_TTY_CONFIG" | "SYSLOG" | "WAKE_ALARM"@
kernelCapabilities ::
  KernelCapabilities
kernelCapabilities =
  KernelCapabilities' {_kcDrop = Nothing, _kcAdd = Nothing}

-- | The Linux capabilities for the container that have been removed from the default configuration provided by Docker. This parameter maps to @CapDrop@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--cap-drop@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> . Valid values: @"ALL" | "AUDIT_CONTROL" | "AUDIT_WRITE" | "BLOCK_SUSPEND" | "CHOWN" | "DAC_OVERRIDE" | "DAC_READ_SEARCH" | "FOWNER" | "FSETID" | "IPC_LOCK" | "IPC_OWNER" | "KILL" | "LEASE" | "LINUX_IMMUTABLE" | "MAC_ADMIN" | "MAC_OVERRIDE" | "MKNOD" | "NET_ADMIN" | "NET_BIND_SERVICE" | "NET_BROADCAST" | "NET_RAW" | "SETFCAP" | "SETGID" | "SETPCAP" | "SETUID" | "SYS_ADMIN" | "SYS_BOOT" | "SYS_CHROOT" | "SYS_MODULE" | "SYS_NICE" | "SYS_PACCT" | "SYS_PTRACE" | "SYS_RAWIO" | "SYS_RESOURCE" | "SYS_TIME" | "SYS_TTY_CONFIG" | "SYSLOG" | "WAKE_ALARM"@
kcDrop :: Lens' KernelCapabilities [Text]
kcDrop = lens _kcDrop (\s a -> s {_kcDrop = a}) . _Default . _Coerce

-- | The Linux capabilities for the container that have been added to the default configuration provided by Docker. This parameter maps to @CapAdd@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--cap-add@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> . Valid values: @"ALL" | "AUDIT_CONTROL" | "AUDIT_WRITE" | "BLOCK_SUSPEND" | "CHOWN" | "DAC_OVERRIDE" | "DAC_READ_SEARCH" | "FOWNER" | "FSETID" | "IPC_LOCK" | "IPC_OWNER" | "KILL" | "LEASE" | "LINUX_IMMUTABLE" | "MAC_ADMIN" | "MAC_OVERRIDE" | "MKNOD" | "NET_ADMIN" | "NET_BIND_SERVICE" | "NET_BROADCAST" | "NET_RAW" | "SETFCAP" | "SETGID" | "SETPCAP" | "SETUID" | "SYS_ADMIN" | "SYS_BOOT" | "SYS_CHROOT" | "SYS_MODULE" | "SYS_NICE" | "SYS_PACCT" | "SYS_PTRACE" | "SYS_RAWIO" | "SYS_RESOURCE" | "SYS_TIME" | "SYS_TTY_CONFIG" | "SYSLOG" | "WAKE_ALARM"@
kcAdd :: Lens' KernelCapabilities [Text]
kcAdd = lens _kcAdd (\s a -> s {_kcAdd = a}) . _Default . _Coerce

instance FromJSON KernelCapabilities where
  parseJSON =
    withObject
      "KernelCapabilities"
      ( \x ->
          KernelCapabilities'
            <$> (x .:? "drop" .!= mempty) <*> (x .:? "add" .!= mempty)
      )

instance Hashable KernelCapabilities

instance NFData KernelCapabilities

instance ToJSON KernelCapabilities where
  toJSON KernelCapabilities' {..} =
    object
      (catMaybes [("drop" .=) <$> _kcDrop, ("add" .=) <$> _kcAdd])
