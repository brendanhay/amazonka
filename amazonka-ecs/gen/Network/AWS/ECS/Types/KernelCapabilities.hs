{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.KernelCapabilities
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.KernelCapabilities where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The Linux capabilities for the container that are added to or dropped
-- from the default configuration provided by Docker. For more information
-- on the default capabilities and the non-default available capabilities,
-- see
-- <https://docs.docker.com/engine/reference/run/#runtime-privilege-and-linux-capabilities Runtime privilege and Linux capabilities>
-- in the /Docker run reference/. For more detailed information on these
-- Linux capabilities, see the
-- <http://man7.org/linux/man-pages/man7/capabilities.7.html capabilities(7)>
-- Linux manual page.
--
-- /See:/ 'newKernelCapabilities' smart constructor.
data KernelCapabilities = KernelCapabilities'
  { -- | The Linux capabilities for the container that have been removed from the
    -- default configuration provided by Docker. This parameter maps to
    -- @CapDrop@ in the
    -- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
    -- section of the
    -- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
    -- @--cap-drop@ option to
    -- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
    --
    -- Valid values:
    -- @\"ALL\" | \"AUDIT_CONTROL\" | \"AUDIT_WRITE\" | \"BLOCK_SUSPEND\" | \"CHOWN\" | \"DAC_OVERRIDE\" | \"DAC_READ_SEARCH\" | \"FOWNER\" | \"FSETID\" | \"IPC_LOCK\" | \"IPC_OWNER\" | \"KILL\" | \"LEASE\" | \"LINUX_IMMUTABLE\" | \"MAC_ADMIN\" | \"MAC_OVERRIDE\" | \"MKNOD\" | \"NET_ADMIN\" | \"NET_BIND_SERVICE\" | \"NET_BROADCAST\" | \"NET_RAW\" | \"SETFCAP\" | \"SETGID\" | \"SETPCAP\" | \"SETUID\" | \"SYS_ADMIN\" | \"SYS_BOOT\" | \"SYS_CHROOT\" | \"SYS_MODULE\" | \"SYS_NICE\" | \"SYS_PACCT\" | \"SYS_PTRACE\" | \"SYS_RAWIO\" | \"SYS_RESOURCE\" | \"SYS_TIME\" | \"SYS_TTY_CONFIG\" | \"SYSLOG\" | \"WAKE_ALARM\"@
    drop :: Core.Maybe [Core.Text],
    -- | The Linux capabilities for the container that have been added to the
    -- default configuration provided by Docker. This parameter maps to
    -- @CapAdd@ in the
    -- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
    -- section of the
    -- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
    -- @--cap-add@ option to
    -- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
    --
    -- Tasks launched on AWS Fargate only support adding the @SYS_PTRACE@
    -- kernel capability.
    --
    -- Valid values:
    -- @\"ALL\" | \"AUDIT_CONTROL\" | \"AUDIT_WRITE\" | \"BLOCK_SUSPEND\" | \"CHOWN\" | \"DAC_OVERRIDE\" | \"DAC_READ_SEARCH\" | \"FOWNER\" | \"FSETID\" | \"IPC_LOCK\" | \"IPC_OWNER\" | \"KILL\" | \"LEASE\" | \"LINUX_IMMUTABLE\" | \"MAC_ADMIN\" | \"MAC_OVERRIDE\" | \"MKNOD\" | \"NET_ADMIN\" | \"NET_BIND_SERVICE\" | \"NET_BROADCAST\" | \"NET_RAW\" | \"SETFCAP\" | \"SETGID\" | \"SETPCAP\" | \"SETUID\" | \"SYS_ADMIN\" | \"SYS_BOOT\" | \"SYS_CHROOT\" | \"SYS_MODULE\" | \"SYS_NICE\" | \"SYS_PACCT\" | \"SYS_PTRACE\" | \"SYS_RAWIO\" | \"SYS_RESOURCE\" | \"SYS_TIME\" | \"SYS_TTY_CONFIG\" | \"SYSLOG\" | \"WAKE_ALARM\"@
    add :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'KernelCapabilities' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'drop', 'kernelCapabilities_drop' - The Linux capabilities for the container that have been removed from the
-- default configuration provided by Docker. This parameter maps to
-- @CapDrop@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @--cap-drop@ option to
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
--
-- Valid values:
-- @\"ALL\" | \"AUDIT_CONTROL\" | \"AUDIT_WRITE\" | \"BLOCK_SUSPEND\" | \"CHOWN\" | \"DAC_OVERRIDE\" | \"DAC_READ_SEARCH\" | \"FOWNER\" | \"FSETID\" | \"IPC_LOCK\" | \"IPC_OWNER\" | \"KILL\" | \"LEASE\" | \"LINUX_IMMUTABLE\" | \"MAC_ADMIN\" | \"MAC_OVERRIDE\" | \"MKNOD\" | \"NET_ADMIN\" | \"NET_BIND_SERVICE\" | \"NET_BROADCAST\" | \"NET_RAW\" | \"SETFCAP\" | \"SETGID\" | \"SETPCAP\" | \"SETUID\" | \"SYS_ADMIN\" | \"SYS_BOOT\" | \"SYS_CHROOT\" | \"SYS_MODULE\" | \"SYS_NICE\" | \"SYS_PACCT\" | \"SYS_PTRACE\" | \"SYS_RAWIO\" | \"SYS_RESOURCE\" | \"SYS_TIME\" | \"SYS_TTY_CONFIG\" | \"SYSLOG\" | \"WAKE_ALARM\"@
--
-- 'add', 'kernelCapabilities_add' - The Linux capabilities for the container that have been added to the
-- default configuration provided by Docker. This parameter maps to
-- @CapAdd@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @--cap-add@ option to
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
--
-- Tasks launched on AWS Fargate only support adding the @SYS_PTRACE@
-- kernel capability.
--
-- Valid values:
-- @\"ALL\" | \"AUDIT_CONTROL\" | \"AUDIT_WRITE\" | \"BLOCK_SUSPEND\" | \"CHOWN\" | \"DAC_OVERRIDE\" | \"DAC_READ_SEARCH\" | \"FOWNER\" | \"FSETID\" | \"IPC_LOCK\" | \"IPC_OWNER\" | \"KILL\" | \"LEASE\" | \"LINUX_IMMUTABLE\" | \"MAC_ADMIN\" | \"MAC_OVERRIDE\" | \"MKNOD\" | \"NET_ADMIN\" | \"NET_BIND_SERVICE\" | \"NET_BROADCAST\" | \"NET_RAW\" | \"SETFCAP\" | \"SETGID\" | \"SETPCAP\" | \"SETUID\" | \"SYS_ADMIN\" | \"SYS_BOOT\" | \"SYS_CHROOT\" | \"SYS_MODULE\" | \"SYS_NICE\" | \"SYS_PACCT\" | \"SYS_PTRACE\" | \"SYS_RAWIO\" | \"SYS_RESOURCE\" | \"SYS_TIME\" | \"SYS_TTY_CONFIG\" | \"SYSLOG\" | \"WAKE_ALARM\"@
newKernelCapabilities ::
  KernelCapabilities
newKernelCapabilities =
  KernelCapabilities'
    { drop = Core.Nothing,
      add = Core.Nothing
    }

-- | The Linux capabilities for the container that have been removed from the
-- default configuration provided by Docker. This parameter maps to
-- @CapDrop@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @--cap-drop@ option to
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
--
-- Valid values:
-- @\"ALL\" | \"AUDIT_CONTROL\" | \"AUDIT_WRITE\" | \"BLOCK_SUSPEND\" | \"CHOWN\" | \"DAC_OVERRIDE\" | \"DAC_READ_SEARCH\" | \"FOWNER\" | \"FSETID\" | \"IPC_LOCK\" | \"IPC_OWNER\" | \"KILL\" | \"LEASE\" | \"LINUX_IMMUTABLE\" | \"MAC_ADMIN\" | \"MAC_OVERRIDE\" | \"MKNOD\" | \"NET_ADMIN\" | \"NET_BIND_SERVICE\" | \"NET_BROADCAST\" | \"NET_RAW\" | \"SETFCAP\" | \"SETGID\" | \"SETPCAP\" | \"SETUID\" | \"SYS_ADMIN\" | \"SYS_BOOT\" | \"SYS_CHROOT\" | \"SYS_MODULE\" | \"SYS_NICE\" | \"SYS_PACCT\" | \"SYS_PTRACE\" | \"SYS_RAWIO\" | \"SYS_RESOURCE\" | \"SYS_TIME\" | \"SYS_TTY_CONFIG\" | \"SYSLOG\" | \"WAKE_ALARM\"@
kernelCapabilities_drop :: Lens.Lens' KernelCapabilities (Core.Maybe [Core.Text])
kernelCapabilities_drop = Lens.lens (\KernelCapabilities' {drop} -> drop) (\s@KernelCapabilities' {} a -> s {drop = a} :: KernelCapabilities) Core.. Lens.mapping Lens._Coerce

-- | The Linux capabilities for the container that have been added to the
-- default configuration provided by Docker. This parameter maps to
-- @CapAdd@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @--cap-add@ option to
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
--
-- Tasks launched on AWS Fargate only support adding the @SYS_PTRACE@
-- kernel capability.
--
-- Valid values:
-- @\"ALL\" | \"AUDIT_CONTROL\" | \"AUDIT_WRITE\" | \"BLOCK_SUSPEND\" | \"CHOWN\" | \"DAC_OVERRIDE\" | \"DAC_READ_SEARCH\" | \"FOWNER\" | \"FSETID\" | \"IPC_LOCK\" | \"IPC_OWNER\" | \"KILL\" | \"LEASE\" | \"LINUX_IMMUTABLE\" | \"MAC_ADMIN\" | \"MAC_OVERRIDE\" | \"MKNOD\" | \"NET_ADMIN\" | \"NET_BIND_SERVICE\" | \"NET_BROADCAST\" | \"NET_RAW\" | \"SETFCAP\" | \"SETGID\" | \"SETPCAP\" | \"SETUID\" | \"SYS_ADMIN\" | \"SYS_BOOT\" | \"SYS_CHROOT\" | \"SYS_MODULE\" | \"SYS_NICE\" | \"SYS_PACCT\" | \"SYS_PTRACE\" | \"SYS_RAWIO\" | \"SYS_RESOURCE\" | \"SYS_TIME\" | \"SYS_TTY_CONFIG\" | \"SYSLOG\" | \"WAKE_ALARM\"@
kernelCapabilities_add :: Lens.Lens' KernelCapabilities (Core.Maybe [Core.Text])
kernelCapabilities_add = Lens.lens (\KernelCapabilities' {add} -> add) (\s@KernelCapabilities' {} a -> s {add = a} :: KernelCapabilities) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON KernelCapabilities where
  parseJSON =
    Core.withObject
      "KernelCapabilities"
      ( \x ->
          KernelCapabilities'
            Core.<$> (x Core..:? "drop" Core..!= Core.mempty)
            Core.<*> (x Core..:? "add" Core..!= Core.mempty)
      )

instance Core.Hashable KernelCapabilities

instance Core.NFData KernelCapabilities

instance Core.ToJSON KernelCapabilities where
  toJSON KernelCapabilities' {..} =
    Core.object
      ( Core.catMaybes
          [ ("drop" Core..=) Core.<$> drop,
            ("add" Core..=) Core.<$> add
          ]
      )
