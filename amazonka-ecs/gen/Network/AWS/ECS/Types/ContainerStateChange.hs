{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.ECS.Types.ContainerStateChange
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.ContainerStateChange where

import Network.AWS.ECS.Types.NetworkBinding
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object representing a change in state for a container.
--
-- /See:/ 'newContainerStateChange' smart constructor.
data ContainerStateChange = ContainerStateChange'
  { -- | The container image SHA 256 digest.
    imageDigest :: Prelude.Maybe Prelude.Text,
    -- | The status of the container.
    status :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Docker container.
    runtimeId :: Prelude.Maybe Prelude.Text,
    -- | The exit code for the container, if the state change is a result of the
    -- container exiting.
    exitCode :: Prelude.Maybe Prelude.Int,
    -- | Any network bindings associated with the container.
    networkBindings :: Prelude.Maybe [NetworkBinding],
    -- | The reason for the state change.
    reason :: Prelude.Maybe Prelude.Text,
    -- | The name of the container.
    containerName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ContainerStateChange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageDigest', 'containerStateChange_imageDigest' - The container image SHA 256 digest.
--
-- 'status', 'containerStateChange_status' - The status of the container.
--
-- 'runtimeId', 'containerStateChange_runtimeId' - The ID of the Docker container.
--
-- 'exitCode', 'containerStateChange_exitCode' - The exit code for the container, if the state change is a result of the
-- container exiting.
--
-- 'networkBindings', 'containerStateChange_networkBindings' - Any network bindings associated with the container.
--
-- 'reason', 'containerStateChange_reason' - The reason for the state change.
--
-- 'containerName', 'containerStateChange_containerName' - The name of the container.
newContainerStateChange ::
  ContainerStateChange
newContainerStateChange =
  ContainerStateChange'
    { imageDigest =
        Prelude.Nothing,
      status = Prelude.Nothing,
      runtimeId = Prelude.Nothing,
      exitCode = Prelude.Nothing,
      networkBindings = Prelude.Nothing,
      reason = Prelude.Nothing,
      containerName = Prelude.Nothing
    }

-- | The container image SHA 256 digest.
containerStateChange_imageDigest :: Lens.Lens' ContainerStateChange (Prelude.Maybe Prelude.Text)
containerStateChange_imageDigest = Lens.lens (\ContainerStateChange' {imageDigest} -> imageDigest) (\s@ContainerStateChange' {} a -> s {imageDigest = a} :: ContainerStateChange)

-- | The status of the container.
containerStateChange_status :: Lens.Lens' ContainerStateChange (Prelude.Maybe Prelude.Text)
containerStateChange_status = Lens.lens (\ContainerStateChange' {status} -> status) (\s@ContainerStateChange' {} a -> s {status = a} :: ContainerStateChange)

-- | The ID of the Docker container.
containerStateChange_runtimeId :: Lens.Lens' ContainerStateChange (Prelude.Maybe Prelude.Text)
containerStateChange_runtimeId = Lens.lens (\ContainerStateChange' {runtimeId} -> runtimeId) (\s@ContainerStateChange' {} a -> s {runtimeId = a} :: ContainerStateChange)

-- | The exit code for the container, if the state change is a result of the
-- container exiting.
containerStateChange_exitCode :: Lens.Lens' ContainerStateChange (Prelude.Maybe Prelude.Int)
containerStateChange_exitCode = Lens.lens (\ContainerStateChange' {exitCode} -> exitCode) (\s@ContainerStateChange' {} a -> s {exitCode = a} :: ContainerStateChange)

-- | Any network bindings associated with the container.
containerStateChange_networkBindings :: Lens.Lens' ContainerStateChange (Prelude.Maybe [NetworkBinding])
containerStateChange_networkBindings = Lens.lens (\ContainerStateChange' {networkBindings} -> networkBindings) (\s@ContainerStateChange' {} a -> s {networkBindings = a} :: ContainerStateChange) Prelude.. Lens.mapping Prelude._Coerce

-- | The reason for the state change.
containerStateChange_reason :: Lens.Lens' ContainerStateChange (Prelude.Maybe Prelude.Text)
containerStateChange_reason = Lens.lens (\ContainerStateChange' {reason} -> reason) (\s@ContainerStateChange' {} a -> s {reason = a} :: ContainerStateChange)

-- | The name of the container.
containerStateChange_containerName :: Lens.Lens' ContainerStateChange (Prelude.Maybe Prelude.Text)
containerStateChange_containerName = Lens.lens (\ContainerStateChange' {containerName} -> containerName) (\s@ContainerStateChange' {} a -> s {containerName = a} :: ContainerStateChange)

instance Prelude.Hashable ContainerStateChange

instance Prelude.NFData ContainerStateChange

instance Prelude.ToJSON ContainerStateChange where
  toJSON ContainerStateChange' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("imageDigest" Prelude..=) Prelude.<$> imageDigest,
            ("status" Prelude..=) Prelude.<$> status,
            ("runtimeId" Prelude..=) Prelude.<$> runtimeId,
            ("exitCode" Prelude..=) Prelude.<$> exitCode,
            ("networkBindings" Prelude..=)
              Prelude.<$> networkBindings,
            ("reason" Prelude..=) Prelude.<$> reason,
            ("containerName" Prelude..=)
              Prelude.<$> containerName
          ]
      )
