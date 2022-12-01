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
-- Module      : Amazonka.AppRunner.Types.InstanceConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppRunner.Types.InstanceConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the runtime configuration of an App Runner service instance
-- (scaling unit).
--
-- /See:/ 'newInstanceConfiguration' smart constructor.
data InstanceConfiguration = InstanceConfiguration'
  { -- | The number of CPU units reserved for each instance of your App Runner
    -- service.
    --
    -- Default: @1 vCPU@
    cpu :: Prelude.Maybe Prelude.Text,
    -- | The amount of memory, in MB or GB, reserved for each instance of your
    -- App Runner service.
    --
    -- Default: @2 GB@
    memory :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of an IAM role that provides permissions
    -- to your App Runner service. These are permissions that your code needs
    -- when it calls any Amazon Web Services APIs.
    instanceRoleArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cpu', 'instanceConfiguration_cpu' - The number of CPU units reserved for each instance of your App Runner
-- service.
--
-- Default: @1 vCPU@
--
-- 'memory', 'instanceConfiguration_memory' - The amount of memory, in MB or GB, reserved for each instance of your
-- App Runner service.
--
-- Default: @2 GB@
--
-- 'instanceRoleArn', 'instanceConfiguration_instanceRoleArn' - The Amazon Resource Name (ARN) of an IAM role that provides permissions
-- to your App Runner service. These are permissions that your code needs
-- when it calls any Amazon Web Services APIs.
newInstanceConfiguration ::
  InstanceConfiguration
newInstanceConfiguration =
  InstanceConfiguration'
    { cpu = Prelude.Nothing,
      memory = Prelude.Nothing,
      instanceRoleArn = Prelude.Nothing
    }

-- | The number of CPU units reserved for each instance of your App Runner
-- service.
--
-- Default: @1 vCPU@
instanceConfiguration_cpu :: Lens.Lens' InstanceConfiguration (Prelude.Maybe Prelude.Text)
instanceConfiguration_cpu = Lens.lens (\InstanceConfiguration' {cpu} -> cpu) (\s@InstanceConfiguration' {} a -> s {cpu = a} :: InstanceConfiguration)

-- | The amount of memory, in MB or GB, reserved for each instance of your
-- App Runner service.
--
-- Default: @2 GB@
instanceConfiguration_memory :: Lens.Lens' InstanceConfiguration (Prelude.Maybe Prelude.Text)
instanceConfiguration_memory = Lens.lens (\InstanceConfiguration' {memory} -> memory) (\s@InstanceConfiguration' {} a -> s {memory = a} :: InstanceConfiguration)

-- | The Amazon Resource Name (ARN) of an IAM role that provides permissions
-- to your App Runner service. These are permissions that your code needs
-- when it calls any Amazon Web Services APIs.
instanceConfiguration_instanceRoleArn :: Lens.Lens' InstanceConfiguration (Prelude.Maybe Prelude.Text)
instanceConfiguration_instanceRoleArn = Lens.lens (\InstanceConfiguration' {instanceRoleArn} -> instanceRoleArn) (\s@InstanceConfiguration' {} a -> s {instanceRoleArn = a} :: InstanceConfiguration)

instance Core.FromJSON InstanceConfiguration where
  parseJSON =
    Core.withObject
      "InstanceConfiguration"
      ( \x ->
          InstanceConfiguration'
            Prelude.<$> (x Core..:? "Cpu")
            Prelude.<*> (x Core..:? "Memory")
            Prelude.<*> (x Core..:? "InstanceRoleArn")
      )

instance Prelude.Hashable InstanceConfiguration where
  hashWithSalt _salt InstanceConfiguration' {..} =
    _salt `Prelude.hashWithSalt` cpu
      `Prelude.hashWithSalt` memory
      `Prelude.hashWithSalt` instanceRoleArn

instance Prelude.NFData InstanceConfiguration where
  rnf InstanceConfiguration' {..} =
    Prelude.rnf cpu
      `Prelude.seq` Prelude.rnf memory
      `Prelude.seq` Prelude.rnf instanceRoleArn

instance Core.ToJSON InstanceConfiguration where
  toJSON InstanceConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Cpu" Core..=) Prelude.<$> cpu,
            ("Memory" Core..=) Prelude.<$> memory,
            ("InstanceRoleArn" Core..=)
              Prelude.<$> instanceRoleArn
          ]
      )
