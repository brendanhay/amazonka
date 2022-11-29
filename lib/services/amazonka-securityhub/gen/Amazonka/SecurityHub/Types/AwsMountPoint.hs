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
-- Module      : Amazonka.SecurityHub.Types.AwsMountPoint
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsMountPoint where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Details for a volume mount point that\'s used in a container definition.
--
-- /See:/ 'newAwsMountPoint' smart constructor.
data AwsMountPoint = AwsMountPoint'
  { -- | The path on the container to mount the host volume at.
    containerPath :: Prelude.Maybe Prelude.Text,
    -- | The name of the volume to mount. Must be a volume name referenced in the
    -- @name@ parameter of task definition @volume@.
    sourceVolume :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsMountPoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containerPath', 'awsMountPoint_containerPath' - The path on the container to mount the host volume at.
--
-- 'sourceVolume', 'awsMountPoint_sourceVolume' - The name of the volume to mount. Must be a volume name referenced in the
-- @name@ parameter of task definition @volume@.
newAwsMountPoint ::
  AwsMountPoint
newAwsMountPoint =
  AwsMountPoint'
    { containerPath = Prelude.Nothing,
      sourceVolume = Prelude.Nothing
    }

-- | The path on the container to mount the host volume at.
awsMountPoint_containerPath :: Lens.Lens' AwsMountPoint (Prelude.Maybe Prelude.Text)
awsMountPoint_containerPath = Lens.lens (\AwsMountPoint' {containerPath} -> containerPath) (\s@AwsMountPoint' {} a -> s {containerPath = a} :: AwsMountPoint)

-- | The name of the volume to mount. Must be a volume name referenced in the
-- @name@ parameter of task definition @volume@.
awsMountPoint_sourceVolume :: Lens.Lens' AwsMountPoint (Prelude.Maybe Prelude.Text)
awsMountPoint_sourceVolume = Lens.lens (\AwsMountPoint' {sourceVolume} -> sourceVolume) (\s@AwsMountPoint' {} a -> s {sourceVolume = a} :: AwsMountPoint)

instance Core.FromJSON AwsMountPoint where
  parseJSON =
    Core.withObject
      "AwsMountPoint"
      ( \x ->
          AwsMountPoint'
            Prelude.<$> (x Core..:? "ContainerPath")
            Prelude.<*> (x Core..:? "SourceVolume")
      )

instance Prelude.Hashable AwsMountPoint where
  hashWithSalt _salt AwsMountPoint' {..} =
    _salt `Prelude.hashWithSalt` containerPath
      `Prelude.hashWithSalt` sourceVolume

instance Prelude.NFData AwsMountPoint where
  rnf AwsMountPoint' {..} =
    Prelude.rnf containerPath
      `Prelude.seq` Prelude.rnf sourceVolume

instance Core.ToJSON AwsMountPoint where
  toJSON AwsMountPoint' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ContainerPath" Core..=) Prelude.<$> containerPath,
            ("SourceVolume" Core..=) Prelude.<$> sourceVolume
          ]
      )
