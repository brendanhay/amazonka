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
-- Module      : Amazonka.GuardDuty.Types.Volume
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.Volume where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types.HostPath
import qualified Amazonka.Prelude as Prelude

-- | Volume used by the Kubernetes workload.
--
-- /See:/ 'newVolume' smart constructor.
data Volume = Volume'
  { -- | Represents a pre-existing file or directory on the host machine that the
    -- volume maps to.
    hostPath :: Prelude.Maybe HostPath,
    -- | Volume name.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Volume' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hostPath', 'volume_hostPath' - Represents a pre-existing file or directory on the host machine that the
-- volume maps to.
--
-- 'name', 'volume_name' - Volume name.
newVolume ::
  Volume
newVolume =
  Volume'
    { hostPath = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | Represents a pre-existing file or directory on the host machine that the
-- volume maps to.
volume_hostPath :: Lens.Lens' Volume (Prelude.Maybe HostPath)
volume_hostPath = Lens.lens (\Volume' {hostPath} -> hostPath) (\s@Volume' {} a -> s {hostPath = a} :: Volume)

-- | Volume name.
volume_name :: Lens.Lens' Volume (Prelude.Maybe Prelude.Text)
volume_name = Lens.lens (\Volume' {name} -> name) (\s@Volume' {} a -> s {name = a} :: Volume)

instance Data.FromJSON Volume where
  parseJSON =
    Data.withObject
      "Volume"
      ( \x ->
          Volume'
            Prelude.<$> (x Data..:? "hostPath")
            Prelude.<*> (x Data..:? "name")
      )

instance Prelude.Hashable Volume where
  hashWithSalt _salt Volume' {..} =
    _salt `Prelude.hashWithSalt` hostPath
      `Prelude.hashWithSalt` name

instance Prelude.NFData Volume where
  rnf Volume' {..} =
    Prelude.rnf hostPath `Prelude.seq` Prelude.rnf name
