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
-- Module      : Amazonka.GuardDuty.Types.HostPath
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.HostPath where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents a pre-existing file or directory on the host machine that the
-- volume maps to.
--
-- /See:/ 'newHostPath' smart constructor.
data HostPath = HostPath'
  { -- | Path of the file or directory on the host that the volume maps to.
    path :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HostPath' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'path', 'hostPath_path' - Path of the file or directory on the host that the volume maps to.
newHostPath ::
  HostPath
newHostPath = HostPath' {path = Prelude.Nothing}

-- | Path of the file or directory on the host that the volume maps to.
hostPath_path :: Lens.Lens' HostPath (Prelude.Maybe Prelude.Text)
hostPath_path = Lens.lens (\HostPath' {path} -> path) (\s@HostPath' {} a -> s {path = a} :: HostPath)

instance Data.FromJSON HostPath where
  parseJSON =
    Data.withObject
      "HostPath"
      (\x -> HostPath' Prelude.<$> (x Data..:? "path"))

instance Prelude.Hashable HostPath where
  hashWithSalt _salt HostPath' {..} =
    _salt `Prelude.hashWithSalt` path

instance Prelude.NFData HostPath where
  rnf HostPath' {..} = Prelude.rnf path
