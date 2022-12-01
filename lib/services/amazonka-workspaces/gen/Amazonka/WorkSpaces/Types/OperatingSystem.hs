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
-- Module      : Amazonka.WorkSpaces.Types.OperatingSystem
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpaces.Types.OperatingSystem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkSpaces.Types.OperatingSystemType

-- | The operating system that the image is running.
--
-- /See:/ 'newOperatingSystem' smart constructor.
data OperatingSystem = OperatingSystem'
  { -- | The operating system.
    type' :: Prelude.Maybe OperatingSystemType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OperatingSystem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'operatingSystem_type' - The operating system.
newOperatingSystem ::
  OperatingSystem
newOperatingSystem =
  OperatingSystem' {type' = Prelude.Nothing}

-- | The operating system.
operatingSystem_type :: Lens.Lens' OperatingSystem (Prelude.Maybe OperatingSystemType)
operatingSystem_type = Lens.lens (\OperatingSystem' {type'} -> type') (\s@OperatingSystem' {} a -> s {type' = a} :: OperatingSystem)

instance Core.FromJSON OperatingSystem where
  parseJSON =
    Core.withObject
      "OperatingSystem"
      ( \x ->
          OperatingSystem' Prelude.<$> (x Core..:? "Type")
      )

instance Prelude.Hashable OperatingSystem where
  hashWithSalt _salt OperatingSystem' {..} =
    _salt `Prelude.hashWithSalt` type'

instance Prelude.NFData OperatingSystem where
  rnf OperatingSystem' {..} = Prelude.rnf type'
