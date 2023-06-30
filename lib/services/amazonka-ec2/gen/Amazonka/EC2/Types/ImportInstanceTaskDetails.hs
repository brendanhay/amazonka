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
-- Module      : Amazonka.EC2.Types.ImportInstanceTaskDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ImportInstanceTaskDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.ImportInstanceVolumeDetailItem
import Amazonka.EC2.Types.PlatformValues
import qualified Amazonka.Prelude as Prelude

-- | Describes an import instance task.
--
-- /See:/ 'newImportInstanceTaskDetails' smart constructor.
data ImportInstanceTaskDetails = ImportInstanceTaskDetails'
  { -- | A description of the task.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of the instance.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The instance operating system.
    platform :: Prelude.Maybe PlatformValues,
    -- | The volumes.
    volumes :: Prelude.Maybe [ImportInstanceVolumeDetailItem]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportInstanceTaskDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'importInstanceTaskDetails_description' - A description of the task.
--
-- 'instanceId', 'importInstanceTaskDetails_instanceId' - The ID of the instance.
--
-- 'platform', 'importInstanceTaskDetails_platform' - The instance operating system.
--
-- 'volumes', 'importInstanceTaskDetails_volumes' - The volumes.
newImportInstanceTaskDetails ::
  ImportInstanceTaskDetails
newImportInstanceTaskDetails =
  ImportInstanceTaskDetails'
    { description =
        Prelude.Nothing,
      instanceId = Prelude.Nothing,
      platform = Prelude.Nothing,
      volumes = Prelude.Nothing
    }

-- | A description of the task.
importInstanceTaskDetails_description :: Lens.Lens' ImportInstanceTaskDetails (Prelude.Maybe Prelude.Text)
importInstanceTaskDetails_description = Lens.lens (\ImportInstanceTaskDetails' {description} -> description) (\s@ImportInstanceTaskDetails' {} a -> s {description = a} :: ImportInstanceTaskDetails)

-- | The ID of the instance.
importInstanceTaskDetails_instanceId :: Lens.Lens' ImportInstanceTaskDetails (Prelude.Maybe Prelude.Text)
importInstanceTaskDetails_instanceId = Lens.lens (\ImportInstanceTaskDetails' {instanceId} -> instanceId) (\s@ImportInstanceTaskDetails' {} a -> s {instanceId = a} :: ImportInstanceTaskDetails)

-- | The instance operating system.
importInstanceTaskDetails_platform :: Lens.Lens' ImportInstanceTaskDetails (Prelude.Maybe PlatformValues)
importInstanceTaskDetails_platform = Lens.lens (\ImportInstanceTaskDetails' {platform} -> platform) (\s@ImportInstanceTaskDetails' {} a -> s {platform = a} :: ImportInstanceTaskDetails)

-- | The volumes.
importInstanceTaskDetails_volumes :: Lens.Lens' ImportInstanceTaskDetails (Prelude.Maybe [ImportInstanceVolumeDetailItem])
importInstanceTaskDetails_volumes = Lens.lens (\ImportInstanceTaskDetails' {volumes} -> volumes) (\s@ImportInstanceTaskDetails' {} a -> s {volumes = a} :: ImportInstanceTaskDetails) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML ImportInstanceTaskDetails where
  parseXML x =
    ImportInstanceTaskDetails'
      Prelude.<$> (x Data..@? "description")
      Prelude.<*> (x Data..@? "instanceId")
      Prelude.<*> (x Data..@? "platform")
      Prelude.<*> ( x
                      Data..@? "volumes"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )

instance Prelude.Hashable ImportInstanceTaskDetails where
  hashWithSalt _salt ImportInstanceTaskDetails' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` platform
      `Prelude.hashWithSalt` volumes

instance Prelude.NFData ImportInstanceTaskDetails where
  rnf ImportInstanceTaskDetails' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf platform
      `Prelude.seq` Prelude.rnf volumes
