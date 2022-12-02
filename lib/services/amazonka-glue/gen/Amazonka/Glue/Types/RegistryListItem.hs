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
-- Module      : Amazonka.Glue.Types.RegistryListItem
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.RegistryListItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.RegistryStatus
import qualified Amazonka.Prelude as Prelude

-- | A structure containing the details for a registry.
--
-- /See:/ 'newRegistryListItem' smart constructor.
data RegistryListItem = RegistryListItem'
  { -- | The name of the registry.
    registryName :: Prelude.Maybe Prelude.Text,
    -- | The data the registry was created.
    createdTime :: Prelude.Maybe Prelude.Text,
    -- | The status of the registry.
    status :: Prelude.Maybe RegistryStatus,
    -- | A description of the registry.
    description :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the registry.
    registryArn :: Prelude.Maybe Prelude.Text,
    -- | The date the registry was updated.
    updatedTime :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegistryListItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'registryName', 'registryListItem_registryName' - The name of the registry.
--
-- 'createdTime', 'registryListItem_createdTime' - The data the registry was created.
--
-- 'status', 'registryListItem_status' - The status of the registry.
--
-- 'description', 'registryListItem_description' - A description of the registry.
--
-- 'registryArn', 'registryListItem_registryArn' - The Amazon Resource Name (ARN) of the registry.
--
-- 'updatedTime', 'registryListItem_updatedTime' - The date the registry was updated.
newRegistryListItem ::
  RegistryListItem
newRegistryListItem =
  RegistryListItem'
    { registryName = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      status = Prelude.Nothing,
      description = Prelude.Nothing,
      registryArn = Prelude.Nothing,
      updatedTime = Prelude.Nothing
    }

-- | The name of the registry.
registryListItem_registryName :: Lens.Lens' RegistryListItem (Prelude.Maybe Prelude.Text)
registryListItem_registryName = Lens.lens (\RegistryListItem' {registryName} -> registryName) (\s@RegistryListItem' {} a -> s {registryName = a} :: RegistryListItem)

-- | The data the registry was created.
registryListItem_createdTime :: Lens.Lens' RegistryListItem (Prelude.Maybe Prelude.Text)
registryListItem_createdTime = Lens.lens (\RegistryListItem' {createdTime} -> createdTime) (\s@RegistryListItem' {} a -> s {createdTime = a} :: RegistryListItem)

-- | The status of the registry.
registryListItem_status :: Lens.Lens' RegistryListItem (Prelude.Maybe RegistryStatus)
registryListItem_status = Lens.lens (\RegistryListItem' {status} -> status) (\s@RegistryListItem' {} a -> s {status = a} :: RegistryListItem)

-- | A description of the registry.
registryListItem_description :: Lens.Lens' RegistryListItem (Prelude.Maybe Prelude.Text)
registryListItem_description = Lens.lens (\RegistryListItem' {description} -> description) (\s@RegistryListItem' {} a -> s {description = a} :: RegistryListItem)

-- | The Amazon Resource Name (ARN) of the registry.
registryListItem_registryArn :: Lens.Lens' RegistryListItem (Prelude.Maybe Prelude.Text)
registryListItem_registryArn = Lens.lens (\RegistryListItem' {registryArn} -> registryArn) (\s@RegistryListItem' {} a -> s {registryArn = a} :: RegistryListItem)

-- | The date the registry was updated.
registryListItem_updatedTime :: Lens.Lens' RegistryListItem (Prelude.Maybe Prelude.Text)
registryListItem_updatedTime = Lens.lens (\RegistryListItem' {updatedTime} -> updatedTime) (\s@RegistryListItem' {} a -> s {updatedTime = a} :: RegistryListItem)

instance Data.FromJSON RegistryListItem where
  parseJSON =
    Data.withObject
      "RegistryListItem"
      ( \x ->
          RegistryListItem'
            Prelude.<$> (x Data..:? "RegistryName")
            Prelude.<*> (x Data..:? "CreatedTime")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "RegistryArn")
            Prelude.<*> (x Data..:? "UpdatedTime")
      )

instance Prelude.Hashable RegistryListItem where
  hashWithSalt _salt RegistryListItem' {..} =
    _salt `Prelude.hashWithSalt` registryName
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` registryArn
      `Prelude.hashWithSalt` updatedTime

instance Prelude.NFData RegistryListItem where
  rnf RegistryListItem' {..} =
    Prelude.rnf registryName
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf registryArn
      `Prelude.seq` Prelude.rnf updatedTime
