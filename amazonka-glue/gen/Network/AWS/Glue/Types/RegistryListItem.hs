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
-- Module      : Network.AWS.Glue.Types.RegistryListItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.RegistryListItem where

import Network.AWS.Glue.Types.RegistryStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A structure containing the details for a registry.
--
-- /See:/ 'newRegistryListItem' smart constructor.
data RegistryListItem = RegistryListItem'
  { -- | The status of the registry.
    status :: Prelude.Maybe RegistryStatus,
    -- | The date the registry was updated.
    updatedTime :: Prelude.Maybe Prelude.Text,
    -- | The data the registry was created.
    createdTime :: Prelude.Maybe Prelude.Text,
    -- | The name of the registry.
    registryName :: Prelude.Maybe Prelude.Text,
    -- | A description of the registry.
    description :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the registry.
    registryArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RegistryListItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'registryListItem_status' - The status of the registry.
--
-- 'updatedTime', 'registryListItem_updatedTime' - The date the registry was updated.
--
-- 'createdTime', 'registryListItem_createdTime' - The data the registry was created.
--
-- 'registryName', 'registryListItem_registryName' - The name of the registry.
--
-- 'description', 'registryListItem_description' - A description of the registry.
--
-- 'registryArn', 'registryListItem_registryArn' - The Amazon Resource Name (ARN) of the registry.
newRegistryListItem ::
  RegistryListItem
newRegistryListItem =
  RegistryListItem'
    { status = Prelude.Nothing,
      updatedTime = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      registryName = Prelude.Nothing,
      description = Prelude.Nothing,
      registryArn = Prelude.Nothing
    }

-- | The status of the registry.
registryListItem_status :: Lens.Lens' RegistryListItem (Prelude.Maybe RegistryStatus)
registryListItem_status = Lens.lens (\RegistryListItem' {status} -> status) (\s@RegistryListItem' {} a -> s {status = a} :: RegistryListItem)

-- | The date the registry was updated.
registryListItem_updatedTime :: Lens.Lens' RegistryListItem (Prelude.Maybe Prelude.Text)
registryListItem_updatedTime = Lens.lens (\RegistryListItem' {updatedTime} -> updatedTime) (\s@RegistryListItem' {} a -> s {updatedTime = a} :: RegistryListItem)

-- | The data the registry was created.
registryListItem_createdTime :: Lens.Lens' RegistryListItem (Prelude.Maybe Prelude.Text)
registryListItem_createdTime = Lens.lens (\RegistryListItem' {createdTime} -> createdTime) (\s@RegistryListItem' {} a -> s {createdTime = a} :: RegistryListItem)

-- | The name of the registry.
registryListItem_registryName :: Lens.Lens' RegistryListItem (Prelude.Maybe Prelude.Text)
registryListItem_registryName = Lens.lens (\RegistryListItem' {registryName} -> registryName) (\s@RegistryListItem' {} a -> s {registryName = a} :: RegistryListItem)

-- | A description of the registry.
registryListItem_description :: Lens.Lens' RegistryListItem (Prelude.Maybe Prelude.Text)
registryListItem_description = Lens.lens (\RegistryListItem' {description} -> description) (\s@RegistryListItem' {} a -> s {description = a} :: RegistryListItem)

-- | The Amazon Resource Name (ARN) of the registry.
registryListItem_registryArn :: Lens.Lens' RegistryListItem (Prelude.Maybe Prelude.Text)
registryListItem_registryArn = Lens.lens (\RegistryListItem' {registryArn} -> registryArn) (\s@RegistryListItem' {} a -> s {registryArn = a} :: RegistryListItem)

instance Prelude.FromJSON RegistryListItem where
  parseJSON =
    Prelude.withObject
      "RegistryListItem"
      ( \x ->
          RegistryListItem'
            Prelude.<$> (x Prelude..:? "Status")
            Prelude.<*> (x Prelude..:? "UpdatedTime")
            Prelude.<*> (x Prelude..:? "CreatedTime")
            Prelude.<*> (x Prelude..:? "RegistryName")
            Prelude.<*> (x Prelude..:? "Description")
            Prelude.<*> (x Prelude..:? "RegistryArn")
      )

instance Prelude.Hashable RegistryListItem

instance Prelude.NFData RegistryListItem
