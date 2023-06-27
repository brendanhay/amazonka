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
-- Module      : Amazonka.AppStream.Types.StorageConnector
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppStream.Types.StorageConnector where

import Amazonka.AppStream.Types.StorageConnectorType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a connector that enables persistent storage for users.
--
-- /See:/ 'newStorageConnector' smart constructor.
data StorageConnector = StorageConnector'
  { -- | The names of the domains for the account.
    domains :: Prelude.Maybe [Prelude.Text],
    -- | The ARN of the storage connector.
    resourceIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The type of storage connector.
    connectorType :: StorageConnectorType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StorageConnector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domains', 'storageConnector_domains' - The names of the domains for the account.
--
-- 'resourceIdentifier', 'storageConnector_resourceIdentifier' - The ARN of the storage connector.
--
-- 'connectorType', 'storageConnector_connectorType' - The type of storage connector.
newStorageConnector ::
  -- | 'connectorType'
  StorageConnectorType ->
  StorageConnector
newStorageConnector pConnectorType_ =
  StorageConnector'
    { domains = Prelude.Nothing,
      resourceIdentifier = Prelude.Nothing,
      connectorType = pConnectorType_
    }

-- | The names of the domains for the account.
storageConnector_domains :: Lens.Lens' StorageConnector (Prelude.Maybe [Prelude.Text])
storageConnector_domains = Lens.lens (\StorageConnector' {domains} -> domains) (\s@StorageConnector' {} a -> s {domains = a} :: StorageConnector) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the storage connector.
storageConnector_resourceIdentifier :: Lens.Lens' StorageConnector (Prelude.Maybe Prelude.Text)
storageConnector_resourceIdentifier = Lens.lens (\StorageConnector' {resourceIdentifier} -> resourceIdentifier) (\s@StorageConnector' {} a -> s {resourceIdentifier = a} :: StorageConnector)

-- | The type of storage connector.
storageConnector_connectorType :: Lens.Lens' StorageConnector StorageConnectorType
storageConnector_connectorType = Lens.lens (\StorageConnector' {connectorType} -> connectorType) (\s@StorageConnector' {} a -> s {connectorType = a} :: StorageConnector)

instance Data.FromJSON StorageConnector where
  parseJSON =
    Data.withObject
      "StorageConnector"
      ( \x ->
          StorageConnector'
            Prelude.<$> (x Data..:? "Domains" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ResourceIdentifier")
            Prelude.<*> (x Data..: "ConnectorType")
      )

instance Prelude.Hashable StorageConnector where
  hashWithSalt _salt StorageConnector' {..} =
    _salt
      `Prelude.hashWithSalt` domains
      `Prelude.hashWithSalt` resourceIdentifier
      `Prelude.hashWithSalt` connectorType

instance Prelude.NFData StorageConnector where
  rnf StorageConnector' {..} =
    Prelude.rnf domains
      `Prelude.seq` Prelude.rnf resourceIdentifier
      `Prelude.seq` Prelude.rnf connectorType

instance Data.ToJSON StorageConnector where
  toJSON StorageConnector' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Domains" Data..=) Prelude.<$> domains,
            ("ResourceIdentifier" Data..=)
              Prelude.<$> resourceIdentifier,
            Prelude.Just
              ("ConnectorType" Data..= connectorType)
          ]
      )
