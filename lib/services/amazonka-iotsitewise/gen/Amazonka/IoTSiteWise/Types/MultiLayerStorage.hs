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
-- Module      : Amazonka.IoTSiteWise.Types.MultiLayerStorage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.MultiLayerStorage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types.CustomerManagedS3Storage
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the storage destination.
--
-- /See:/ 'newMultiLayerStorage' smart constructor.
data MultiLayerStorage = MultiLayerStorage'
  { -- | Contains information about a customer managed Amazon S3 bucket.
    customerManagedS3Storage :: CustomerManagedS3Storage
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MultiLayerStorage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customerManagedS3Storage', 'multiLayerStorage_customerManagedS3Storage' - Contains information about a customer managed Amazon S3 bucket.
newMultiLayerStorage ::
  -- | 'customerManagedS3Storage'
  CustomerManagedS3Storage ->
  MultiLayerStorage
newMultiLayerStorage pCustomerManagedS3Storage_ =
  MultiLayerStorage'
    { customerManagedS3Storage =
        pCustomerManagedS3Storage_
    }

-- | Contains information about a customer managed Amazon S3 bucket.
multiLayerStorage_customerManagedS3Storage :: Lens.Lens' MultiLayerStorage CustomerManagedS3Storage
multiLayerStorage_customerManagedS3Storage = Lens.lens (\MultiLayerStorage' {customerManagedS3Storage} -> customerManagedS3Storage) (\s@MultiLayerStorage' {} a -> s {customerManagedS3Storage = a} :: MultiLayerStorage)

instance Data.FromJSON MultiLayerStorage where
  parseJSON =
    Data.withObject
      "MultiLayerStorage"
      ( \x ->
          MultiLayerStorage'
            Prelude.<$> (x Data..: "customerManagedS3Storage")
      )

instance Prelude.Hashable MultiLayerStorage where
  hashWithSalt _salt MultiLayerStorage' {..} =
    _salt
      `Prelude.hashWithSalt` customerManagedS3Storage

instance Prelude.NFData MultiLayerStorage where
  rnf MultiLayerStorage' {..} =
    Prelude.rnf customerManagedS3Storage

instance Data.ToJSON MultiLayerStorage where
  toJSON MultiLayerStorage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "customerManagedS3Storage"
                  Data..= customerManagedS3Storage
              )
          ]
      )
