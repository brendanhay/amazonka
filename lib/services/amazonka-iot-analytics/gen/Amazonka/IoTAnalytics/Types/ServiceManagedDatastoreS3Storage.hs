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
-- Module      : Amazonka.IoTAnalytics.Types.ServiceManagedDatastoreS3Storage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTAnalytics.Types.ServiceManagedDatastoreS3Storage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Used to store data in an Amazon S3 bucket managed by IoT Analytics. You
-- can\'t change the choice of Amazon S3 storage after your data store is
-- created.
--
-- /See:/ 'newServiceManagedDatastoreS3Storage' smart constructor.
data ServiceManagedDatastoreS3Storage = ServiceManagedDatastoreS3Storage'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceManagedDatastoreS3Storage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newServiceManagedDatastoreS3Storage ::
  ServiceManagedDatastoreS3Storage
newServiceManagedDatastoreS3Storage =
  ServiceManagedDatastoreS3Storage'

instance
  Core.FromJSON
    ServiceManagedDatastoreS3Storage
  where
  parseJSON =
    Core.withObject
      "ServiceManagedDatastoreS3Storage"
      ( \x ->
          Prelude.pure ServiceManagedDatastoreS3Storage'
      )

instance
  Prelude.Hashable
    ServiceManagedDatastoreS3Storage
  where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance
  Prelude.NFData
    ServiceManagedDatastoreS3Storage
  where
  rnf _ = ()

instance Core.ToJSON ServiceManagedDatastoreS3Storage where
  toJSON = Prelude.const (Core.Object Prelude.mempty)
