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
-- Module      : Network.AWS.IoTAnalytics.Types.ServiceManagedDatastoreS3Storage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.ServiceManagedDatastoreS3Storage where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Use this to store data store data in an S3 bucket managed by AWS IoT
-- Analytics. You cannot change the choice of service-managed or
-- customer-managed S3 storage after the data store is created.
--
-- /See:/ 'newServiceManagedDatastoreS3Storage' smart constructor.
data ServiceManagedDatastoreS3Storage = ServiceManagedDatastoreS3Storage'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
      (\x -> Core.pure ServiceManagedDatastoreS3Storage')

instance
  Core.Hashable
    ServiceManagedDatastoreS3Storage

instance Core.NFData ServiceManagedDatastoreS3Storage

instance Core.ToJSON ServiceManagedDatastoreS3Storage where
  toJSON = Core.const (Core.Object Core.mempty)
