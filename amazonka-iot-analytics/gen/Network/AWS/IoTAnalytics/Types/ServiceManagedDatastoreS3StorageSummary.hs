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
-- Module      : Network.AWS.IoTAnalytics.Types.ServiceManagedDatastoreS3StorageSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.ServiceManagedDatastoreS3StorageSummary where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Used to store data store data in an S3 bucket managed by AWS IoT
-- Analytics.
--
-- /See:/ 'newServiceManagedDatastoreS3StorageSummary' smart constructor.
data ServiceManagedDatastoreS3StorageSummary = ServiceManagedDatastoreS3StorageSummary'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ServiceManagedDatastoreS3StorageSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newServiceManagedDatastoreS3StorageSummary ::
  ServiceManagedDatastoreS3StorageSummary
newServiceManagedDatastoreS3StorageSummary =
  ServiceManagedDatastoreS3StorageSummary'

instance
  Prelude.FromJSON
    ServiceManagedDatastoreS3StorageSummary
  where
  parseJSON =
    Prelude.withObject
      "ServiceManagedDatastoreS3StorageSummary"
      ( \x ->
          Prelude.pure
            ServiceManagedDatastoreS3StorageSummary'
      )

instance
  Prelude.Hashable
    ServiceManagedDatastoreS3StorageSummary

instance
  Prelude.NFData
    ServiceManagedDatastoreS3StorageSummary
