{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.ServiceManagedDatastoreS3StorageSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.ServiceManagedDatastoreS3StorageSummary
  ( ServiceManagedDatastoreS3StorageSummary (..),

    -- * Smart constructor
    mkServiceManagedDatastoreS3StorageSummary,

    -- * Lenses
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Used to store data store data in an S3 bucket managed by AWS IoT Analytics.
--
-- /See:/ 'mkServiceManagedDatastoreS3StorageSummary' smart constructor.
data ServiceManagedDatastoreS3StorageSummary = ServiceManagedDatastoreS3StorageSummary'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ServiceManagedDatastoreS3StorageSummary' with the minimum fields required to make a request.
mkServiceManagedDatastoreS3StorageSummary ::
  ServiceManagedDatastoreS3StorageSummary
mkServiceManagedDatastoreS3StorageSummary =
  ServiceManagedDatastoreS3StorageSummary'

instance Lude.FromJSON ServiceManagedDatastoreS3StorageSummary where
  parseJSON =
    Lude.withObject
      "ServiceManagedDatastoreS3StorageSummary"
      (\x -> Lude.pure ServiceManagedDatastoreS3StorageSummary')
