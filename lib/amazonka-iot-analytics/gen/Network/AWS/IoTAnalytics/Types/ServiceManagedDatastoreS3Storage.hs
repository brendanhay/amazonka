{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.ServiceManagedDatastoreS3Storage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.ServiceManagedDatastoreS3Storage
  ( ServiceManagedDatastoreS3Storage (..),

    -- * Smart constructor
    mkServiceManagedDatastoreS3Storage,

    -- * Lenses
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Use this to store data store data in an S3 bucket managed by AWS IoT Analytics. You cannot change the choice of service-managed or customer-managed S3 storage after the data store is created.
--
-- /See:/ 'mkServiceManagedDatastoreS3Storage' smart constructor.
data ServiceManagedDatastoreS3Storage = ServiceManagedDatastoreS3Storage'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ServiceManagedDatastoreS3Storage' with the minimum fields required to make a request.
mkServiceManagedDatastoreS3Storage ::
  ServiceManagedDatastoreS3Storage
mkServiceManagedDatastoreS3Storage =
  ServiceManagedDatastoreS3Storage'

instance Lude.FromJSON ServiceManagedDatastoreS3Storage where
  parseJSON =
    Lude.withObject
      "ServiceManagedDatastoreS3Storage"
      (\x -> Lude.pure ServiceManagedDatastoreS3Storage')

instance Lude.ToJSON ServiceManagedDatastoreS3Storage where
  toJSON = Lude.const (Lude.Object Lude.mempty)
