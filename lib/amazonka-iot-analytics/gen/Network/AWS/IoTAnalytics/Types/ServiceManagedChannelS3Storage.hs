{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.ServiceManagedChannelS3Storage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.ServiceManagedChannelS3Storage
  ( ServiceManagedChannelS3Storage (..),

    -- * Smart constructor
    mkServiceManagedChannelS3Storage,

    -- * Lenses
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Use this to store channel data in an S3 bucket managed by AWS IoT Analytics. You cannot change the choice of service-managed or customer-managed S3 storage after the channel is created.
--
-- /See:/ 'mkServiceManagedChannelS3Storage' smart constructor.
data ServiceManagedChannelS3Storage = ServiceManagedChannelS3Storage'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ServiceManagedChannelS3Storage' with the minimum fields required to make a request.
mkServiceManagedChannelS3Storage ::
  ServiceManagedChannelS3Storage
mkServiceManagedChannelS3Storage = ServiceManagedChannelS3Storage'

instance Lude.FromJSON ServiceManagedChannelS3Storage where
  parseJSON =
    Lude.withObject
      "ServiceManagedChannelS3Storage"
      (\x -> Lude.pure ServiceManagedChannelS3Storage')

instance Lude.ToJSON ServiceManagedChannelS3Storage where
  toJSON = Lude.const (Lude.Object Lude.mempty)
