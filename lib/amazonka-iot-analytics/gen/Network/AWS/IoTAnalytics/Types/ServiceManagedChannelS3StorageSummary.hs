-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.ServiceManagedChannelS3StorageSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.ServiceManagedChannelS3StorageSummary
  ( ServiceManagedChannelS3StorageSummary (..),

    -- * Smart constructor
    mkServiceManagedChannelS3StorageSummary,

    -- * Lenses
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Used to store channel data in an S3 bucket managed by AWS IoT Analytics.
--
-- /See:/ 'mkServiceManagedChannelS3StorageSummary' smart constructor.
data ServiceManagedChannelS3StorageSummary = ServiceManagedChannelS3StorageSummary'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ServiceManagedChannelS3StorageSummary' with the minimum fields required to make a request.
mkServiceManagedChannelS3StorageSummary ::
  ServiceManagedChannelS3StorageSummary
mkServiceManagedChannelS3StorageSummary =
  ServiceManagedChannelS3StorageSummary'

instance Lude.FromJSON ServiceManagedChannelS3StorageSummary where
  parseJSON =
    Lude.withObject
      "ServiceManagedChannelS3StorageSummary"
      (\x -> Lude.pure ServiceManagedChannelS3StorageSummary')
