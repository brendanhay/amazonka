{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.CloudTrailConfigurationResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.CloudTrailConfigurationResult
  ( CloudTrailConfigurationResult (..),

    -- * Smart constructor
    mkCloudTrailConfigurationResult,

    -- * Lenses
    ctcrStatus,
  )
where

import Network.AWS.GuardDuty.Types.DataSourceStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information on the status of CloudTrail as a data source for the detector.
--
-- /See:/ 'mkCloudTrailConfigurationResult' smart constructor.
newtype CloudTrailConfigurationResult = CloudTrailConfigurationResult'
  { status ::
      DataSourceStatus
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CloudTrailConfigurationResult' with the minimum fields required to make a request.
--
-- * 'status' - Describes whether CloudTrail is enabled as a data source for the detector.
mkCloudTrailConfigurationResult ::
  -- | 'status'
  DataSourceStatus ->
  CloudTrailConfigurationResult
mkCloudTrailConfigurationResult pStatus_ =
  CloudTrailConfigurationResult' {status = pStatus_}

-- | Describes whether CloudTrail is enabled as a data source for the detector.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctcrStatus :: Lens.Lens' CloudTrailConfigurationResult DataSourceStatus
ctcrStatus = Lens.lens (status :: CloudTrailConfigurationResult -> DataSourceStatus) (\s a -> s {status = a} :: CloudTrailConfigurationResult)
{-# DEPRECATED ctcrStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Lude.FromJSON CloudTrailConfigurationResult where
  parseJSON =
    Lude.withObject
      "CloudTrailConfigurationResult"
      ( \x ->
          CloudTrailConfigurationResult' Lude.<$> (x Lude..: "status")
      )
