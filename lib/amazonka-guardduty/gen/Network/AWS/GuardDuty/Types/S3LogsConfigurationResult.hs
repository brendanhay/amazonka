{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.S3LogsConfigurationResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.S3LogsConfigurationResult
  ( S3LogsConfigurationResult (..),

    -- * Smart constructor
    mkS3LogsConfigurationResult,

    -- * Lenses
    slcrStatus,
  )
where

import Network.AWS.GuardDuty.Types.DataSourceStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes whether S3 data event logs will be enabled as a data source.
--
-- /See:/ 'mkS3LogsConfigurationResult' smart constructor.
newtype S3LogsConfigurationResult = S3LogsConfigurationResult'
  { -- | A value that describes whether S3 data event logs are automatically enabled for new members of the organization.
    status :: DataSourceStatus
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'S3LogsConfigurationResult' with the minimum fields required to make a request.
--
-- * 'status' - A value that describes whether S3 data event logs are automatically enabled for new members of the organization.
mkS3LogsConfigurationResult ::
  -- | 'status'
  DataSourceStatus ->
  S3LogsConfigurationResult
mkS3LogsConfigurationResult pStatus_ =
  S3LogsConfigurationResult' {status = pStatus_}

-- | A value that describes whether S3 data event logs are automatically enabled for new members of the organization.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slcrStatus :: Lens.Lens' S3LogsConfigurationResult DataSourceStatus
slcrStatus = Lens.lens (status :: S3LogsConfigurationResult -> DataSourceStatus) (\s a -> s {status = a} :: S3LogsConfigurationResult)
{-# DEPRECATED slcrStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Lude.FromJSON S3LogsConfigurationResult where
  parseJSON =
    Lude.withObject
      "S3LogsConfigurationResult"
      (\x -> S3LogsConfigurationResult' Lude.<$> (x Lude..: "status"))
