{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.OrganizationDataSourceConfigurationsResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.OrganizationDataSourceConfigurationsResult
  ( OrganizationDataSourceConfigurationsResult (..),

    -- * Smart constructor
    mkOrganizationDataSourceConfigurationsResult,

    -- * Lenses
    odscrS3Logs,
  )
where

import Network.AWS.GuardDuty.Types.OrganizationS3LogsConfigurationResult
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object that contains information on which data sources are automatically enabled for new members within the organization.
--
-- /See:/ 'mkOrganizationDataSourceConfigurationsResult' smart constructor.
newtype OrganizationDataSourceConfigurationsResult = OrganizationDataSourceConfigurationsResult'
  { -- | Describes whether S3 data event logs are enabled as a data source.
    s3Logs :: OrganizationS3LogsConfigurationResult
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OrganizationDataSourceConfigurationsResult' with the minimum fields required to make a request.
--
-- * 's3Logs' - Describes whether S3 data event logs are enabled as a data source.
mkOrganizationDataSourceConfigurationsResult ::
  -- | 's3Logs'
  OrganizationS3LogsConfigurationResult ->
  OrganizationDataSourceConfigurationsResult
mkOrganizationDataSourceConfigurationsResult pS3Logs_ =
  OrganizationDataSourceConfigurationsResult' {s3Logs = pS3Logs_}

-- | Describes whether S3 data event logs are enabled as a data source.
--
-- /Note:/ Consider using 's3Logs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odscrS3Logs :: Lens.Lens' OrganizationDataSourceConfigurationsResult OrganizationS3LogsConfigurationResult
odscrS3Logs = Lens.lens (s3Logs :: OrganizationDataSourceConfigurationsResult -> OrganizationS3LogsConfigurationResult) (\s a -> s {s3Logs = a} :: OrganizationDataSourceConfigurationsResult)
{-# DEPRECATED odscrS3Logs "Use generic-lens or generic-optics with 's3Logs' instead." #-}

instance Lude.FromJSON OrganizationDataSourceConfigurationsResult where
  parseJSON =
    Lude.withObject
      "OrganizationDataSourceConfigurationsResult"
      ( \x ->
          OrganizationDataSourceConfigurationsResult'
            Lude.<$> (x Lude..: "s3Logs")
      )
