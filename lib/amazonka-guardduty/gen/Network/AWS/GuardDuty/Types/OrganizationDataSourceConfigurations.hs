-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.OrganizationDataSourceConfigurations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.OrganizationDataSourceConfigurations
  ( OrganizationDataSourceConfigurations (..),

    -- * Smart constructor
    mkOrganizationDataSourceConfigurations,

    -- * Lenses
    odscS3Logs,
  )
where

import Network.AWS.GuardDuty.Types.OrganizationS3LogsConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object that contains information on which data sources will be configured to be automatically enabled for new members within the organization.
--
-- /See:/ 'mkOrganizationDataSourceConfigurations' smart constructor.
newtype OrganizationDataSourceConfigurations = OrganizationDataSourceConfigurations'
  { s3Logs ::
      Lude.Maybe
        OrganizationS3LogsConfiguration
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OrganizationDataSourceConfigurations' with the minimum fields required to make a request.
--
-- * 's3Logs' - Describes whether S3 data event logs are enabled for new members of the organization.
mkOrganizationDataSourceConfigurations ::
  OrganizationDataSourceConfigurations
mkOrganizationDataSourceConfigurations =
  OrganizationDataSourceConfigurations' {s3Logs = Lude.Nothing}

-- | Describes whether S3 data event logs are enabled for new members of the organization.
--
-- /Note:/ Consider using 's3Logs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odscS3Logs :: Lens.Lens' OrganizationDataSourceConfigurations (Lude.Maybe OrganizationS3LogsConfiguration)
odscS3Logs = Lens.lens (s3Logs :: OrganizationDataSourceConfigurations -> Lude.Maybe OrganizationS3LogsConfiguration) (\s a -> s {s3Logs = a} :: OrganizationDataSourceConfigurations)
{-# DEPRECATED odscS3Logs "Use generic-lens or generic-optics with 's3Logs' instead." #-}

instance Lude.ToJSON OrganizationDataSourceConfigurations where
  toJSON OrganizationDataSourceConfigurations' {..} =
    Lude.object (Lude.catMaybes [("s3Logs" Lude..=) Lude.<$> s3Logs])
