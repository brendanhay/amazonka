{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.OrganizationS3LogsConfigurationResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.OrganizationS3LogsConfigurationResult
  ( OrganizationS3LogsConfigurationResult (..),

    -- * Smart constructor
    mkOrganizationS3LogsConfigurationResult,

    -- * Lenses
    oslcrAutoEnable,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The current configuration of S3 data event logs as a data source for the organization.
--
-- /See:/ 'mkOrganizationS3LogsConfigurationResult' smart constructor.
newtype OrganizationS3LogsConfigurationResult = OrganizationS3LogsConfigurationResult'
  { autoEnable ::
      Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OrganizationS3LogsConfigurationResult' with the minimum fields required to make a request.
--
-- * 'autoEnable' - A value that describes whether S3 data event logs are automatically enabled for new members of the organization.
mkOrganizationS3LogsConfigurationResult ::
  -- | 'autoEnable'
  Lude.Bool ->
  OrganizationS3LogsConfigurationResult
mkOrganizationS3LogsConfigurationResult pAutoEnable_ =
  OrganizationS3LogsConfigurationResult' {autoEnable = pAutoEnable_}

-- | A value that describes whether S3 data event logs are automatically enabled for new members of the organization.
--
-- /Note:/ Consider using 'autoEnable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oslcrAutoEnable :: Lens.Lens' OrganizationS3LogsConfigurationResult Lude.Bool
oslcrAutoEnable = Lens.lens (autoEnable :: OrganizationS3LogsConfigurationResult -> Lude.Bool) (\s a -> s {autoEnable = a} :: OrganizationS3LogsConfigurationResult)
{-# DEPRECATED oslcrAutoEnable "Use generic-lens or generic-optics with 'autoEnable' instead." #-}

instance Lude.FromJSON OrganizationS3LogsConfigurationResult where
  parseJSON =
    Lude.withObject
      "OrganizationS3LogsConfigurationResult"
      ( \x ->
          OrganizationS3LogsConfigurationResult'
            Lude.<$> (x Lude..: "autoEnable")
      )
