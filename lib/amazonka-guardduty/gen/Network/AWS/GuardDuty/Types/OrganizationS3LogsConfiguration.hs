{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.OrganizationS3LogsConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.OrganizationS3LogsConfiguration
  ( OrganizationS3LogsConfiguration (..),

    -- * Smart constructor
    mkOrganizationS3LogsConfiguration,

    -- * Lenses
    oslcAutoEnable,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes whether S3 data event logs will be automatically enabled for new members of the organization.
--
-- /See:/ 'mkOrganizationS3LogsConfiguration' smart constructor.
newtype OrganizationS3LogsConfiguration = OrganizationS3LogsConfiguration'
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

-- | Creates a value of 'OrganizationS3LogsConfiguration' with the minimum fields required to make a request.
--
-- * 'autoEnable' - A value that contains information on whether S3 data event logs will be enabled automatically as a data source for the organization.
mkOrganizationS3LogsConfiguration ::
  -- | 'autoEnable'
  Lude.Bool ->
  OrganizationS3LogsConfiguration
mkOrganizationS3LogsConfiguration pAutoEnable_ =
  OrganizationS3LogsConfiguration' {autoEnable = pAutoEnable_}

-- | A value that contains information on whether S3 data event logs will be enabled automatically as a data source for the organization.
--
-- /Note:/ Consider using 'autoEnable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oslcAutoEnable :: Lens.Lens' OrganizationS3LogsConfiguration Lude.Bool
oslcAutoEnable = Lens.lens (autoEnable :: OrganizationS3LogsConfiguration -> Lude.Bool) (\s a -> s {autoEnable = a} :: OrganizationS3LogsConfiguration)
{-# DEPRECATED oslcAutoEnable "Use generic-lens or generic-optics with 'autoEnable' instead." #-}

instance Lude.ToJSON OrganizationS3LogsConfiguration where
  toJSON OrganizationS3LogsConfiguration' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("autoEnable" Lude..= autoEnable)])
