-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.Types.GroupConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroups.Types.GroupConfiguration
  ( GroupConfiguration (..),

    -- * Smart constructor
    mkGroupConfiguration,

    -- * Lenses
    gcStatus,
    gcFailureReason,
    gcProposedConfiguration,
    gcConfiguration,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.ResourceGroups.Types.GroupConfigurationItem
import Network.AWS.ResourceGroups.Types.GroupConfigurationStatus

-- | A service configuration associated with a resource group. The configuration options are determined by the AWS service that defines the @Type@ , and specifies which resources can be included in the group. You can add a service configuration when you create the group.
--
-- /See:/ 'mkGroupConfiguration' smart constructor.
data GroupConfiguration = GroupConfiguration'
  { status ::
      Lude.Maybe GroupConfigurationStatus,
    failureReason :: Lude.Maybe Lude.Text,
    proposedConfiguration ::
      Lude.Maybe [GroupConfigurationItem],
    configuration :: Lude.Maybe [GroupConfigurationItem]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GroupConfiguration' with the minimum fields required to make a request.
--
-- * 'configuration' - The configuration currently associated with the group and in effect.
-- * 'failureReason' - If present, the reason why a request to update the group configuration failed.
-- * 'proposedConfiguration' - If present, the new configuration that is in the process of being applied to the group.
-- * 'status' - The current status of an attempt to update the group configuration.
mkGroupConfiguration ::
  GroupConfiguration
mkGroupConfiguration =
  GroupConfiguration'
    { status = Lude.Nothing,
      failureReason = Lude.Nothing,
      proposedConfiguration = Lude.Nothing,
      configuration = Lude.Nothing
    }

-- | The current status of an attempt to update the group configuration.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcStatus :: Lens.Lens' GroupConfiguration (Lude.Maybe GroupConfigurationStatus)
gcStatus = Lens.lens (status :: GroupConfiguration -> Lude.Maybe GroupConfigurationStatus) (\s a -> s {status = a} :: GroupConfiguration)
{-# DEPRECATED gcStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | If present, the reason why a request to update the group configuration failed.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcFailureReason :: Lens.Lens' GroupConfiguration (Lude.Maybe Lude.Text)
gcFailureReason = Lens.lens (failureReason :: GroupConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {failureReason = a} :: GroupConfiguration)
{-# DEPRECATED gcFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | If present, the new configuration that is in the process of being applied to the group.
--
-- /Note:/ Consider using 'proposedConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcProposedConfiguration :: Lens.Lens' GroupConfiguration (Lude.Maybe [GroupConfigurationItem])
gcProposedConfiguration = Lens.lens (proposedConfiguration :: GroupConfiguration -> Lude.Maybe [GroupConfigurationItem]) (\s a -> s {proposedConfiguration = a} :: GroupConfiguration)
{-# DEPRECATED gcProposedConfiguration "Use generic-lens or generic-optics with 'proposedConfiguration' instead." #-}

-- | The configuration currently associated with the group and in effect.
--
-- /Note:/ Consider using 'configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcConfiguration :: Lens.Lens' GroupConfiguration (Lude.Maybe [GroupConfigurationItem])
gcConfiguration = Lens.lens (configuration :: GroupConfiguration -> Lude.Maybe [GroupConfigurationItem]) (\s a -> s {configuration = a} :: GroupConfiguration)
{-# DEPRECATED gcConfiguration "Use generic-lens or generic-optics with 'configuration' instead." #-}

instance Lude.FromJSON GroupConfiguration where
  parseJSON =
    Lude.withObject
      "GroupConfiguration"
      ( \x ->
          GroupConfiguration'
            Lude.<$> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "FailureReason")
            Lude.<*> (x Lude..:? "ProposedConfiguration" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Configuration" Lude..!= Lude.mempty)
      )
