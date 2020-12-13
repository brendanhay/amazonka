{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ResourceChange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ResourceChange
  ( ResourceChange (..),

    -- * Smart constructor
    mkResourceChange,

    -- * Lenses
    rcLogicalResourceId,
    rcPhysicalResourceId,
    rcResourceType,
    rcAction,
    rcScope,
    rcDetails,
    rcReplacement,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.ServiceCatalog.Types.ChangeAction
import Network.AWS.ServiceCatalog.Types.Replacement
import Network.AWS.ServiceCatalog.Types.ResourceAttribute
import Network.AWS.ServiceCatalog.Types.ResourceChangeDetail

-- | Information about a resource change that will occur when a plan is executed.
--
-- /See:/ 'mkResourceChange' smart constructor.
data ResourceChange = ResourceChange'
  { -- | The ID of the resource, as defined in the CloudFormation template.
    logicalResourceId :: Lude.Maybe Lude.Text,
    -- | The ID of the resource, if it was already created.
    physicalResourceId :: Lude.Maybe Lude.Text,
    -- | The type of resource.
    resourceType :: Lude.Maybe Lude.Text,
    -- | The change action.
    action :: Lude.Maybe ChangeAction,
    -- | The change scope.
    scope :: Lude.Maybe [ResourceAttribute],
    -- | Information about the resource changes.
    details :: Lude.Maybe [ResourceChangeDetail],
    -- | If the change type is @Modify@ , indicates whether the existing resource is deleted and replaced with a new one.
    replacement :: Lude.Maybe Replacement
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResourceChange' with the minimum fields required to make a request.
--
-- * 'logicalResourceId' - The ID of the resource, as defined in the CloudFormation template.
-- * 'physicalResourceId' - The ID of the resource, if it was already created.
-- * 'resourceType' - The type of resource.
-- * 'action' - The change action.
-- * 'scope' - The change scope.
-- * 'details' - Information about the resource changes.
-- * 'replacement' - If the change type is @Modify@ , indicates whether the existing resource is deleted and replaced with a new one.
mkResourceChange ::
  ResourceChange
mkResourceChange =
  ResourceChange'
    { logicalResourceId = Lude.Nothing,
      physicalResourceId = Lude.Nothing,
      resourceType = Lude.Nothing,
      action = Lude.Nothing,
      scope = Lude.Nothing,
      details = Lude.Nothing,
      replacement = Lude.Nothing
    }

-- | The ID of the resource, as defined in the CloudFormation template.
--
-- /Note:/ Consider using 'logicalResourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcLogicalResourceId :: Lens.Lens' ResourceChange (Lude.Maybe Lude.Text)
rcLogicalResourceId = Lens.lens (logicalResourceId :: ResourceChange -> Lude.Maybe Lude.Text) (\s a -> s {logicalResourceId = a} :: ResourceChange)
{-# DEPRECATED rcLogicalResourceId "Use generic-lens or generic-optics with 'logicalResourceId' instead." #-}

-- | The ID of the resource, if it was already created.
--
-- /Note:/ Consider using 'physicalResourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcPhysicalResourceId :: Lens.Lens' ResourceChange (Lude.Maybe Lude.Text)
rcPhysicalResourceId = Lens.lens (physicalResourceId :: ResourceChange -> Lude.Maybe Lude.Text) (\s a -> s {physicalResourceId = a} :: ResourceChange)
{-# DEPRECATED rcPhysicalResourceId "Use generic-lens or generic-optics with 'physicalResourceId' instead." #-}

-- | The type of resource.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcResourceType :: Lens.Lens' ResourceChange (Lude.Maybe Lude.Text)
rcResourceType = Lens.lens (resourceType :: ResourceChange -> Lude.Maybe Lude.Text) (\s a -> s {resourceType = a} :: ResourceChange)
{-# DEPRECATED rcResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The change action.
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcAction :: Lens.Lens' ResourceChange (Lude.Maybe ChangeAction)
rcAction = Lens.lens (action :: ResourceChange -> Lude.Maybe ChangeAction) (\s a -> s {action = a} :: ResourceChange)
{-# DEPRECATED rcAction "Use generic-lens or generic-optics with 'action' instead." #-}

-- | The change scope.
--
-- /Note:/ Consider using 'scope' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcScope :: Lens.Lens' ResourceChange (Lude.Maybe [ResourceAttribute])
rcScope = Lens.lens (scope :: ResourceChange -> Lude.Maybe [ResourceAttribute]) (\s a -> s {scope = a} :: ResourceChange)
{-# DEPRECATED rcScope "Use generic-lens or generic-optics with 'scope' instead." #-}

-- | Information about the resource changes.
--
-- /Note:/ Consider using 'details' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcDetails :: Lens.Lens' ResourceChange (Lude.Maybe [ResourceChangeDetail])
rcDetails = Lens.lens (details :: ResourceChange -> Lude.Maybe [ResourceChangeDetail]) (\s a -> s {details = a} :: ResourceChange)
{-# DEPRECATED rcDetails "Use generic-lens or generic-optics with 'details' instead." #-}

-- | If the change type is @Modify@ , indicates whether the existing resource is deleted and replaced with a new one.
--
-- /Note:/ Consider using 'replacement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcReplacement :: Lens.Lens' ResourceChange (Lude.Maybe Replacement)
rcReplacement = Lens.lens (replacement :: ResourceChange -> Lude.Maybe Replacement) (\s a -> s {replacement = a} :: ResourceChange)
{-# DEPRECATED rcReplacement "Use generic-lens or generic-optics with 'replacement' instead." #-}

instance Lude.FromJSON ResourceChange where
  parseJSON =
    Lude.withObject
      "ResourceChange"
      ( \x ->
          ResourceChange'
            Lude.<$> (x Lude..:? "LogicalResourceId")
            Lude.<*> (x Lude..:? "PhysicalResourceId")
            Lude.<*> (x Lude..:? "ResourceType")
            Lude.<*> (x Lude..:? "Action")
            Lude.<*> (x Lude..:? "Scope" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Details" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Replacement")
      )
