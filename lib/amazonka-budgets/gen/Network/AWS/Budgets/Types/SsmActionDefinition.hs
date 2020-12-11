-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.SsmActionDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.SsmActionDefinition
  ( SsmActionDefinition (..),

    -- * Smart constructor
    mkSsmActionDefinition,

    -- * Lenses
    sadActionSubType,
    sadRegion,
    sadInstanceIds,
  )
where

import Network.AWS.Budgets.Types.ActionSubType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The AWS Systems Manager (SSM) action definition details.
--
-- /See:/ 'mkSsmActionDefinition' smart constructor.
data SsmActionDefinition = SsmActionDefinition'
  { actionSubType ::
      ActionSubType,
    region :: Lude.Text,
    instanceIds :: Lude.NonEmpty Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SsmActionDefinition' with the minimum fields required to make a request.
--
-- * 'actionSubType' - The action subType.
-- * 'instanceIds' - The EC2 and RDS instance IDs.
-- * 'region' - The Region to run the SSM document.
mkSsmActionDefinition ::
  -- | 'actionSubType'
  ActionSubType ->
  -- | 'region'
  Lude.Text ->
  -- | 'instanceIds'
  Lude.NonEmpty Lude.Text ->
  SsmActionDefinition
mkSsmActionDefinition pActionSubType_ pRegion_ pInstanceIds_ =
  SsmActionDefinition'
    { actionSubType = pActionSubType_,
      region = pRegion_,
      instanceIds = pInstanceIds_
    }

-- | The action subType.
--
-- /Note:/ Consider using 'actionSubType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sadActionSubType :: Lens.Lens' SsmActionDefinition ActionSubType
sadActionSubType = Lens.lens (actionSubType :: SsmActionDefinition -> ActionSubType) (\s a -> s {actionSubType = a} :: SsmActionDefinition)
{-# DEPRECATED sadActionSubType "Use generic-lens or generic-optics with 'actionSubType' instead." #-}

-- | The Region to run the SSM document.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sadRegion :: Lens.Lens' SsmActionDefinition Lude.Text
sadRegion = Lens.lens (region :: SsmActionDefinition -> Lude.Text) (\s a -> s {region = a} :: SsmActionDefinition)
{-# DEPRECATED sadRegion "Use generic-lens or generic-optics with 'region' instead." #-}

-- | The EC2 and RDS instance IDs.
--
-- /Note:/ Consider using 'instanceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sadInstanceIds :: Lens.Lens' SsmActionDefinition (Lude.NonEmpty Lude.Text)
sadInstanceIds = Lens.lens (instanceIds :: SsmActionDefinition -> Lude.NonEmpty Lude.Text) (\s a -> s {instanceIds = a} :: SsmActionDefinition)
{-# DEPRECATED sadInstanceIds "Use generic-lens or generic-optics with 'instanceIds' instead." #-}

instance Lude.FromJSON SsmActionDefinition where
  parseJSON =
    Lude.withObject
      "SsmActionDefinition"
      ( \x ->
          SsmActionDefinition'
            Lude.<$> (x Lude..: "ActionSubType")
            Lude.<*> (x Lude..: "Region")
            Lude.<*> (x Lude..: "InstanceIds")
      )

instance Lude.ToJSON SsmActionDefinition where
  toJSON SsmActionDefinition' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ActionSubType" Lude..= actionSubType),
            Lude.Just ("Region" Lude..= region),
            Lude.Just ("InstanceIds" Lude..= instanceIds)
          ]
      )
