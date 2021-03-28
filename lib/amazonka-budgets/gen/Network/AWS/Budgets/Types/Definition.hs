{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.Definition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Budgets.Types.Definition
  ( Definition (..)
  -- * Smart constructor
  , mkDefinition
  -- * Lenses
  , dIamActionDefinition
  , dScpActionDefinition
  , dSsmActionDefinition
  ) where

import qualified Network.AWS.Budgets.Types.IamActionDefinition as Types
import qualified Network.AWS.Budgets.Types.ScpActionDefinition as Types
import qualified Network.AWS.Budgets.Types.SsmActionDefinition as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies all of the type-specific parameters. 
--
-- /See:/ 'mkDefinition' smart constructor.
data Definition = Definition'
  { iamActionDefinition :: Core.Maybe Types.IamActionDefinition
    -- ^ The AWS Identity and Access Management (IAM) action definition details. 
  , scpActionDefinition :: Core.Maybe Types.ScpActionDefinition
    -- ^ The service control policies (SCPs) action definition details. 
  , ssmActionDefinition :: Core.Maybe Types.SsmActionDefinition
    -- ^ The AWS Systems Manager (SSM) action definition details. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Definition' value with any optional fields omitted.
mkDefinition
    :: Definition
mkDefinition
  = Definition'{iamActionDefinition = Core.Nothing,
                scpActionDefinition = Core.Nothing,
                ssmActionDefinition = Core.Nothing}

-- | The AWS Identity and Access Management (IAM) action definition details. 
--
-- /Note:/ Consider using 'iamActionDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dIamActionDefinition :: Lens.Lens' Definition (Core.Maybe Types.IamActionDefinition)
dIamActionDefinition = Lens.field @"iamActionDefinition"
{-# INLINEABLE dIamActionDefinition #-}
{-# DEPRECATED iamActionDefinition "Use generic-lens or generic-optics with 'iamActionDefinition' instead"  #-}

-- | The service control policies (SCPs) action definition details. 
--
-- /Note:/ Consider using 'scpActionDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dScpActionDefinition :: Lens.Lens' Definition (Core.Maybe Types.ScpActionDefinition)
dScpActionDefinition = Lens.field @"scpActionDefinition"
{-# INLINEABLE dScpActionDefinition #-}
{-# DEPRECATED scpActionDefinition "Use generic-lens or generic-optics with 'scpActionDefinition' instead"  #-}

-- | The AWS Systems Manager (SSM) action definition details. 
--
-- /Note:/ Consider using 'ssmActionDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dSsmActionDefinition :: Lens.Lens' Definition (Core.Maybe Types.SsmActionDefinition)
dSsmActionDefinition = Lens.field @"ssmActionDefinition"
{-# INLINEABLE dSsmActionDefinition #-}
{-# DEPRECATED ssmActionDefinition "Use generic-lens or generic-optics with 'ssmActionDefinition' instead"  #-}

instance Core.FromJSON Definition where
        toJSON Definition{..}
          = Core.object
              (Core.catMaybes
                 [("IamActionDefinition" Core..=) Core.<$> iamActionDefinition,
                  ("ScpActionDefinition" Core..=) Core.<$> scpActionDefinition,
                  ("SsmActionDefinition" Core..=) Core.<$> ssmActionDefinition])

instance Core.FromJSON Definition where
        parseJSON
          = Core.withObject "Definition" Core.$
              \ x ->
                Definition' Core.<$>
                  (x Core..:? "IamActionDefinition") Core.<*>
                    x Core..:? "ScpActionDefinition"
                    Core.<*> x Core..:? "SsmActionDefinition"
