{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.WorkflowType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SWF.Types.WorkflowType
  ( WorkflowType (..)
  -- * Smart constructor
  , mkWorkflowType
  -- * Lenses
  , wtName
  , wtVersion
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SWF.Types.Name as Types
import qualified Network.AWS.SWF.Types.Version as Types

-- | Represents a workflow type.
--
-- /See:/ 'mkWorkflowType' smart constructor.
data WorkflowType = WorkflowType'
  { name :: Types.Name
    -- ^ The name of the workflow type.
  , version :: Types.Version
    -- ^ The version of the workflow type.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'WorkflowType' value with any optional fields omitted.
mkWorkflowType
    :: Types.Name -- ^ 'name'
    -> Types.Version -- ^ 'version'
    -> WorkflowType
mkWorkflowType name version = WorkflowType'{name, version}

-- | The name of the workflow type.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wtName :: Lens.Lens' WorkflowType Types.Name
wtName = Lens.field @"name"
{-# INLINEABLE wtName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The version of the workflow type.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wtVersion :: Lens.Lens' WorkflowType Types.Version
wtVersion = Lens.field @"version"
{-# INLINEABLE wtVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

instance Core.FromJSON WorkflowType where
        toJSON WorkflowType{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("name" Core..= name),
                  Core.Just ("version" Core..= version)])

instance Core.FromJSON WorkflowType where
        parseJSON
          = Core.withObject "WorkflowType" Core.$
              \ x ->
                WorkflowType' Core.<$>
                  (x Core..: "name") Core.<*> x Core..: "version"
