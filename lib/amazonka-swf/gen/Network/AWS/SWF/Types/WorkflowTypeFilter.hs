{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.WorkflowTypeFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SWF.Types.WorkflowTypeFilter
  ( WorkflowTypeFilter (..)
  -- * Smart constructor
  , mkWorkflowTypeFilter
  -- * Lenses
  , wtfName
  , wtfVersion
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SWF.Types.Name as Types
import qualified Network.AWS.SWF.Types.VersionOptional as Types

-- | Used to filter workflow execution query results by type. Each parameter, if specified, defines a rule that must be satisfied by each returned result.
--
-- /See:/ 'mkWorkflowTypeFilter' smart constructor.
data WorkflowTypeFilter = WorkflowTypeFilter'
  { name :: Types.Name
    -- ^ Name of the workflow type.
  , version :: Core.Maybe Types.VersionOptional
    -- ^ Version of the workflow type.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'WorkflowTypeFilter' value with any optional fields omitted.
mkWorkflowTypeFilter
    :: Types.Name -- ^ 'name'
    -> WorkflowTypeFilter
mkWorkflowTypeFilter name
  = WorkflowTypeFilter'{name, version = Core.Nothing}

-- | Name of the workflow type.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wtfName :: Lens.Lens' WorkflowTypeFilter Types.Name
wtfName = Lens.field @"name"
{-# INLINEABLE wtfName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Version of the workflow type.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wtfVersion :: Lens.Lens' WorkflowTypeFilter (Core.Maybe Types.VersionOptional)
wtfVersion = Lens.field @"version"
{-# INLINEABLE wtfVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

instance Core.FromJSON WorkflowTypeFilter where
        toJSON WorkflowTypeFilter{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("name" Core..= name),
                  ("version" Core..=) Core.<$> version])
