{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.Types.ParameterGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DAX.Types.ParameterGroup
  ( ParameterGroup (..),

    -- * Smart constructor
    mkParameterGroup,

    -- * Lenses
    pgDescription,
    pgParameterGroupName,
  )
where

import qualified Network.AWS.DAX.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A named set of parameters that are applied to all of the nodes in a DAX cluster.
--
-- /See:/ 'mkParameterGroup' smart constructor.
data ParameterGroup = ParameterGroup'
  { -- | A description of the parameter group.
    description :: Core.Maybe Types.String,
    -- | The name of the parameter group.
    parameterGroupName :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ParameterGroup' value with any optional fields omitted.
mkParameterGroup ::
  ParameterGroup
mkParameterGroup =
  ParameterGroup'
    { description = Core.Nothing,
      parameterGroupName = Core.Nothing
    }

-- | A description of the parameter group.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgDescription :: Lens.Lens' ParameterGroup (Core.Maybe Types.String)
pgDescription = Lens.field @"description"
{-# DEPRECATED pgDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The name of the parameter group.
--
-- /Note:/ Consider using 'parameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgParameterGroupName :: Lens.Lens' ParameterGroup (Core.Maybe Types.String)
pgParameterGroupName = Lens.field @"parameterGroupName"
{-# DEPRECATED pgParameterGroupName "Use generic-lens or generic-optics with 'parameterGroupName' instead." #-}

instance Core.FromJSON ParameterGroup where
  parseJSON =
    Core.withObject "ParameterGroup" Core.$
      \x ->
        ParameterGroup'
          Core.<$> (x Core..:? "Description")
          Core.<*> (x Core..:? "ParameterGroupName")
