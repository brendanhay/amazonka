{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.Change
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.Change
  ( Change (..),

    -- * Smart constructor
    mkChange,

    -- * Lenses
    cResourceChange,
    cType,
  )
where

import qualified Network.AWS.CloudFormation.Types.ChangeType as Types
import qualified Network.AWS.CloudFormation.Types.ResourceChange as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The @Change@ structure describes the changes AWS CloudFormation will perform if you execute the change set.
--
-- /See:/ 'mkChange' smart constructor.
data Change = Change'
  { -- | A @ResourceChange@ structure that describes the resource and action that AWS CloudFormation will perform.
    resourceChange :: Core.Maybe Types.ResourceChange,
    -- | The type of entity that AWS CloudFormation changes. Currently, the only entity type is @Resource@ .
    type' :: Core.Maybe Types.ChangeType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Change' value with any optional fields omitted.
mkChange ::
  Change
mkChange =
  Change' {resourceChange = Core.Nothing, type' = Core.Nothing}

-- | A @ResourceChange@ structure that describes the resource and action that AWS CloudFormation will perform.
--
-- /Note:/ Consider using 'resourceChange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cResourceChange :: Lens.Lens' Change (Core.Maybe Types.ResourceChange)
cResourceChange = Lens.field @"resourceChange"
{-# DEPRECATED cResourceChange "Use generic-lens or generic-optics with 'resourceChange' instead." #-}

-- | The type of entity that AWS CloudFormation changes. Currently, the only entity type is @Resource@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cType :: Lens.Lens' Change (Core.Maybe Types.ChangeType)
cType = Lens.field @"type'"
{-# DEPRECATED cType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromXML Change where
  parseXML x =
    Change'
      Core.<$> (x Core..@? "ResourceChange") Core.<*> (x Core..@? "Type")
