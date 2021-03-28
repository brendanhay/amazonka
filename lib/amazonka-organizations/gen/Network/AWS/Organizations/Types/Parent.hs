{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.Parent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Organizations.Types.Parent
  ( Parent (..)
  -- * Smart constructor
  , mkParent
  -- * Lenses
  , pId
  , pType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Organizations.Types.Id as Types
import qualified Network.AWS.Organizations.Types.ParentType as Types
import qualified Network.AWS.Prelude as Core

-- | Contains information about either a root or an organizational unit (OU) that can contain OUs or accounts in an organization.
--
-- /See:/ 'mkParent' smart constructor.
data Parent = Parent'
  { id :: Core.Maybe Types.Id
    -- ^ The unique identifier (ID) of the parent entity.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a parent ID string requires one of the following:
--
--     * __Root__ - A string that begins with "r-" followed by from 4 to 32 lowercase letters or digits.
--
--
--     * __Organizational unit (OU)__ - A string that begins with "ou-" followed by from 4 to 32 lowercase letters or digits (the ID of the root that the OU is in). This string is followed by a second "-" dash and from 8 to 32 additional lowercase letters or digits.
--
--
  , type' :: Core.Maybe Types.ParentType
    -- ^ The type of the parent entity.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Parent' value with any optional fields omitted.
mkParent
    :: Parent
mkParent = Parent'{id = Core.Nothing, type' = Core.Nothing}

-- | The unique identifier (ID) of the parent entity.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a parent ID string requires one of the following:
--
--     * __Root__ - A string that begins with "r-" followed by from 4 to 32 lowercase letters or digits.
--
--
--     * __Organizational unit (OU)__ - A string that begins with "ou-" followed by from 4 to 32 lowercase letters or digits (the ID of the root that the OU is in). This string is followed by a second "-" dash and from 8 to 32 additional lowercase letters or digits.
--
--
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pId :: Lens.Lens' Parent (Core.Maybe Types.Id)
pId = Lens.field @"id"
{-# INLINEABLE pId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The type of the parent entity.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pType :: Lens.Lens' Parent (Core.Maybe Types.ParentType)
pType = Lens.field @"type'"
{-# INLINEABLE pType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromJSON Parent where
        parseJSON
          = Core.withObject "Parent" Core.$
              \ x ->
                Parent' Core.<$> (x Core..:? "Id") Core.<*> x Core..:? "Type"
