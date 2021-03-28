{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.Child
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Organizations.Types.Child
  ( Child (..)
  -- * Smart constructor
  , mkChild
  -- * Lenses
  , cId
  , cType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Organizations.Types.ChildType as Types
import qualified Network.AWS.Organizations.Types.Id as Types
import qualified Network.AWS.Prelude as Core

-- | Contains a list of child entities, either OUs or accounts.
--
-- /See:/ 'mkChild' smart constructor.
data Child = Child'
  { id :: Core.Maybe Types.Id
    -- ^ The unique identifier (ID) of this child entity.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a child ID string requires one of the following:
--
--     * __Account__ - A string that consists of exactly 12 digits.
--
--
--     * __Organizational unit (OU)__ - A string that begins with "ou-" followed by from 4 to 32 lowercase letters or digits (the ID of the root that contains the OU). This string is followed by a second "-" dash and from 8 to 32 additional lowercase letters or digits.
--
--
  , type' :: Core.Maybe Types.ChildType
    -- ^ The type of this child entity.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Child' value with any optional fields omitted.
mkChild
    :: Child
mkChild = Child'{id = Core.Nothing, type' = Core.Nothing}

-- | The unique identifier (ID) of this child entity.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a child ID string requires one of the following:
--
--     * __Account__ - A string that consists of exactly 12 digits.
--
--
--     * __Organizational unit (OU)__ - A string that begins with "ou-" followed by from 4 to 32 lowercase letters or digits (the ID of the root that contains the OU). This string is followed by a second "-" dash and from 8 to 32 additional lowercase letters or digits.
--
--
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cId :: Lens.Lens' Child (Core.Maybe Types.Id)
cId = Lens.field @"id"
{-# INLINEABLE cId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The type of this child entity.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cType :: Lens.Lens' Child (Core.Maybe Types.ChildType)
cType = Lens.field @"type'"
{-# INLINEABLE cType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromJSON Child where
        parseJSON
          = Core.withObject "Child" Core.$
              \ x -> Child' Core.<$> (x Core..:? "Id") Core.<*> x Core..:? "Type"
