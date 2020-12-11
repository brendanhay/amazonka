-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.Parent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.Parent
  ( Parent (..),

    -- * Smart constructor
    mkParent,

    -- * Lenses
    pId,
    pType,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types.ParentType
import qualified Network.AWS.Prelude as Lude

-- | Contains information about either a root or an organizational unit (OU) that can contain OUs or accounts in an organization.
--
-- /See:/ 'mkParent' smart constructor.
data Parent = Parent'
  { id :: Lude.Maybe Lude.Text,
    type' :: Lude.Maybe ParentType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Parent' with the minimum fields required to make a request.
--
-- * 'id' - The unique identifier (ID) of the parent entity.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a parent ID string requires one of the following:
--
--     * __Root__ - A string that begins with "r-" followed by from 4 to 32 lowercase letters or digits.
--
--
--     * __Organizational unit (OU)__ - A string that begins with "ou-" followed by from 4 to 32 lowercase letters or digits (the ID of the root that the OU is in). This string is followed by a second "-" dash and from 8 to 32 additional lowercase letters or digits.
--
--
-- * 'type'' - The type of the parent entity.
mkParent ::
  Parent
mkParent = Parent' {id = Lude.Nothing, type' = Lude.Nothing}

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
pId :: Lens.Lens' Parent (Lude.Maybe Lude.Text)
pId = Lens.lens (id :: Parent -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: Parent)
{-# DEPRECATED pId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The type of the parent entity.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pType :: Lens.Lens' Parent (Lude.Maybe ParentType)
pType = Lens.lens (type' :: Parent -> Lude.Maybe ParentType) (\s a -> s {type' = a} :: Parent)
{-# DEPRECATED pType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON Parent where
  parseJSON =
    Lude.withObject
      "Parent"
      ( \x ->
          Parent' Lude.<$> (x Lude..:? "Id") Lude.<*> (x Lude..:? "Type")
      )
