{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.Child
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.Child
  ( Child (..),

    -- * Smart constructor
    mkChild,

    -- * Lenses
    cId,
    cType,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types.ChildType
import qualified Network.AWS.Prelude as Lude

-- | Contains a list of child entities, either OUs or accounts.
--
-- /See:/ 'mkChild' smart constructor.
data Child = Child'
  { -- | The unique identifier (ID) of this child entity.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> for a child ID string requires one of the following:
    --
    --     * __Account__ - A string that consists of exactly 12 digits.
    --
    --
    --     * __Organizational unit (OU)__ - A string that begins with "ou-" followed by from 4 to 32 lowercase letters or digits (the ID of the root that contains the OU). This string is followed by a second "-" dash and from 8 to 32 additional lowercase letters or digits.
    id :: Lude.Maybe Lude.Text,
    -- | The type of this child entity.
    type' :: Lude.Maybe ChildType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Child' with the minimum fields required to make a request.
--
-- * 'id' - The unique identifier (ID) of this child entity.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a child ID string requires one of the following:
--
--     * __Account__ - A string that consists of exactly 12 digits.
--
--
--     * __Organizational unit (OU)__ - A string that begins with "ou-" followed by from 4 to 32 lowercase letters or digits (the ID of the root that contains the OU). This string is followed by a second "-" dash and from 8 to 32 additional lowercase letters or digits.
--
--
-- * 'type'' - The type of this child entity.
mkChild ::
  Child
mkChild = Child' {id = Lude.Nothing, type' = Lude.Nothing}

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
cId :: Lens.Lens' Child (Lude.Maybe Lude.Text)
cId = Lens.lens (id :: Child -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: Child)
{-# DEPRECATED cId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The type of this child entity.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cType :: Lens.Lens' Child (Lude.Maybe ChildType)
cType = Lens.lens (type' :: Child -> Lude.Maybe ChildType) (\s a -> s {type' = a} :: Child)
{-# DEPRECATED cType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON Child where
  parseJSON =
    Lude.withObject
      "Child"
      ( \x ->
          Child' Lude.<$> (x Lude..:? "Id") Lude.<*> (x Lude..:? "Type")
      )
