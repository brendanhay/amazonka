{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.Types.Group
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkMail.Types.Group
  ( Group (..),

    -- * Smart constructor
    mkGroup,

    -- * Lenses
    gEmail,
    gState,
    gDisabledDate,
    gName,
    gId,
    gEnabledDate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WorkMail.Types.EntityState

-- | The representation of an Amazon WorkMail group.
--
-- /See:/ 'mkGroup' smart constructor.
data Group = Group'
  { -- | The email of the group.
    email :: Lude.Maybe Lude.Text,
    -- | The state of the group, which can be ENABLED, DISABLED, or DELETED.
    state :: Lude.Maybe EntityState,
    -- | The date indicating when the group was disabled from Amazon WorkMail use.
    disabledDate :: Lude.Maybe Lude.Timestamp,
    -- | The name of the group.
    name :: Lude.Maybe Lude.Text,
    -- | The identifier of the group.
    id :: Lude.Maybe Lude.Text,
    -- | The date indicating when the group was enabled for Amazon WorkMail use.
    enabledDate :: Lude.Maybe Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Group' with the minimum fields required to make a request.
--
-- * 'email' - The email of the group.
-- * 'state' - The state of the group, which can be ENABLED, DISABLED, or DELETED.
-- * 'disabledDate' - The date indicating when the group was disabled from Amazon WorkMail use.
-- * 'name' - The name of the group.
-- * 'id' - The identifier of the group.
-- * 'enabledDate' - The date indicating when the group was enabled for Amazon WorkMail use.
mkGroup ::
  Group
mkGroup =
  Group'
    { email = Lude.Nothing,
      state = Lude.Nothing,
      disabledDate = Lude.Nothing,
      name = Lude.Nothing,
      id = Lude.Nothing,
      enabledDate = Lude.Nothing
    }

-- | The email of the group.
--
-- /Note:/ Consider using 'email' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gEmail :: Lens.Lens' Group (Lude.Maybe Lude.Text)
gEmail = Lens.lens (email :: Group -> Lude.Maybe Lude.Text) (\s a -> s {email = a} :: Group)
{-# DEPRECATED gEmail "Use generic-lens or generic-optics with 'email' instead." #-}

-- | The state of the group, which can be ENABLED, DISABLED, or DELETED.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gState :: Lens.Lens' Group (Lude.Maybe EntityState)
gState = Lens.lens (state :: Group -> Lude.Maybe EntityState) (\s a -> s {state = a} :: Group)
{-# DEPRECATED gState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The date indicating when the group was disabled from Amazon WorkMail use.
--
-- /Note:/ Consider using 'disabledDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gDisabledDate :: Lens.Lens' Group (Lude.Maybe Lude.Timestamp)
gDisabledDate = Lens.lens (disabledDate :: Group -> Lude.Maybe Lude.Timestamp) (\s a -> s {disabledDate = a} :: Group)
{-# DEPRECATED gDisabledDate "Use generic-lens or generic-optics with 'disabledDate' instead." #-}

-- | The name of the group.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gName :: Lens.Lens' Group (Lude.Maybe Lude.Text)
gName = Lens.lens (name :: Group -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Group)
{-# DEPRECATED gName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The identifier of the group.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gId :: Lens.Lens' Group (Lude.Maybe Lude.Text)
gId = Lens.lens (id :: Group -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: Group)
{-# DEPRECATED gId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The date indicating when the group was enabled for Amazon WorkMail use.
--
-- /Note:/ Consider using 'enabledDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gEnabledDate :: Lens.Lens' Group (Lude.Maybe Lude.Timestamp)
gEnabledDate = Lens.lens (enabledDate :: Group -> Lude.Maybe Lude.Timestamp) (\s a -> s {enabledDate = a} :: Group)
{-# DEPRECATED gEnabledDate "Use generic-lens or generic-optics with 'enabledDate' instead." #-}

instance Lude.FromJSON Group where
  parseJSON =
    Lude.withObject
      "Group"
      ( \x ->
          Group'
            Lude.<$> (x Lude..:? "Email")
            Lude.<*> (x Lude..:? "State")
            Lude.<*> (x Lude..:? "DisabledDate")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Id")
            Lude.<*> (x Lude..:? "EnabledDate")
      )
