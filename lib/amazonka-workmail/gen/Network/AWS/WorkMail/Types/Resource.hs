-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.Types.Resource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkMail.Types.Resource
  ( Resource (..),

    -- * Smart constructor
    mkResource,

    -- * Lenses
    rEmail,
    rState,
    rDisabledDate,
    rName,
    rId,
    rType,
    rEnabledDate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WorkMail.Types.EntityState
import Network.AWS.WorkMail.Types.ResourceType

-- | The representation of a resource.
--
-- /See:/ 'mkResource' smart constructor.
data Resource = Resource'
  { email :: Lude.Maybe Lude.Text,
    state :: Lude.Maybe EntityState,
    disabledDate :: Lude.Maybe Lude.Timestamp,
    name :: Lude.Maybe Lude.Text,
    id :: Lude.Maybe Lude.Text,
    type' :: Lude.Maybe ResourceType,
    enabledDate :: Lude.Maybe Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Resource' with the minimum fields required to make a request.
--
-- * 'disabledDate' - The date indicating when the resource was disabled from Amazon WorkMail use.
-- * 'email' - The email of the resource.
-- * 'enabledDate' - The date indicating when the resource was enabled for Amazon WorkMail use.
-- * 'id' - The identifier of the resource.
-- * 'name' - The name of the resource.
-- * 'state' - The state of the resource, which can be ENABLED, DISABLED, or DELETED.
-- * 'type'' - The type of the resource: equipment or room.
mkResource ::
  Resource
mkResource =
  Resource'
    { email = Lude.Nothing,
      state = Lude.Nothing,
      disabledDate = Lude.Nothing,
      name = Lude.Nothing,
      id = Lude.Nothing,
      type' = Lude.Nothing,
      enabledDate = Lude.Nothing
    }

-- | The email of the resource.
--
-- /Note:/ Consider using 'email' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rEmail :: Lens.Lens' Resource (Lude.Maybe Lude.Text)
rEmail = Lens.lens (email :: Resource -> Lude.Maybe Lude.Text) (\s a -> s {email = a} :: Resource)
{-# DEPRECATED rEmail "Use generic-lens or generic-optics with 'email' instead." #-}

-- | The state of the resource, which can be ENABLED, DISABLED, or DELETED.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rState :: Lens.Lens' Resource (Lude.Maybe EntityState)
rState = Lens.lens (state :: Resource -> Lude.Maybe EntityState) (\s a -> s {state = a} :: Resource)
{-# DEPRECATED rState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The date indicating when the resource was disabled from Amazon WorkMail use.
--
-- /Note:/ Consider using 'disabledDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rDisabledDate :: Lens.Lens' Resource (Lude.Maybe Lude.Timestamp)
rDisabledDate = Lens.lens (disabledDate :: Resource -> Lude.Maybe Lude.Timestamp) (\s a -> s {disabledDate = a} :: Resource)
{-# DEPRECATED rDisabledDate "Use generic-lens or generic-optics with 'disabledDate' instead." #-}

-- | The name of the resource.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rName :: Lens.Lens' Resource (Lude.Maybe Lude.Text)
rName = Lens.lens (name :: Resource -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Resource)
{-# DEPRECATED rName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The identifier of the resource.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rId :: Lens.Lens' Resource (Lude.Maybe Lude.Text)
rId = Lens.lens (id :: Resource -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: Resource)
{-# DEPRECATED rId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The type of the resource: equipment or room.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rType :: Lens.Lens' Resource (Lude.Maybe ResourceType)
rType = Lens.lens (type' :: Resource -> Lude.Maybe ResourceType) (\s a -> s {type' = a} :: Resource)
{-# DEPRECATED rType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The date indicating when the resource was enabled for Amazon WorkMail use.
--
-- /Note:/ Consider using 'enabledDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rEnabledDate :: Lens.Lens' Resource (Lude.Maybe Lude.Timestamp)
rEnabledDate = Lens.lens (enabledDate :: Resource -> Lude.Maybe Lude.Timestamp) (\s a -> s {enabledDate = a} :: Resource)
{-# DEPRECATED rEnabledDate "Use generic-lens or generic-optics with 'enabledDate' instead." #-}

instance Lude.FromJSON Resource where
  parseJSON =
    Lude.withObject
      "Resource"
      ( \x ->
          Resource'
            Lude.<$> (x Lude..:? "Email")
            Lude.<*> (x Lude..:? "State")
            Lude.<*> (x Lude..:? "DisabledDate")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Id")
            Lude.<*> (x Lude..:? "Type")
            Lude.<*> (x Lude..:? "EnabledDate")
      )
