{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VolumeStatusAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VolumeStatusAction
  ( VolumeStatusAction (..),

    -- * Smart constructor
    mkVolumeStatusAction,

    -- * Lenses
    vsaEventType,
    vsaCode,
    vsaDescription,
    vsaEventId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a volume status operation code.
--
-- /See:/ 'mkVolumeStatusAction' smart constructor.
data VolumeStatusAction = VolumeStatusAction'
  { -- | The event type associated with this operation.
    eventType :: Lude.Maybe Lude.Text,
    -- | The code identifying the operation, for example, @enable-volume-io@ .
    code :: Lude.Maybe Lude.Text,
    -- | A description of the operation.
    description :: Lude.Maybe Lude.Text,
    -- | The ID of the event associated with this operation.
    eventId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VolumeStatusAction' with the minimum fields required to make a request.
--
-- * 'eventType' - The event type associated with this operation.
-- * 'code' - The code identifying the operation, for example, @enable-volume-io@ .
-- * 'description' - A description of the operation.
-- * 'eventId' - The ID of the event associated with this operation.
mkVolumeStatusAction ::
  VolumeStatusAction
mkVolumeStatusAction =
  VolumeStatusAction'
    { eventType = Lude.Nothing,
      code = Lude.Nothing,
      description = Lude.Nothing,
      eventId = Lude.Nothing
    }

-- | The event type associated with this operation.
--
-- /Note:/ Consider using 'eventType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsaEventType :: Lens.Lens' VolumeStatusAction (Lude.Maybe Lude.Text)
vsaEventType = Lens.lens (eventType :: VolumeStatusAction -> Lude.Maybe Lude.Text) (\s a -> s {eventType = a} :: VolumeStatusAction)
{-# DEPRECATED vsaEventType "Use generic-lens or generic-optics with 'eventType' instead." #-}

-- | The code identifying the operation, for example, @enable-volume-io@ .
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsaCode :: Lens.Lens' VolumeStatusAction (Lude.Maybe Lude.Text)
vsaCode = Lens.lens (code :: VolumeStatusAction -> Lude.Maybe Lude.Text) (\s a -> s {code = a} :: VolumeStatusAction)
{-# DEPRECATED vsaCode "Use generic-lens or generic-optics with 'code' instead." #-}

-- | A description of the operation.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsaDescription :: Lens.Lens' VolumeStatusAction (Lude.Maybe Lude.Text)
vsaDescription = Lens.lens (description :: VolumeStatusAction -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: VolumeStatusAction)
{-# DEPRECATED vsaDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The ID of the event associated with this operation.
--
-- /Note:/ Consider using 'eventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsaEventId :: Lens.Lens' VolumeStatusAction (Lude.Maybe Lude.Text)
vsaEventId = Lens.lens (eventId :: VolumeStatusAction -> Lude.Maybe Lude.Text) (\s a -> s {eventId = a} :: VolumeStatusAction)
{-# DEPRECATED vsaEventId "Use generic-lens or generic-optics with 'eventId' instead." #-}

instance Lude.FromXML VolumeStatusAction where
  parseXML x =
    VolumeStatusAction'
      Lude.<$> (x Lude..@? "eventType")
      Lude.<*> (x Lude..@? "code")
      Lude.<*> (x Lude..@? "description")
      Lude.<*> (x Lude..@? "eventId")
