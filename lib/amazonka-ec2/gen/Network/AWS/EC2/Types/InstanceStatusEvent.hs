{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceStatusEvent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceStatusEvent
  ( InstanceStatusEvent (..),

    -- * Smart constructor
    mkInstanceStatusEvent,

    -- * Lenses
    iseNotBefore,
    iseCode,
    iseInstanceEventId,
    iseDescription,
    iseNotBeforeDeadline,
    iseNotAfter,
  )
where

import Network.AWS.EC2.Types.EventCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a scheduled event for an instance.
--
-- /See:/ 'mkInstanceStatusEvent' smart constructor.
data InstanceStatusEvent = InstanceStatusEvent'
  { notBefore ::
      Lude.Maybe Lude.DateTime,
    code :: Lude.Maybe EventCode,
    instanceEventId :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    notBeforeDeadline :: Lude.Maybe Lude.DateTime,
    notAfter :: Lude.Maybe Lude.DateTime
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceStatusEvent' with the minimum fields required to make a request.
--
-- * 'code' - The event code.
-- * 'description' - A description of the event.
--
-- After a scheduled event is completed, it can still be described for up to a week. If the event has been completed, this description starts with the following text: [Completed].
-- * 'instanceEventId' - The ID of the event.
-- * 'notAfter' - The latest scheduled end time for the event.
-- * 'notBefore' - The earliest scheduled start time for the event.
-- * 'notBeforeDeadline' - The deadline for starting the event.
mkInstanceStatusEvent ::
  InstanceStatusEvent
mkInstanceStatusEvent =
  InstanceStatusEvent'
    { notBefore = Lude.Nothing,
      code = Lude.Nothing,
      instanceEventId = Lude.Nothing,
      description = Lude.Nothing,
      notBeforeDeadline = Lude.Nothing,
      notAfter = Lude.Nothing
    }

-- | The earliest scheduled start time for the event.
--
-- /Note:/ Consider using 'notBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iseNotBefore :: Lens.Lens' InstanceStatusEvent (Lude.Maybe Lude.DateTime)
iseNotBefore = Lens.lens (notBefore :: InstanceStatusEvent -> Lude.Maybe Lude.DateTime) (\s a -> s {notBefore = a} :: InstanceStatusEvent)
{-# DEPRECATED iseNotBefore "Use generic-lens or generic-optics with 'notBefore' instead." #-}

-- | The event code.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iseCode :: Lens.Lens' InstanceStatusEvent (Lude.Maybe EventCode)
iseCode = Lens.lens (code :: InstanceStatusEvent -> Lude.Maybe EventCode) (\s a -> s {code = a} :: InstanceStatusEvent)
{-# DEPRECATED iseCode "Use generic-lens or generic-optics with 'code' instead." #-}

-- | The ID of the event.
--
-- /Note:/ Consider using 'instanceEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iseInstanceEventId :: Lens.Lens' InstanceStatusEvent (Lude.Maybe Lude.Text)
iseInstanceEventId = Lens.lens (instanceEventId :: InstanceStatusEvent -> Lude.Maybe Lude.Text) (\s a -> s {instanceEventId = a} :: InstanceStatusEvent)
{-# DEPRECATED iseInstanceEventId "Use generic-lens or generic-optics with 'instanceEventId' instead." #-}

-- | A description of the event.
--
-- After a scheduled event is completed, it can still be described for up to a week. If the event has been completed, this description starts with the following text: [Completed].
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iseDescription :: Lens.Lens' InstanceStatusEvent (Lude.Maybe Lude.Text)
iseDescription = Lens.lens (description :: InstanceStatusEvent -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: InstanceStatusEvent)
{-# DEPRECATED iseDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The deadline for starting the event.
--
-- /Note:/ Consider using 'notBeforeDeadline' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iseNotBeforeDeadline :: Lens.Lens' InstanceStatusEvent (Lude.Maybe Lude.DateTime)
iseNotBeforeDeadline = Lens.lens (notBeforeDeadline :: InstanceStatusEvent -> Lude.Maybe Lude.DateTime) (\s a -> s {notBeforeDeadline = a} :: InstanceStatusEvent)
{-# DEPRECATED iseNotBeforeDeadline "Use generic-lens or generic-optics with 'notBeforeDeadline' instead." #-}

-- | The latest scheduled end time for the event.
--
-- /Note:/ Consider using 'notAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iseNotAfter :: Lens.Lens' InstanceStatusEvent (Lude.Maybe Lude.DateTime)
iseNotAfter = Lens.lens (notAfter :: InstanceStatusEvent -> Lude.Maybe Lude.DateTime) (\s a -> s {notAfter = a} :: InstanceStatusEvent)
{-# DEPRECATED iseNotAfter "Use generic-lens or generic-optics with 'notAfter' instead." #-}

instance Lude.FromXML InstanceStatusEvent where
  parseXML x =
    InstanceStatusEvent'
      Lude.<$> (x Lude..@? "notBefore")
      Lude.<*> (x Lude..@? "code")
      Lude.<*> (x Lude..@? "instanceEventId")
      Lude.<*> (x Lude..@? "description")
      Lude.<*> (x Lude..@? "notBeforeDeadline")
      Lude.<*> (x Lude..@? "notAfter")
