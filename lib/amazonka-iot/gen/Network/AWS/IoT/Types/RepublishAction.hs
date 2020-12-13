{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.RepublishAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.RepublishAction
  ( RepublishAction (..),

    -- * Smart constructor
    mkRepublishAction,

    -- * Lenses
    raTopic,
    raQos,
    raRoleARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an action to republish to another topic.
--
-- /See:/ 'mkRepublishAction' smart constructor.
data RepublishAction = RepublishAction'
  { -- | The name of the MQTT topic.
    topic :: Lude.Text,
    -- | The Quality of Service (QoS) level to use when republishing messages. The default value is 0.
    qos :: Lude.Maybe Lude.Natural,
    -- | The ARN of the IAM role that grants access.
    roleARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RepublishAction' with the minimum fields required to make a request.
--
-- * 'topic' - The name of the MQTT topic.
-- * 'qos' - The Quality of Service (QoS) level to use when republishing messages. The default value is 0.
-- * 'roleARN' - The ARN of the IAM role that grants access.
mkRepublishAction ::
  -- | 'topic'
  Lude.Text ->
  -- | 'roleARN'
  Lude.Text ->
  RepublishAction
mkRepublishAction pTopic_ pRoleARN_ =
  RepublishAction'
    { topic = pTopic_,
      qos = Lude.Nothing,
      roleARN = pRoleARN_
    }

-- | The name of the MQTT topic.
--
-- /Note:/ Consider using 'topic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raTopic :: Lens.Lens' RepublishAction Lude.Text
raTopic = Lens.lens (topic :: RepublishAction -> Lude.Text) (\s a -> s {topic = a} :: RepublishAction)
{-# DEPRECATED raTopic "Use generic-lens or generic-optics with 'topic' instead." #-}

-- | The Quality of Service (QoS) level to use when republishing messages. The default value is 0.
--
-- /Note:/ Consider using 'qos' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raQos :: Lens.Lens' RepublishAction (Lude.Maybe Lude.Natural)
raQos = Lens.lens (qos :: RepublishAction -> Lude.Maybe Lude.Natural) (\s a -> s {qos = a} :: RepublishAction)
{-# DEPRECATED raQos "Use generic-lens or generic-optics with 'qos' instead." #-}

-- | The ARN of the IAM role that grants access.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raRoleARN :: Lens.Lens' RepublishAction Lude.Text
raRoleARN = Lens.lens (roleARN :: RepublishAction -> Lude.Text) (\s a -> s {roleARN = a} :: RepublishAction)
{-# DEPRECATED raRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.FromJSON RepublishAction where
  parseJSON =
    Lude.withObject
      "RepublishAction"
      ( \x ->
          RepublishAction'
            Lude.<$> (x Lude..: "topic")
            Lude.<*> (x Lude..:? "qos")
            Lude.<*> (x Lude..: "roleArn")
      )

instance Lude.ToJSON RepublishAction where
  toJSON RepublishAction' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("topic" Lude..= topic),
            ("qos" Lude..=) Lude.<$> qos,
            Lude.Just ("roleArn" Lude..= roleARN)
          ]
      )
