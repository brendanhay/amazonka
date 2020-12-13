{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.DeviceShadowEnrichActivity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.DeviceShadowEnrichActivity
  ( DeviceShadowEnrichActivity (..),

    -- * Smart constructor
    mkDeviceShadowEnrichActivity,

    -- * Lenses
    dseaAttribute,
    dseaNext,
    dseaName,
    dseaThingName,
    dseaRoleARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An activity that adds information from the AWS IoT Device Shadow service to a message.
--
-- /See:/ 'mkDeviceShadowEnrichActivity' smart constructor.
data DeviceShadowEnrichActivity = DeviceShadowEnrichActivity'
  { -- | The name of the attribute that is added to the message.
    attribute :: Lude.Text,
    -- | The next activity in the pipeline.
    next :: Lude.Maybe Lude.Text,
    -- | The name of the @deviceShadowEnrich@ activity.
    name :: Lude.Text,
    -- | The name of the IoT device whose shadow information is added to the message.
    thingName :: Lude.Text,
    -- | The ARN of the role that allows access to the device's shadow.
    roleARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeviceShadowEnrichActivity' with the minimum fields required to make a request.
--
-- * 'attribute' - The name of the attribute that is added to the message.
-- * 'next' - The next activity in the pipeline.
-- * 'name' - The name of the @deviceShadowEnrich@ activity.
-- * 'thingName' - The name of the IoT device whose shadow information is added to the message.
-- * 'roleARN' - The ARN of the role that allows access to the device's shadow.
mkDeviceShadowEnrichActivity ::
  -- | 'attribute'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  -- | 'thingName'
  Lude.Text ->
  -- | 'roleARN'
  Lude.Text ->
  DeviceShadowEnrichActivity
mkDeviceShadowEnrichActivity
  pAttribute_
  pName_
  pThingName_
  pRoleARN_ =
    DeviceShadowEnrichActivity'
      { attribute = pAttribute_,
        next = Lude.Nothing,
        name = pName_,
        thingName = pThingName_,
        roleARN = pRoleARN_
      }

-- | The name of the attribute that is added to the message.
--
-- /Note:/ Consider using 'attribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dseaAttribute :: Lens.Lens' DeviceShadowEnrichActivity Lude.Text
dseaAttribute = Lens.lens (attribute :: DeviceShadowEnrichActivity -> Lude.Text) (\s a -> s {attribute = a} :: DeviceShadowEnrichActivity)
{-# DEPRECATED dseaAttribute "Use generic-lens or generic-optics with 'attribute' instead." #-}

-- | The next activity in the pipeline.
--
-- /Note:/ Consider using 'next' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dseaNext :: Lens.Lens' DeviceShadowEnrichActivity (Lude.Maybe Lude.Text)
dseaNext = Lens.lens (next :: DeviceShadowEnrichActivity -> Lude.Maybe Lude.Text) (\s a -> s {next = a} :: DeviceShadowEnrichActivity)
{-# DEPRECATED dseaNext "Use generic-lens or generic-optics with 'next' instead." #-}

-- | The name of the @deviceShadowEnrich@ activity.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dseaName :: Lens.Lens' DeviceShadowEnrichActivity Lude.Text
dseaName = Lens.lens (name :: DeviceShadowEnrichActivity -> Lude.Text) (\s a -> s {name = a} :: DeviceShadowEnrichActivity)
{-# DEPRECATED dseaName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The name of the IoT device whose shadow information is added to the message.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dseaThingName :: Lens.Lens' DeviceShadowEnrichActivity Lude.Text
dseaThingName = Lens.lens (thingName :: DeviceShadowEnrichActivity -> Lude.Text) (\s a -> s {thingName = a} :: DeviceShadowEnrichActivity)
{-# DEPRECATED dseaThingName "Use generic-lens or generic-optics with 'thingName' instead." #-}

-- | The ARN of the role that allows access to the device's shadow.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dseaRoleARN :: Lens.Lens' DeviceShadowEnrichActivity Lude.Text
dseaRoleARN = Lens.lens (roleARN :: DeviceShadowEnrichActivity -> Lude.Text) (\s a -> s {roleARN = a} :: DeviceShadowEnrichActivity)
{-# DEPRECATED dseaRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.FromJSON DeviceShadowEnrichActivity where
  parseJSON =
    Lude.withObject
      "DeviceShadowEnrichActivity"
      ( \x ->
          DeviceShadowEnrichActivity'
            Lude.<$> (x Lude..: "attribute")
            Lude.<*> (x Lude..:? "next")
            Lude.<*> (x Lude..: "name")
            Lude.<*> (x Lude..: "thingName")
            Lude.<*> (x Lude..: "roleArn")
      )

instance Lude.ToJSON DeviceShadowEnrichActivity where
  toJSON DeviceShadowEnrichActivity' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("attribute" Lude..= attribute),
            ("next" Lude..=) Lude.<$> next,
            Lude.Just ("name" Lude..= name),
            Lude.Just ("thingName" Lude..= thingName),
            Lude.Just ("roleArn" Lude..= roleARN)
          ]
      )
