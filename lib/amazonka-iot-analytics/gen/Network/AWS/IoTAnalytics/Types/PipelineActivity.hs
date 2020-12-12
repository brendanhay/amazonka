{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.PipelineActivity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.PipelineActivity
  ( PipelineActivity (..),

    -- * Smart constructor
    mkPipelineActivity,

    -- * Lenses
    paSelectAttributes,
    paChannel,
    paAddAttributes,
    paDeviceRegistryEnrich,
    paRemoveAttributes,
    paLambda,
    paDatastore,
    paDeviceShadowEnrich,
    paFilter,
    paMath,
  )
where

import Network.AWS.IoTAnalytics.Types.AddAttributesActivity
import Network.AWS.IoTAnalytics.Types.ChannelActivity
import Network.AWS.IoTAnalytics.Types.DatastoreActivity
import Network.AWS.IoTAnalytics.Types.DeviceRegistryEnrichActivity
import Network.AWS.IoTAnalytics.Types.DeviceShadowEnrichActivity
import Network.AWS.IoTAnalytics.Types.FilterActivity
import Network.AWS.IoTAnalytics.Types.LambdaActivity
import Network.AWS.IoTAnalytics.Types.MathActivity
import Network.AWS.IoTAnalytics.Types.RemoveAttributesActivity
import Network.AWS.IoTAnalytics.Types.SelectAttributesActivity
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An activity that performs a transformation on a message.
--
-- /See:/ 'mkPipelineActivity' smart constructor.
data PipelineActivity = PipelineActivity'
  { selectAttributes ::
      Lude.Maybe SelectAttributesActivity,
    channel :: Lude.Maybe ChannelActivity,
    addAttributes :: Lude.Maybe AddAttributesActivity,
    deviceRegistryEnrich ::
      Lude.Maybe DeviceRegistryEnrichActivity,
    removeAttributes :: Lude.Maybe RemoveAttributesActivity,
    lambda :: Lude.Maybe LambdaActivity,
    datastore :: Lude.Maybe DatastoreActivity,
    deviceShadowEnrich ::
      Lude.Maybe DeviceShadowEnrichActivity,
    filter :: Lude.Maybe FilterActivity,
    math :: Lude.Maybe MathActivity
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PipelineActivity' with the minimum fields required to make a request.
--
-- * 'addAttributes' - Adds other attributes based on existing attributes in the message.
-- * 'channel' - Determines the source of the messages to be processed.
-- * 'datastore' - Specifies where to store the processed message data.
-- * 'deviceRegistryEnrich' - Adds data from the AWS IoT device registry to your message.
-- * 'deviceShadowEnrich' - Adds information from the AWS IoT Device Shadow service to a message.
-- * 'filter' - Filters a message based on its attributes.
-- * 'lambda' - Runs a Lambda function to modify the message.
-- * 'math' - Computes an arithmetic expression using the message's attributes and adds it to the message.
-- * 'removeAttributes' - Removes attributes from a message.
-- * 'selectAttributes' - Creates a new message using only the specified attributes from the original message.
mkPipelineActivity ::
  PipelineActivity
mkPipelineActivity =
  PipelineActivity'
    { selectAttributes = Lude.Nothing,
      channel = Lude.Nothing,
      addAttributes = Lude.Nothing,
      deviceRegistryEnrich = Lude.Nothing,
      removeAttributes = Lude.Nothing,
      lambda = Lude.Nothing,
      datastore = Lude.Nothing,
      deviceShadowEnrich = Lude.Nothing,
      filter = Lude.Nothing,
      math = Lude.Nothing
    }

-- | Creates a new message using only the specified attributes from the original message.
--
-- /Note:/ Consider using 'selectAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paSelectAttributes :: Lens.Lens' PipelineActivity (Lude.Maybe SelectAttributesActivity)
paSelectAttributes = Lens.lens (selectAttributes :: PipelineActivity -> Lude.Maybe SelectAttributesActivity) (\s a -> s {selectAttributes = a} :: PipelineActivity)
{-# DEPRECATED paSelectAttributes "Use generic-lens or generic-optics with 'selectAttributes' instead." #-}

-- | Determines the source of the messages to be processed.
--
-- /Note:/ Consider using 'channel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paChannel :: Lens.Lens' PipelineActivity (Lude.Maybe ChannelActivity)
paChannel = Lens.lens (channel :: PipelineActivity -> Lude.Maybe ChannelActivity) (\s a -> s {channel = a} :: PipelineActivity)
{-# DEPRECATED paChannel "Use generic-lens or generic-optics with 'channel' instead." #-}

-- | Adds other attributes based on existing attributes in the message.
--
-- /Note:/ Consider using 'addAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paAddAttributes :: Lens.Lens' PipelineActivity (Lude.Maybe AddAttributesActivity)
paAddAttributes = Lens.lens (addAttributes :: PipelineActivity -> Lude.Maybe AddAttributesActivity) (\s a -> s {addAttributes = a} :: PipelineActivity)
{-# DEPRECATED paAddAttributes "Use generic-lens or generic-optics with 'addAttributes' instead." #-}

-- | Adds data from the AWS IoT device registry to your message.
--
-- /Note:/ Consider using 'deviceRegistryEnrich' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paDeviceRegistryEnrich :: Lens.Lens' PipelineActivity (Lude.Maybe DeviceRegistryEnrichActivity)
paDeviceRegistryEnrich = Lens.lens (deviceRegistryEnrich :: PipelineActivity -> Lude.Maybe DeviceRegistryEnrichActivity) (\s a -> s {deviceRegistryEnrich = a} :: PipelineActivity)
{-# DEPRECATED paDeviceRegistryEnrich "Use generic-lens or generic-optics with 'deviceRegistryEnrich' instead." #-}

-- | Removes attributes from a message.
--
-- /Note:/ Consider using 'removeAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paRemoveAttributes :: Lens.Lens' PipelineActivity (Lude.Maybe RemoveAttributesActivity)
paRemoveAttributes = Lens.lens (removeAttributes :: PipelineActivity -> Lude.Maybe RemoveAttributesActivity) (\s a -> s {removeAttributes = a} :: PipelineActivity)
{-# DEPRECATED paRemoveAttributes "Use generic-lens or generic-optics with 'removeAttributes' instead." #-}

-- | Runs a Lambda function to modify the message.
--
-- /Note:/ Consider using 'lambda' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paLambda :: Lens.Lens' PipelineActivity (Lude.Maybe LambdaActivity)
paLambda = Lens.lens (lambda :: PipelineActivity -> Lude.Maybe LambdaActivity) (\s a -> s {lambda = a} :: PipelineActivity)
{-# DEPRECATED paLambda "Use generic-lens or generic-optics with 'lambda' instead." #-}

-- | Specifies where to store the processed message data.
--
-- /Note:/ Consider using 'datastore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paDatastore :: Lens.Lens' PipelineActivity (Lude.Maybe DatastoreActivity)
paDatastore = Lens.lens (datastore :: PipelineActivity -> Lude.Maybe DatastoreActivity) (\s a -> s {datastore = a} :: PipelineActivity)
{-# DEPRECATED paDatastore "Use generic-lens or generic-optics with 'datastore' instead." #-}

-- | Adds information from the AWS IoT Device Shadow service to a message.
--
-- /Note:/ Consider using 'deviceShadowEnrich' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paDeviceShadowEnrich :: Lens.Lens' PipelineActivity (Lude.Maybe DeviceShadowEnrichActivity)
paDeviceShadowEnrich = Lens.lens (deviceShadowEnrich :: PipelineActivity -> Lude.Maybe DeviceShadowEnrichActivity) (\s a -> s {deviceShadowEnrich = a} :: PipelineActivity)
{-# DEPRECATED paDeviceShadowEnrich "Use generic-lens or generic-optics with 'deviceShadowEnrich' instead." #-}

-- | Filters a message based on its attributes.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paFilter :: Lens.Lens' PipelineActivity (Lude.Maybe FilterActivity)
paFilter = Lens.lens (filter :: PipelineActivity -> Lude.Maybe FilterActivity) (\s a -> s {filter = a} :: PipelineActivity)
{-# DEPRECATED paFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | Computes an arithmetic expression using the message's attributes and adds it to the message.
--
-- /Note:/ Consider using 'math' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paMath :: Lens.Lens' PipelineActivity (Lude.Maybe MathActivity)
paMath = Lens.lens (math :: PipelineActivity -> Lude.Maybe MathActivity) (\s a -> s {math = a} :: PipelineActivity)
{-# DEPRECATED paMath "Use generic-lens or generic-optics with 'math' instead." #-}

instance Lude.FromJSON PipelineActivity where
  parseJSON =
    Lude.withObject
      "PipelineActivity"
      ( \x ->
          PipelineActivity'
            Lude.<$> (x Lude..:? "selectAttributes")
            Lude.<*> (x Lude..:? "channel")
            Lude.<*> (x Lude..:? "addAttributes")
            Lude.<*> (x Lude..:? "deviceRegistryEnrich")
            Lude.<*> (x Lude..:? "removeAttributes")
            Lude.<*> (x Lude..:? "lambda")
            Lude.<*> (x Lude..:? "datastore")
            Lude.<*> (x Lude..:? "deviceShadowEnrich")
            Lude.<*> (x Lude..:? "filter")
            Lude.<*> (x Lude..:? "math")
      )

instance Lude.ToJSON PipelineActivity where
  toJSON PipelineActivity' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("selectAttributes" Lude..=) Lude.<$> selectAttributes,
            ("channel" Lude..=) Lude.<$> channel,
            ("addAttributes" Lude..=) Lude.<$> addAttributes,
            ("deviceRegistryEnrich" Lude..=) Lude.<$> deviceRegistryEnrich,
            ("removeAttributes" Lude..=) Lude.<$> removeAttributes,
            ("lambda" Lude..=) Lude.<$> lambda,
            ("datastore" Lude..=) Lude.<$> datastore,
            ("deviceShadowEnrich" Lude..=) Lude.<$> deviceShadowEnrich,
            ("filter" Lude..=) Lude.<$> filter,
            ("math" Lude..=) Lude.<$> math
          ]
      )
