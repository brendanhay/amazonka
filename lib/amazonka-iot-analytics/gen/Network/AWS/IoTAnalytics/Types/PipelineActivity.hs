{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.PipelineActivity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoTAnalytics.Types.PipelineActivity
  ( PipelineActivity (..)
  -- * Smart constructor
  , mkPipelineActivity
  -- * Lenses
  , paAddAttributes
  , paChannel
  , paDatastore
  , paDeviceRegistryEnrich
  , paDeviceShadowEnrich
  , paFilter
  , paLambda
  , paMath
  , paRemoveAttributes
  , paSelectAttributes
  ) where

import qualified Network.AWS.IoTAnalytics.Types.AddAttributesActivity as Types
import qualified Network.AWS.IoTAnalytics.Types.ChannelActivity as Types
import qualified Network.AWS.IoTAnalytics.Types.DatastoreActivity as Types
import qualified Network.AWS.IoTAnalytics.Types.DeviceRegistryEnrichActivity as Types
import qualified Network.AWS.IoTAnalytics.Types.DeviceShadowEnrichActivity as Types
import qualified Network.AWS.IoTAnalytics.Types.FilterActivity as Types
import qualified Network.AWS.IoTAnalytics.Types.LambdaActivity as Types
import qualified Network.AWS.IoTAnalytics.Types.MathActivity as Types
import qualified Network.AWS.IoTAnalytics.Types.RemoveAttributesActivity as Types
import qualified Network.AWS.IoTAnalytics.Types.SelectAttributesActivity as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An activity that performs a transformation on a message.
--
-- /See:/ 'mkPipelineActivity' smart constructor.
data PipelineActivity = PipelineActivity'
  { addAttributes :: Core.Maybe Types.AddAttributesActivity
    -- ^ Adds other attributes based on existing attributes in the message.
  , channel :: Core.Maybe Types.ChannelActivity
    -- ^ Determines the source of the messages to be processed.
  , datastore :: Core.Maybe Types.DatastoreActivity
    -- ^ Specifies where to store the processed message data.
  , deviceRegistryEnrich :: Core.Maybe Types.DeviceRegistryEnrichActivity
    -- ^ Adds data from the AWS IoT device registry to your message.
  , deviceShadowEnrich :: Core.Maybe Types.DeviceShadowEnrichActivity
    -- ^ Adds information from the AWS IoT Device Shadow service to a message.
  , filter :: Core.Maybe Types.FilterActivity
    -- ^ Filters a message based on its attributes.
  , lambda :: Core.Maybe Types.LambdaActivity
    -- ^ Runs a Lambda function to modify the message.
  , math :: Core.Maybe Types.MathActivity
    -- ^ Computes an arithmetic expression using the message's attributes and adds it to the message.
  , removeAttributes :: Core.Maybe Types.RemoveAttributesActivity
    -- ^ Removes attributes from a message.
  , selectAttributes :: Core.Maybe Types.SelectAttributesActivity
    -- ^ Creates a new message using only the specified attributes from the original message. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PipelineActivity' value with any optional fields omitted.
mkPipelineActivity
    :: PipelineActivity
mkPipelineActivity
  = PipelineActivity'{addAttributes = Core.Nothing,
                      channel = Core.Nothing, datastore = Core.Nothing,
                      deviceRegistryEnrich = Core.Nothing,
                      deviceShadowEnrich = Core.Nothing, filter = Core.Nothing,
                      lambda = Core.Nothing, math = Core.Nothing,
                      removeAttributes = Core.Nothing, selectAttributes = Core.Nothing}

-- | Adds other attributes based on existing attributes in the message.
--
-- /Note:/ Consider using 'addAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paAddAttributes :: Lens.Lens' PipelineActivity (Core.Maybe Types.AddAttributesActivity)
paAddAttributes = Lens.field @"addAttributes"
{-# INLINEABLE paAddAttributes #-}
{-# DEPRECATED addAttributes "Use generic-lens or generic-optics with 'addAttributes' instead"  #-}

-- | Determines the source of the messages to be processed.
--
-- /Note:/ Consider using 'channel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paChannel :: Lens.Lens' PipelineActivity (Core.Maybe Types.ChannelActivity)
paChannel = Lens.field @"channel"
{-# INLINEABLE paChannel #-}
{-# DEPRECATED channel "Use generic-lens or generic-optics with 'channel' instead"  #-}

-- | Specifies where to store the processed message data.
--
-- /Note:/ Consider using 'datastore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paDatastore :: Lens.Lens' PipelineActivity (Core.Maybe Types.DatastoreActivity)
paDatastore = Lens.field @"datastore"
{-# INLINEABLE paDatastore #-}
{-# DEPRECATED datastore "Use generic-lens or generic-optics with 'datastore' instead"  #-}

-- | Adds data from the AWS IoT device registry to your message.
--
-- /Note:/ Consider using 'deviceRegistryEnrich' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paDeviceRegistryEnrich :: Lens.Lens' PipelineActivity (Core.Maybe Types.DeviceRegistryEnrichActivity)
paDeviceRegistryEnrich = Lens.field @"deviceRegistryEnrich"
{-# INLINEABLE paDeviceRegistryEnrich #-}
{-# DEPRECATED deviceRegistryEnrich "Use generic-lens or generic-optics with 'deviceRegistryEnrich' instead"  #-}

-- | Adds information from the AWS IoT Device Shadow service to a message.
--
-- /Note:/ Consider using 'deviceShadowEnrich' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paDeviceShadowEnrich :: Lens.Lens' PipelineActivity (Core.Maybe Types.DeviceShadowEnrichActivity)
paDeviceShadowEnrich = Lens.field @"deviceShadowEnrich"
{-# INLINEABLE paDeviceShadowEnrich #-}
{-# DEPRECATED deviceShadowEnrich "Use generic-lens or generic-optics with 'deviceShadowEnrich' instead"  #-}

-- | Filters a message based on its attributes.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paFilter :: Lens.Lens' PipelineActivity (Core.Maybe Types.FilterActivity)
paFilter = Lens.field @"filter"
{-# INLINEABLE paFilter #-}
{-# DEPRECATED filter "Use generic-lens or generic-optics with 'filter' instead"  #-}

-- | Runs a Lambda function to modify the message.
--
-- /Note:/ Consider using 'lambda' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paLambda :: Lens.Lens' PipelineActivity (Core.Maybe Types.LambdaActivity)
paLambda = Lens.field @"lambda"
{-# INLINEABLE paLambda #-}
{-# DEPRECATED lambda "Use generic-lens or generic-optics with 'lambda' instead"  #-}

-- | Computes an arithmetic expression using the message's attributes and adds it to the message.
--
-- /Note:/ Consider using 'math' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paMath :: Lens.Lens' PipelineActivity (Core.Maybe Types.MathActivity)
paMath = Lens.field @"math"
{-# INLINEABLE paMath #-}
{-# DEPRECATED math "Use generic-lens or generic-optics with 'math' instead"  #-}

-- | Removes attributes from a message.
--
-- /Note:/ Consider using 'removeAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paRemoveAttributes :: Lens.Lens' PipelineActivity (Core.Maybe Types.RemoveAttributesActivity)
paRemoveAttributes = Lens.field @"removeAttributes"
{-# INLINEABLE paRemoveAttributes #-}
{-# DEPRECATED removeAttributes "Use generic-lens or generic-optics with 'removeAttributes' instead"  #-}

-- | Creates a new message using only the specified attributes from the original message. 
--
-- /Note:/ Consider using 'selectAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paSelectAttributes :: Lens.Lens' PipelineActivity (Core.Maybe Types.SelectAttributesActivity)
paSelectAttributes = Lens.field @"selectAttributes"
{-# INLINEABLE paSelectAttributes #-}
{-# DEPRECATED selectAttributes "Use generic-lens or generic-optics with 'selectAttributes' instead"  #-}

instance Core.FromJSON PipelineActivity where
        toJSON PipelineActivity{..}
          = Core.object
              (Core.catMaybes
                 [("addAttributes" Core..=) Core.<$> addAttributes,
                  ("channel" Core..=) Core.<$> channel,
                  ("datastore" Core..=) Core.<$> datastore,
                  ("deviceRegistryEnrich" Core..=) Core.<$> deviceRegistryEnrich,
                  ("deviceShadowEnrich" Core..=) Core.<$> deviceShadowEnrich,
                  ("filter" Core..=) Core.<$> filter,
                  ("lambda" Core..=) Core.<$> lambda, ("math" Core..=) Core.<$> math,
                  ("removeAttributes" Core..=) Core.<$> removeAttributes,
                  ("selectAttributes" Core..=) Core.<$> selectAttributes])

instance Core.FromJSON PipelineActivity where
        parseJSON
          = Core.withObject "PipelineActivity" Core.$
              \ x ->
                PipelineActivity' Core.<$>
                  (x Core..:? "addAttributes") Core.<*> x Core..:? "channel" Core.<*>
                    x Core..:? "datastore"
                    Core.<*> x Core..:? "deviceRegistryEnrich"
                    Core.<*> x Core..:? "deviceShadowEnrich"
                    Core.<*> x Core..:? "filter"
                    Core.<*> x Core..:? "lambda"
                    Core.<*> x Core..:? "math"
                    Core.<*> x Core..:? "removeAttributes"
                    Core.<*> x Core..:? "selectAttributes"
