{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.AddApplicationInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a streaming source to your Amazon Kinesis application. For conceptual information, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html Configuring Application Input> . 
--
-- You can add a streaming source either when you create an application or you can use this operation to add a streaming source after you create an application. For more information, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_CreateApplication.html CreateApplication> .
-- Any configuration update, including adding a streaming source using this operation, results in a new version of the application. You can use the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation to find the current application version. 
-- This operation requires permissions to perform the @kinesisanalytics:AddApplicationInput@ action.
module Network.AWS.KinesisAnalytics.AddApplicationInput
    (
    -- * Creating a request
      AddApplicationInput (..)
    , mkAddApplicationInput
    -- ** Request lenses
    , aaiApplicationName
    , aaiCurrentApplicationVersionId
    , aaiInput

    -- * Destructuring the response
    , AddApplicationInputResponse (..)
    , mkAddApplicationInputResponse
    -- ** Response lenses
    , aairrsResponseStatus
    ) where

import qualified Network.AWS.KinesisAnalytics.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkAddApplicationInput' smart constructor.
data AddApplicationInput = AddApplicationInput'
  { applicationName :: Types.ApplicationName
    -- ^ Name of your existing Amazon Kinesis Analytics application to which you want to add the streaming source.
  , currentApplicationVersionId :: Core.Natural
    -- ^ Current version of your Amazon Kinesis Analytics application. You can use the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation to find the current application version.
  , input :: Types.Input
    -- ^ The <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_Input.html Input> to add.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddApplicationInput' value with any optional fields omitted.
mkAddApplicationInput
    :: Types.ApplicationName -- ^ 'applicationName'
    -> Core.Natural -- ^ 'currentApplicationVersionId'
    -> Types.Input -- ^ 'input'
    -> AddApplicationInput
mkAddApplicationInput applicationName currentApplicationVersionId
  input
  = AddApplicationInput'{applicationName,
                         currentApplicationVersionId, input}

-- | Name of your existing Amazon Kinesis Analytics application to which you want to add the streaming source.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaiApplicationName :: Lens.Lens' AddApplicationInput Types.ApplicationName
aaiApplicationName = Lens.field @"applicationName"
{-# INLINEABLE aaiApplicationName #-}
{-# DEPRECATED applicationName "Use generic-lens or generic-optics with 'applicationName' instead"  #-}

-- | Current version of your Amazon Kinesis Analytics application. You can use the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation to find the current application version.
--
-- /Note:/ Consider using 'currentApplicationVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaiCurrentApplicationVersionId :: Lens.Lens' AddApplicationInput Core.Natural
aaiCurrentApplicationVersionId = Lens.field @"currentApplicationVersionId"
{-# INLINEABLE aaiCurrentApplicationVersionId #-}
{-# DEPRECATED currentApplicationVersionId "Use generic-lens or generic-optics with 'currentApplicationVersionId' instead"  #-}

-- | The <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_Input.html Input> to add.
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaiInput :: Lens.Lens' AddApplicationInput Types.Input
aaiInput = Lens.field @"input"
{-# INLINEABLE aaiInput #-}
{-# DEPRECATED input "Use generic-lens or generic-optics with 'input' instead"  #-}

instance Core.ToQuery AddApplicationInput where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AddApplicationInput where
        toHeaders AddApplicationInput{..}
          = Core.pure
              ("X-Amz-Target", "KinesisAnalytics_20150814.AddApplicationInput")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AddApplicationInput where
        toJSON AddApplicationInput{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ApplicationName" Core..= applicationName),
                  Core.Just
                    ("CurrentApplicationVersionId" Core..=
                       currentApplicationVersionId),
                  Core.Just ("Input" Core..= input)])

instance Core.AWSRequest AddApplicationInput where
        type Rs AddApplicationInput = AddApplicationInputResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 AddApplicationInputResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | 
--
-- /See:/ 'mkAddApplicationInputResponse' smart constructor.
newtype AddApplicationInputResponse = AddApplicationInputResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AddApplicationInputResponse' value with any optional fields omitted.
mkAddApplicationInputResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AddApplicationInputResponse
mkAddApplicationInputResponse responseStatus
  = AddApplicationInputResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aairrsResponseStatus :: Lens.Lens' AddApplicationInputResponse Core.Int
aairrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE aairrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
