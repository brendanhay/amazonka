{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.CreatePresignedNotebookInstanceUrl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a URL that you can use to connect to the Jupyter server from a notebook instance. In the Amazon SageMaker console, when you choose @Open@ next to a notebook instance, Amazon SageMaker opens a new tab showing the Jupyter server home page from the notebook instance. The console uses this API to get the URL and show the page.
--
-- The IAM role or user used to call this API defines the permissions to access the notebook instance. Once the presigned URL is created, no additional permission is required to access this URL. IAM authorization policies for this API are also enforced for every HTTP request and WebSocket frame that attempts to connect to the notebook instance.
-- You can restrict access to this API and to the URL that it returns to a list of IP addresses that you specify. Use the @NotIpAddress@ condition operator and the @aws:SourceIP@ condition context key to specify the list of IP addresses that you want to have access to the notebook instance. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/security_iam_id-based-policy-examples.html#nbi-ip-filter Limit Access to a Notebook Instance by IP Address> .
module Network.AWS.SageMaker.CreatePresignedNotebookInstanceUrl
    (
    -- * Creating a request
      CreatePresignedNotebookInstanceUrl (..)
    , mkCreatePresignedNotebookInstanceUrl
    -- ** Request lenses
    , cpniuNotebookInstanceName
    , cpniuSessionExpirationDurationInSeconds

    -- * Destructuring the response
    , CreatePresignedNotebookInstanceUrlResponse (..)
    , mkCreatePresignedNotebookInstanceUrlResponse
    -- ** Response lenses
    , cpniurrsAuthorizedUrl
    , cpniurrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkCreatePresignedNotebookInstanceUrl' smart constructor.
data CreatePresignedNotebookInstanceUrl = CreatePresignedNotebookInstanceUrl'
  { notebookInstanceName :: Types.NotebookInstanceName
    -- ^ The name of the notebook instance.
  , sessionExpirationDurationInSeconds :: Core.Maybe Core.Natural
    -- ^ The duration of the session, in seconds. The default is 12 hours.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePresignedNotebookInstanceUrl' value with any optional fields omitted.
mkCreatePresignedNotebookInstanceUrl
    :: Types.NotebookInstanceName -- ^ 'notebookInstanceName'
    -> CreatePresignedNotebookInstanceUrl
mkCreatePresignedNotebookInstanceUrl notebookInstanceName
  = CreatePresignedNotebookInstanceUrl'{notebookInstanceName,
                                        sessionExpirationDurationInSeconds = Core.Nothing}

-- | The name of the notebook instance.
--
-- /Note:/ Consider using 'notebookInstanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpniuNotebookInstanceName :: Lens.Lens' CreatePresignedNotebookInstanceUrl Types.NotebookInstanceName
cpniuNotebookInstanceName = Lens.field @"notebookInstanceName"
{-# INLINEABLE cpniuNotebookInstanceName #-}
{-# DEPRECATED notebookInstanceName "Use generic-lens or generic-optics with 'notebookInstanceName' instead"  #-}

-- | The duration of the session, in seconds. The default is 12 hours.
--
-- /Note:/ Consider using 'sessionExpirationDurationInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpniuSessionExpirationDurationInSeconds :: Lens.Lens' CreatePresignedNotebookInstanceUrl (Core.Maybe Core.Natural)
cpniuSessionExpirationDurationInSeconds = Lens.field @"sessionExpirationDurationInSeconds"
{-# INLINEABLE cpniuSessionExpirationDurationInSeconds #-}
{-# DEPRECATED sessionExpirationDurationInSeconds "Use generic-lens or generic-optics with 'sessionExpirationDurationInSeconds' instead"  #-}

instance Core.ToQuery CreatePresignedNotebookInstanceUrl where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreatePresignedNotebookInstanceUrl where
        toHeaders CreatePresignedNotebookInstanceUrl{..}
          = Core.pure
              ("X-Amz-Target", "SageMaker.CreatePresignedNotebookInstanceUrl")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreatePresignedNotebookInstanceUrl where
        toJSON CreatePresignedNotebookInstanceUrl{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("NotebookInstanceName" Core..= notebookInstanceName),
                  ("SessionExpirationDurationInSeconds" Core..=) Core.<$>
                    sessionExpirationDurationInSeconds])

instance Core.AWSRequest CreatePresignedNotebookInstanceUrl where
        type Rs CreatePresignedNotebookInstanceUrl =
             CreatePresignedNotebookInstanceUrlResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreatePresignedNotebookInstanceUrlResponse' Core.<$>
                   (x Core..:? "AuthorizedUrl") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreatePresignedNotebookInstanceUrlResponse' smart constructor.
data CreatePresignedNotebookInstanceUrlResponse = CreatePresignedNotebookInstanceUrlResponse'
  { authorizedUrl :: Core.Maybe Types.AuthorizedUrl
    -- ^ A JSON object that contains the URL string. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePresignedNotebookInstanceUrlResponse' value with any optional fields omitted.
mkCreatePresignedNotebookInstanceUrlResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreatePresignedNotebookInstanceUrlResponse
mkCreatePresignedNotebookInstanceUrlResponse responseStatus
  = CreatePresignedNotebookInstanceUrlResponse'{authorizedUrl =
                                                  Core.Nothing,
                                                responseStatus}

-- | A JSON object that contains the URL string. 
--
-- /Note:/ Consider using 'authorizedUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpniurrsAuthorizedUrl :: Lens.Lens' CreatePresignedNotebookInstanceUrlResponse (Core.Maybe Types.AuthorizedUrl)
cpniurrsAuthorizedUrl = Lens.field @"authorizedUrl"
{-# INLINEABLE cpniurrsAuthorizedUrl #-}
{-# DEPRECATED authorizedUrl "Use generic-lens or generic-optics with 'authorizedUrl' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpniurrsResponseStatus :: Lens.Lens' CreatePresignedNotebookInstanceUrlResponse Core.Int
cpniurrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cpniurrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
