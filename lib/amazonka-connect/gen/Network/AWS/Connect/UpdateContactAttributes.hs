{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.UpdateContactAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates the contact attributes associated with the specified contact.
--
-- You can add or update attributes for both ongoing and completed contacts. For example, you can update the customer's name or the reason the customer called while the call is active, or add notes about steps that the agent took during the call that are displayed to the next agent that takes the call. You can also update attributes for a contact using data from your CRM application and save the data with the contact in Amazon Connect. You could also flag calls for additional analysis, such as legal review or identifying abusive callers.
-- Contact attributes are available in Amazon Connect for 24 months, and are then deleted.
-- __Important:__ You cannot use the operation to update attributes for contacts that occurred prior to the release of the API, September 12, 2018. You can update attributes only for contacts that started after the release of the API. If you attempt to update attributes for a contact that occurred prior to the release of the API, a 400 error is returned. This applies also to queued callbacks that were initiated prior to the release of the API but are still active in your instance.
module Network.AWS.Connect.UpdateContactAttributes
    (
    -- * Creating a request
      UpdateContactAttributes (..)
    , mkUpdateContactAttributes
    -- ** Request lenses
    , ucaInitialContactId
    , ucaInstanceId
    , ucaAttributes

    -- * Destructuring the response
    , UpdateContactAttributesResponse (..)
    , mkUpdateContactAttributesResponse
    -- ** Response lenses
    , ucarrsResponseStatus
    ) where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateContactAttributes' smart constructor.
data UpdateContactAttributes = UpdateContactAttributes'
  { initialContactId :: Types.InitialContactId
    -- ^ The identifier of the contact. This is the identifier of the contact associated with the first interaction with the contact center.
  , instanceId :: Types.InstanceId
    -- ^ The identifier of the Amazon Connect instance.
  , attributes :: Core.HashMap Types.AttributeName Types.AttributeValue
    -- ^ The Amazon Connect attributes. These attributes can be accessed in contact flows just like any other contact attributes.
--
-- You can have up to 32,768 UTF-8 bytes across all attributes for a contact. Attribute keys can include only alphanumeric, dash, and underscore characters.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateContactAttributes' value with any optional fields omitted.
mkUpdateContactAttributes
    :: Types.InitialContactId -- ^ 'initialContactId'
    -> Types.InstanceId -- ^ 'instanceId'
    -> UpdateContactAttributes
mkUpdateContactAttributes initialContactId instanceId
  = UpdateContactAttributes'{initialContactId, instanceId,
                             attributes = Core.mempty}

-- | The identifier of the contact. This is the identifier of the contact associated with the first interaction with the contact center.
--
-- /Note:/ Consider using 'initialContactId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucaInitialContactId :: Lens.Lens' UpdateContactAttributes Types.InitialContactId
ucaInitialContactId = Lens.field @"initialContactId"
{-# INLINEABLE ucaInitialContactId #-}
{-# DEPRECATED initialContactId "Use generic-lens or generic-optics with 'initialContactId' instead"  #-}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucaInstanceId :: Lens.Lens' UpdateContactAttributes Types.InstanceId
ucaInstanceId = Lens.field @"instanceId"
{-# INLINEABLE ucaInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | The Amazon Connect attributes. These attributes can be accessed in contact flows just like any other contact attributes.
--
-- You can have up to 32,768 UTF-8 bytes across all attributes for a contact. Attribute keys can include only alphanumeric, dash, and underscore characters.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucaAttributes :: Lens.Lens' UpdateContactAttributes (Core.HashMap Types.AttributeName Types.AttributeValue)
ucaAttributes = Lens.field @"attributes"
{-# INLINEABLE ucaAttributes #-}
{-# DEPRECATED attributes "Use generic-lens or generic-optics with 'attributes' instead"  #-}

instance Core.ToQuery UpdateContactAttributes where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateContactAttributes where
        toHeaders UpdateContactAttributes{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateContactAttributes where
        toJSON UpdateContactAttributes{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("InitialContactId" Core..= initialContactId),
                  Core.Just ("InstanceId" Core..= instanceId),
                  Core.Just ("Attributes" Core..= attributes)])

instance Core.AWSRequest UpdateContactAttributes where
        type Rs UpdateContactAttributes = UpdateContactAttributesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/contact/attributes",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 UpdateContactAttributesResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateContactAttributesResponse' smart constructor.
newtype UpdateContactAttributesResponse = UpdateContactAttributesResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateContactAttributesResponse' value with any optional fields omitted.
mkUpdateContactAttributesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateContactAttributesResponse
mkUpdateContactAttributesResponse responseStatus
  = UpdateContactAttributesResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucarrsResponseStatus :: Lens.Lens' UpdateContactAttributesResponse Core.Int
ucarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ucarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
