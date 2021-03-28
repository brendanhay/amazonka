{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.ListServiceSpecificCredentials
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the service-specific credentials associated with the specified IAM user. If none exists, the operation returns an empty list. The service-specific credentials returned by this operation are used only for authenticating the IAM user to a specific service. For more information about using service-specific credentials to authenticate to an AWS service, see <https://docs.aws.amazon.com/codecommit/latest/userguide/setting-up-gc.html Set Up service-specific credentials> in the AWS CodeCommit User Guide.
module Network.AWS.IAM.ListServiceSpecificCredentials
    (
    -- * Creating a request
      ListServiceSpecificCredentials (..)
    , mkListServiceSpecificCredentials
    -- ** Request lenses
    , lsscServiceName
    , lsscUserName

    -- * Destructuring the response
    , ListServiceSpecificCredentialsResponse (..)
    , mkListServiceSpecificCredentialsResponse
    -- ** Response lenses
    , lsscrrsServiceSpecificCredentials
    , lsscrrsResponseStatus
    ) where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListServiceSpecificCredentials' smart constructor.
data ListServiceSpecificCredentials = ListServiceSpecificCredentials'
  { serviceName :: Core.Maybe Types.ServiceName
    -- ^ Filters the returned results to only those for the specified AWS service. If not specified, then AWS returns service-specific credentials for all services.
  , userName :: Core.Maybe Types.UserName
    -- ^ The name of the user whose service-specific credentials you want information about. If this value is not specified, then the operation assumes the user whose credentials are used to call the operation.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListServiceSpecificCredentials' value with any optional fields omitted.
mkListServiceSpecificCredentials
    :: ListServiceSpecificCredentials
mkListServiceSpecificCredentials
  = ListServiceSpecificCredentials'{serviceName = Core.Nothing,
                                    userName = Core.Nothing}

-- | Filters the returned results to only those for the specified AWS service. If not specified, then AWS returns service-specific credentials for all services.
--
-- /Note:/ Consider using 'serviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsscServiceName :: Lens.Lens' ListServiceSpecificCredentials (Core.Maybe Types.ServiceName)
lsscServiceName = Lens.field @"serviceName"
{-# INLINEABLE lsscServiceName #-}
{-# DEPRECATED serviceName "Use generic-lens or generic-optics with 'serviceName' instead"  #-}

-- | The name of the user whose service-specific credentials you want information about. If this value is not specified, then the operation assumes the user whose credentials are used to call the operation.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsscUserName :: Lens.Lens' ListServiceSpecificCredentials (Core.Maybe Types.UserName)
lsscUserName = Lens.field @"userName"
{-# INLINEABLE lsscUserName #-}
{-# DEPRECATED userName "Use generic-lens or generic-optics with 'userName' instead"  #-}

instance Core.ToQuery ListServiceSpecificCredentials where
        toQuery ListServiceSpecificCredentials{..}
          = Core.toQueryPair "Action"
              ("ListServiceSpecificCredentials" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-05-08" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ServiceName") serviceName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "UserName") userName

instance Core.ToHeaders ListServiceSpecificCredentials where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListServiceSpecificCredentials where
        type Rs ListServiceSpecificCredentials =
             ListServiceSpecificCredentialsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "ListServiceSpecificCredentialsResult"
              (\ s h x ->
                 ListServiceSpecificCredentialsResponse' Core.<$>
                   (x Core..@? "ServiceSpecificCredentials" Core..<@>
                      Core.parseXMLList "member")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkListServiceSpecificCredentialsResponse' smart constructor.
data ListServiceSpecificCredentialsResponse = ListServiceSpecificCredentialsResponse'
  { serviceSpecificCredentials :: Core.Maybe [Types.ServiceSpecificCredentialMetadata]
    -- ^ A list of structures that each contain details about a service-specific credential.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListServiceSpecificCredentialsResponse' value with any optional fields omitted.
mkListServiceSpecificCredentialsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListServiceSpecificCredentialsResponse
mkListServiceSpecificCredentialsResponse responseStatus
  = ListServiceSpecificCredentialsResponse'{serviceSpecificCredentials
                                              = Core.Nothing,
                                            responseStatus}

-- | A list of structures that each contain details about a service-specific credential.
--
-- /Note:/ Consider using 'serviceSpecificCredentials' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsscrrsServiceSpecificCredentials :: Lens.Lens' ListServiceSpecificCredentialsResponse (Core.Maybe [Types.ServiceSpecificCredentialMetadata])
lsscrrsServiceSpecificCredentials = Lens.field @"serviceSpecificCredentials"
{-# INLINEABLE lsscrrsServiceSpecificCredentials #-}
{-# DEPRECATED serviceSpecificCredentials "Use generic-lens or generic-optics with 'serviceSpecificCredentials' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsscrrsResponseStatus :: Lens.Lens' ListServiceSpecificCredentialsResponse Core.Int
lsscrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lsscrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
