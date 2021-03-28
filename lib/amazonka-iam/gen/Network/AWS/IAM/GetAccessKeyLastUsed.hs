{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.GetAccessKeyLastUsed
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about when the specified access key was last used. The information includes the date and time of last use, along with the AWS service and Region that were specified in the last request made with that key.
module Network.AWS.IAM.GetAccessKeyLastUsed
    (
    -- * Creating a request
      GetAccessKeyLastUsed (..)
    , mkGetAccessKeyLastUsed
    -- ** Request lenses
    , gakluAccessKeyId

    -- * Destructuring the response
    , GetAccessKeyLastUsedResponse (..)
    , mkGetAccessKeyLastUsedResponse
    -- ** Response lenses
    , gaklurrsAccessKeyLastUsed
    , gaklurrsUserName
    , gaklurrsResponseStatus
    ) where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetAccessKeyLastUsed' smart constructor.
newtype GetAccessKeyLastUsed = GetAccessKeyLastUsed'
  { accessKeyId :: Types.AccessKey
    -- ^ The identifier of an access key.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that can consist of any upper or lowercased letter or digit.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetAccessKeyLastUsed' value with any optional fields omitted.
mkGetAccessKeyLastUsed
    :: Types.AccessKey -- ^ 'accessKeyId'
    -> GetAccessKeyLastUsed
mkGetAccessKeyLastUsed accessKeyId
  = GetAccessKeyLastUsed'{accessKeyId}

-- | The identifier of an access key.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that can consist of any upper or lowercased letter or digit.
--
-- /Note:/ Consider using 'accessKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gakluAccessKeyId :: Lens.Lens' GetAccessKeyLastUsed Types.AccessKey
gakluAccessKeyId = Lens.field @"accessKeyId"
{-# INLINEABLE gakluAccessKeyId #-}
{-# DEPRECATED accessKeyId "Use generic-lens or generic-optics with 'accessKeyId' instead"  #-}

instance Core.ToQuery GetAccessKeyLastUsed where
        toQuery GetAccessKeyLastUsed{..}
          = Core.toQueryPair "Action" ("GetAccessKeyLastUsed" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-05-08" :: Core.Text)
              Core.<> Core.toQueryPair "AccessKeyId" accessKeyId

instance Core.ToHeaders GetAccessKeyLastUsed where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetAccessKeyLastUsed where
        type Rs GetAccessKeyLastUsed = GetAccessKeyLastUsedResponse
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
          = Response.receiveXMLWrapper "GetAccessKeyLastUsedResult"
              (\ s h x ->
                 GetAccessKeyLastUsedResponse' Core.<$>
                   (x Core..@? "AccessKeyLastUsed") Core.<*> x Core..@? "UserName"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the response to a successful 'GetAccessKeyLastUsed' request. It is also returned as a member of the 'AccessKeyMetaData' structure returned by the 'ListAccessKeys' action.
--
-- /See:/ 'mkGetAccessKeyLastUsedResponse' smart constructor.
data GetAccessKeyLastUsedResponse = GetAccessKeyLastUsedResponse'
  { accessKeyLastUsed :: Core.Maybe Types.AccessKeyLastUsed
    -- ^ Contains information about the last time the access key was used.
  , userName :: Core.Maybe Types.ExistingUserNameType
    -- ^ The name of the AWS IAM user that owns this access key.
--
--
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetAccessKeyLastUsedResponse' value with any optional fields omitted.
mkGetAccessKeyLastUsedResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetAccessKeyLastUsedResponse
mkGetAccessKeyLastUsedResponse responseStatus
  = GetAccessKeyLastUsedResponse'{accessKeyLastUsed = Core.Nothing,
                                  userName = Core.Nothing, responseStatus}

-- | Contains information about the last time the access key was used.
--
-- /Note:/ Consider using 'accessKeyLastUsed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaklurrsAccessKeyLastUsed :: Lens.Lens' GetAccessKeyLastUsedResponse (Core.Maybe Types.AccessKeyLastUsed)
gaklurrsAccessKeyLastUsed = Lens.field @"accessKeyLastUsed"
{-# INLINEABLE gaklurrsAccessKeyLastUsed #-}
{-# DEPRECATED accessKeyLastUsed "Use generic-lens or generic-optics with 'accessKeyLastUsed' instead"  #-}

-- | The name of the AWS IAM user that owns this access key.
--
--
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaklurrsUserName :: Lens.Lens' GetAccessKeyLastUsedResponse (Core.Maybe Types.ExistingUserNameType)
gaklurrsUserName = Lens.field @"userName"
{-# INLINEABLE gaklurrsUserName #-}
{-# DEPRECATED userName "Use generic-lens or generic-optics with 'userName' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaklurrsResponseStatus :: Lens.Lens' GetAccessKeyLastUsedResponse Core.Int
gaklurrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gaklurrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
