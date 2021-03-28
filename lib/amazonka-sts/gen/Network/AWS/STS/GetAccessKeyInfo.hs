{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.STS.GetAccessKeyInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the account identifier for the specified access key ID.
--
-- Access keys consist of two parts: an access key ID (for example, @AKIAIOSFODNN7EXAMPLE@ ) and a secret access key (for example, @wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY@ ). For more information about access keys, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_access-keys.html Managing Access Keys for IAM Users> in the /IAM User Guide/ .
-- When you pass an access key ID to this operation, it returns the ID of the AWS account to which the keys belong. Access key IDs beginning with @AKIA@ are long-term credentials for an IAM user or the AWS account root user. Access key IDs beginning with @ASIA@ are temporary credentials that are created using STS operations. If the account in the response belongs to you, you can sign in as the root user and review your root user access keys. Then, you can pull a <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_getting-report.html credentials report> to learn which IAM user owns the keys. To learn who requested the temporary credentials for an @ASIA@ access key, view the STS events in your <https://docs.aws.amazon.com/IAM/latest/UserGuide/cloudtrail-integration.html CloudTrail logs> in the /IAM User Guide/ .
-- This operation does not indicate the state of the access key. The key might be active, inactive, or deleted. Active keys might not have permissions to perform an operation. Providing a deleted access key might return an error that the key doesn't exist.
module Network.AWS.STS.GetAccessKeyInfo
    (
    -- * Creating a request
      GetAccessKeyInfo (..)
    , mkGetAccessKeyInfo
    -- ** Request lenses
    , gakiAccessKeyId

    -- * Destructuring the response
    , GetAccessKeyInfoResponse (..)
    , mkGetAccessKeyInfoResponse
    -- ** Response lenses
    , gakirrsAccount
    , gakirrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.STS.Types as Types

-- | /See:/ 'mkGetAccessKeyInfo' smart constructor.
newtype GetAccessKeyInfo = GetAccessKeyInfo'
  { accessKeyId :: Types.AccessKeyId
    -- ^ The identifier of an access key.
--
-- This parameter allows (through its regex pattern) a string of characters that can consist of any upper- or lowercase letter or digit.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetAccessKeyInfo' value with any optional fields omitted.
mkGetAccessKeyInfo
    :: Types.AccessKeyId -- ^ 'accessKeyId'
    -> GetAccessKeyInfo
mkGetAccessKeyInfo accessKeyId = GetAccessKeyInfo'{accessKeyId}

-- | The identifier of an access key.
--
-- This parameter allows (through its regex pattern) a string of characters that can consist of any upper- or lowercase letter or digit.
--
-- /Note:/ Consider using 'accessKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gakiAccessKeyId :: Lens.Lens' GetAccessKeyInfo Types.AccessKeyId
gakiAccessKeyId = Lens.field @"accessKeyId"
{-# INLINEABLE gakiAccessKeyId #-}
{-# DEPRECATED accessKeyId "Use generic-lens or generic-optics with 'accessKeyId' instead"  #-}

instance Core.ToQuery GetAccessKeyInfo where
        toQuery GetAccessKeyInfo{..}
          = Core.toQueryPair "Action" ("GetAccessKeyInfo" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2011-06-15" :: Core.Text)
              Core.<> Core.toQueryPair "AccessKeyId" accessKeyId

instance Core.ToHeaders GetAccessKeyInfo where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetAccessKeyInfo where
        type Rs GetAccessKeyInfo = GetAccessKeyInfoResponse
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
          = Response.receiveXMLWrapper "GetAccessKeyInfoResult"
              (\ s h x ->
                 GetAccessKeyInfoResponse' Core.<$>
                   (x Core..@? "Account") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetAccessKeyInfoResponse' smart constructor.
data GetAccessKeyInfoResponse = GetAccessKeyInfoResponse'
  { account :: Core.Maybe Types.Account
    -- ^ The number used to identify the AWS account.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetAccessKeyInfoResponse' value with any optional fields omitted.
mkGetAccessKeyInfoResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetAccessKeyInfoResponse
mkGetAccessKeyInfoResponse responseStatus
  = GetAccessKeyInfoResponse'{account = Core.Nothing, responseStatus}

-- | The number used to identify the AWS account.
--
-- /Note:/ Consider using 'account' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gakirrsAccount :: Lens.Lens' GetAccessKeyInfoResponse (Core.Maybe Types.Account)
gakirrsAccount = Lens.field @"account"
{-# INLINEABLE gakirrsAccount #-}
{-# DEPRECATED account "Use generic-lens or generic-optics with 'account' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gakirrsResponseStatus :: Lens.Lens' GetAccessKeyInfoResponse Core.Int
gakirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gakirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
