{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.UpdateServiceSpecificCredential
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the status of a service-specific credential to @Active@ or @Inactive@ . Service-specific credentials that are inactive cannot be used for authentication to the service. This operation can be used to disable a user's service-specific credential as part of a credential rotation work flow.
module Network.AWS.IAM.UpdateServiceSpecificCredential
    (
    -- * Creating a request
      UpdateServiceSpecificCredential (..)
    , mkUpdateServiceSpecificCredential
    -- ** Request lenses
    , usscServiceSpecificCredentialId
    , usscStatus
    , usscUserName

    -- * Destructuring the response
    , UpdateServiceSpecificCredentialResponse (..)
    , mkUpdateServiceSpecificCredentialResponse
    ) where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateServiceSpecificCredential' smart constructor.
data UpdateServiceSpecificCredential = UpdateServiceSpecificCredential'
  { serviceSpecificCredentialId :: Types.ServiceSpecificCredentialId
    -- ^ The unique identifier of the service-specific credential.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that can consist of any upper or lowercased letter or digit.
  , status :: Types.StatusType
    -- ^ The status to be assigned to the service-specific credential.
  , userName :: Core.Maybe Types.UserNameType
    -- ^ The name of the IAM user associated with the service-specific credential. If you do not specify this value, then the operation assumes the user whose credentials are used to call the operation.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateServiceSpecificCredential' value with any optional fields omitted.
mkUpdateServiceSpecificCredential
    :: Types.ServiceSpecificCredentialId -- ^ 'serviceSpecificCredentialId'
    -> Types.StatusType -- ^ 'status'
    -> UpdateServiceSpecificCredential
mkUpdateServiceSpecificCredential serviceSpecificCredentialId
  status
  = UpdateServiceSpecificCredential'{serviceSpecificCredentialId,
                                     status, userName = Core.Nothing}

-- | The unique identifier of the service-specific credential.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that can consist of any upper or lowercased letter or digit.
--
-- /Note:/ Consider using 'serviceSpecificCredentialId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usscServiceSpecificCredentialId :: Lens.Lens' UpdateServiceSpecificCredential Types.ServiceSpecificCredentialId
usscServiceSpecificCredentialId = Lens.field @"serviceSpecificCredentialId"
{-# INLINEABLE usscServiceSpecificCredentialId #-}
{-# DEPRECATED serviceSpecificCredentialId "Use generic-lens or generic-optics with 'serviceSpecificCredentialId' instead"  #-}

-- | The status to be assigned to the service-specific credential.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usscStatus :: Lens.Lens' UpdateServiceSpecificCredential Types.StatusType
usscStatus = Lens.field @"status"
{-# INLINEABLE usscStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The name of the IAM user associated with the service-specific credential. If you do not specify this value, then the operation assumes the user whose credentials are used to call the operation.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usscUserName :: Lens.Lens' UpdateServiceSpecificCredential (Core.Maybe Types.UserNameType)
usscUserName = Lens.field @"userName"
{-# INLINEABLE usscUserName #-}
{-# DEPRECATED userName "Use generic-lens or generic-optics with 'userName' instead"  #-}

instance Core.ToQuery UpdateServiceSpecificCredential where
        toQuery UpdateServiceSpecificCredential{..}
          = Core.toQueryPair "Action"
              ("UpdateServiceSpecificCredential" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-05-08" :: Core.Text)
              Core.<>
              Core.toQueryPair "ServiceSpecificCredentialId"
                serviceSpecificCredentialId
              Core.<> Core.toQueryPair "Status" status
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "UserName") userName

instance Core.ToHeaders UpdateServiceSpecificCredential where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest UpdateServiceSpecificCredential where
        type Rs UpdateServiceSpecificCredential =
             UpdateServiceSpecificCredentialResponse
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
          = Response.receiveNull UpdateServiceSpecificCredentialResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateServiceSpecificCredentialResponse' smart constructor.
data UpdateServiceSpecificCredentialResponse = UpdateServiceSpecificCredentialResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateServiceSpecificCredentialResponse' value with any optional fields omitted.
mkUpdateServiceSpecificCredentialResponse
    :: UpdateServiceSpecificCredentialResponse
mkUpdateServiceSpecificCredentialResponse
  = UpdateServiceSpecificCredentialResponse'
