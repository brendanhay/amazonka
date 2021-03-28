{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.DeleteServiceSpecificCredential
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified service-specific credential.
module Network.AWS.IAM.DeleteServiceSpecificCredential
    (
    -- * Creating a request
      DeleteServiceSpecificCredential (..)
    , mkDeleteServiceSpecificCredential
    -- ** Request lenses
    , dsscServiceSpecificCredentialId
    , dsscUserName

    -- * Destructuring the response
    , DeleteServiceSpecificCredentialResponse (..)
    , mkDeleteServiceSpecificCredentialResponse
    ) where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteServiceSpecificCredential' smart constructor.
data DeleteServiceSpecificCredential = DeleteServiceSpecificCredential'
  { serviceSpecificCredentialId :: Types.ServiceSpecificCredentialId
    -- ^ The unique identifier of the service-specific credential. You can get this value by calling 'ListServiceSpecificCredentials' .
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that can consist of any upper or lowercased letter or digit.
  , userName :: Core.Maybe Types.UserNameType
    -- ^ The name of the IAM user associated with the service-specific credential. If this value is not specified, then the operation assumes the user whose credentials are used to call the operation.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteServiceSpecificCredential' value with any optional fields omitted.
mkDeleteServiceSpecificCredential
    :: Types.ServiceSpecificCredentialId -- ^ 'serviceSpecificCredentialId'
    -> DeleteServiceSpecificCredential
mkDeleteServiceSpecificCredential serviceSpecificCredentialId
  = DeleteServiceSpecificCredential'{serviceSpecificCredentialId,
                                     userName = Core.Nothing}

-- | The unique identifier of the service-specific credential. You can get this value by calling 'ListServiceSpecificCredentials' .
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that can consist of any upper or lowercased letter or digit.
--
-- /Note:/ Consider using 'serviceSpecificCredentialId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsscServiceSpecificCredentialId :: Lens.Lens' DeleteServiceSpecificCredential Types.ServiceSpecificCredentialId
dsscServiceSpecificCredentialId = Lens.field @"serviceSpecificCredentialId"
{-# INLINEABLE dsscServiceSpecificCredentialId #-}
{-# DEPRECATED serviceSpecificCredentialId "Use generic-lens or generic-optics with 'serviceSpecificCredentialId' instead"  #-}

-- | The name of the IAM user associated with the service-specific credential. If this value is not specified, then the operation assumes the user whose credentials are used to call the operation.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsscUserName :: Lens.Lens' DeleteServiceSpecificCredential (Core.Maybe Types.UserNameType)
dsscUserName = Lens.field @"userName"
{-# INLINEABLE dsscUserName #-}
{-# DEPRECATED userName "Use generic-lens or generic-optics with 'userName' instead"  #-}

instance Core.ToQuery DeleteServiceSpecificCredential where
        toQuery DeleteServiceSpecificCredential{..}
          = Core.toQueryPair "Action"
              ("DeleteServiceSpecificCredential" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-05-08" :: Core.Text)
              Core.<>
              Core.toQueryPair "ServiceSpecificCredentialId"
                serviceSpecificCredentialId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "UserName") userName

instance Core.ToHeaders DeleteServiceSpecificCredential where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteServiceSpecificCredential where
        type Rs DeleteServiceSpecificCredential =
             DeleteServiceSpecificCredentialResponse
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
          = Response.receiveNull DeleteServiceSpecificCredentialResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteServiceSpecificCredentialResponse' smart constructor.
data DeleteServiceSpecificCredentialResponse = DeleteServiceSpecificCredentialResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteServiceSpecificCredentialResponse' value with any optional fields omitted.
mkDeleteServiceSpecificCredentialResponse
    :: DeleteServiceSpecificCredentialResponse
mkDeleteServiceSpecificCredentialResponse
  = DeleteServiceSpecificCredentialResponse'
