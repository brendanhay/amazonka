{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.DeleteAccountPasswordPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the password policy for the AWS account. There are no parameters.
module Network.AWS.IAM.DeleteAccountPasswordPolicy
    (
    -- * Creating a request
      DeleteAccountPasswordPolicy (..)
    , mkDeleteAccountPasswordPolicy

    -- * Destructuring the response
    , DeleteAccountPasswordPolicyResponse (..)
    , mkDeleteAccountPasswordPolicyResponse
    ) where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteAccountPasswordPolicy' smart constructor.
data DeleteAccountPasswordPolicy = DeleteAccountPasswordPolicy'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAccountPasswordPolicy' value with any optional fields omitted.
mkDeleteAccountPasswordPolicy
    :: DeleteAccountPasswordPolicy
mkDeleteAccountPasswordPolicy = DeleteAccountPasswordPolicy'

instance Core.ToQuery DeleteAccountPasswordPolicy where
        toQuery DeleteAccountPasswordPolicy{..}
          = Core.toQueryPair "Action"
              ("DeleteAccountPasswordPolicy" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-05-08" :: Core.Text)

instance Core.ToHeaders DeleteAccountPasswordPolicy where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteAccountPasswordPolicy where
        type Rs DeleteAccountPasswordPolicy =
             DeleteAccountPasswordPolicyResponse
        toRequest x@_
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
          = Response.receiveNull DeleteAccountPasswordPolicyResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteAccountPasswordPolicyResponse' smart constructor.
data DeleteAccountPasswordPolicyResponse = DeleteAccountPasswordPolicyResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAccountPasswordPolicyResponse' value with any optional fields omitted.
mkDeleteAccountPasswordPolicyResponse
    :: DeleteAccountPasswordPolicyResponse
mkDeleteAccountPasswordPolicyResponse
  = DeleteAccountPasswordPolicyResponse'
