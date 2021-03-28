{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.DeleteAccountAlias
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified AWS account alias. For information about using an AWS account alias, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/AccountAlias.html Using an Alias for Your AWS Account ID> in the /IAM User Guide/ .
module Network.AWS.IAM.DeleteAccountAlias
    (
    -- * Creating a request
      DeleteAccountAlias (..)
    , mkDeleteAccountAlias
    -- ** Request lenses
    , daaAccountAlias

    -- * Destructuring the response
    , DeleteAccountAliasResponse (..)
    , mkDeleteAccountAliasResponse
    ) where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteAccountAlias' smart constructor.
newtype DeleteAccountAlias = DeleteAccountAlias'
  { accountAlias :: Types.AccountAlias
    -- ^ The name of the account alias to delete.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of lowercase letters, digits, and dashes. You cannot start or finish with a dash, nor can you have two dashes in a row.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAccountAlias' value with any optional fields omitted.
mkDeleteAccountAlias
    :: Types.AccountAlias -- ^ 'accountAlias'
    -> DeleteAccountAlias
mkDeleteAccountAlias accountAlias
  = DeleteAccountAlias'{accountAlias}

-- | The name of the account alias to delete.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of lowercase letters, digits, and dashes. You cannot start or finish with a dash, nor can you have two dashes in a row.
--
-- /Note:/ Consider using 'accountAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daaAccountAlias :: Lens.Lens' DeleteAccountAlias Types.AccountAlias
daaAccountAlias = Lens.field @"accountAlias"
{-# INLINEABLE daaAccountAlias #-}
{-# DEPRECATED accountAlias "Use generic-lens or generic-optics with 'accountAlias' instead"  #-}

instance Core.ToQuery DeleteAccountAlias where
        toQuery DeleteAccountAlias{..}
          = Core.toQueryPair "Action" ("DeleteAccountAlias" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-05-08" :: Core.Text)
              Core.<> Core.toQueryPair "AccountAlias" accountAlias

instance Core.ToHeaders DeleteAccountAlias where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteAccountAlias where
        type Rs DeleteAccountAlias = DeleteAccountAliasResponse
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
        parseResponse = Response.receiveNull DeleteAccountAliasResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteAccountAliasResponse' smart constructor.
data DeleteAccountAliasResponse = DeleteAccountAliasResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAccountAliasResponse' value with any optional fields omitted.
mkDeleteAccountAliasResponse
    :: DeleteAccountAliasResponse
mkDeleteAccountAliasResponse = DeleteAccountAliasResponse'
