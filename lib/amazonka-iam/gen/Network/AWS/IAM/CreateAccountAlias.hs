{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.CreateAccountAlias
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an alias for your AWS account. For information about using an AWS account alias, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/AccountAlias.html Using an Alias for Your AWS Account ID> in the /IAM User Guide/ .
module Network.AWS.IAM.CreateAccountAlias
    (
    -- * Creating a request
      CreateAccountAlias (..)
    , mkCreateAccountAlias
    -- ** Request lenses
    , caaAccountAlias

    -- * Destructuring the response
    , CreateAccountAliasResponse (..)
    , mkCreateAccountAliasResponse
    ) where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateAccountAlias' smart constructor.
newtype CreateAccountAlias = CreateAccountAlias'
  { accountAlias :: Types.AccountAliasType
    -- ^ The account alias to create.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of lowercase letters, digits, and dashes. You cannot start or finish with a dash, nor can you have two dashes in a row.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateAccountAlias' value with any optional fields omitted.
mkCreateAccountAlias
    :: Types.AccountAliasType -- ^ 'accountAlias'
    -> CreateAccountAlias
mkCreateAccountAlias accountAlias
  = CreateAccountAlias'{accountAlias}

-- | The account alias to create.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of lowercase letters, digits, and dashes. You cannot start or finish with a dash, nor can you have two dashes in a row.
--
-- /Note:/ Consider using 'accountAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caaAccountAlias :: Lens.Lens' CreateAccountAlias Types.AccountAliasType
caaAccountAlias = Lens.field @"accountAlias"
{-# INLINEABLE caaAccountAlias #-}
{-# DEPRECATED accountAlias "Use generic-lens or generic-optics with 'accountAlias' instead"  #-}

instance Core.ToQuery CreateAccountAlias where
        toQuery CreateAccountAlias{..}
          = Core.toQueryPair "Action" ("CreateAccountAlias" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-05-08" :: Core.Text)
              Core.<> Core.toQueryPair "AccountAlias" accountAlias

instance Core.ToHeaders CreateAccountAlias where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateAccountAlias where
        type Rs CreateAccountAlias = CreateAccountAliasResponse
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
        parseResponse = Response.receiveNull CreateAccountAliasResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateAccountAliasResponse' smart constructor.
data CreateAccountAliasResponse = CreateAccountAliasResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateAccountAliasResponse' value with any optional fields omitted.
mkCreateAccountAliasResponse
    :: CreateAccountAliasResponse
mkCreateAccountAliasResponse = CreateAccountAliasResponse'
