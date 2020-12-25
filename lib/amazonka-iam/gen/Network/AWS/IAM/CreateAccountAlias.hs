{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    CreateAccountAlias (..),
    mkCreateAccountAlias,

    -- ** Request lenses
    caaAccountAlias,

    -- * Destructuring the response
    CreateAccountAliasResponse (..),
    mkCreateAccountAliasResponse,
  )
where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateAccountAlias' smart constructor.
newtype CreateAccountAlias = CreateAccountAlias'
  { -- | The account alias to create.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of lowercase letters, digits, and dashes. You cannot start or finish with a dash, nor can you have two dashes in a row.
    accountAlias :: Types.AccountAliasType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateAccountAlias' value with any optional fields omitted.
mkCreateAccountAlias ::
  -- | 'accountAlias'
  Types.AccountAliasType ->
  CreateAccountAlias
mkCreateAccountAlias accountAlias =
  CreateAccountAlias' {accountAlias}

-- | The account alias to create.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of lowercase letters, digits, and dashes. You cannot start or finish with a dash, nor can you have two dashes in a row.
--
-- /Note:/ Consider using 'accountAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caaAccountAlias :: Lens.Lens' CreateAccountAlias Types.AccountAliasType
caaAccountAlias = Lens.field @"accountAlias"
{-# DEPRECATED caaAccountAlias "Use generic-lens or generic-optics with 'accountAlias' instead." #-}

instance Core.AWSRequest CreateAccountAlias where
  type Rs CreateAccountAlias = CreateAccountAliasResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "CreateAccountAlias")
                Core.<> (Core.pure ("Version", "2010-05-08"))
                Core.<> (Core.toQueryValue "AccountAlias" accountAlias)
            )
      }
  response = Response.receiveNull CreateAccountAliasResponse'

-- | /See:/ 'mkCreateAccountAliasResponse' smart constructor.
data CreateAccountAliasResponse = CreateAccountAliasResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateAccountAliasResponse' value with any optional fields omitted.
mkCreateAccountAliasResponse ::
  CreateAccountAliasResponse
mkCreateAccountAliasResponse = CreateAccountAliasResponse'
