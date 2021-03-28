{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.MoveAccount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Moves an account from its current source parent root or organizational unit (OU) to the specified destination parent root or OU.
--
-- This operation can be called only from the organization's management account.
module Network.AWS.Organizations.MoveAccount
    (
    -- * Creating a request
      MoveAccount (..)
    , mkMoveAccount
    -- ** Request lenses
    , maAccountId
    , maSourceParentId
    , maDestinationParentId

    -- * Destructuring the response
    , MoveAccountResponse (..)
    , mkMoveAccountResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Organizations.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkMoveAccount' smart constructor.
data MoveAccount = MoveAccount'
  { accountId :: Types.AccountId
    -- ^ The unique identifier (ID) of the account that you want to move.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for an account ID string requires exactly 12 digits.
  , sourceParentId :: Types.SourceParentId
    -- ^ The unique identifier (ID) of the root or organizational unit that you want to move the account from.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a parent ID string requires one of the following:
--
--     * __Root__ - A string that begins with "r-" followed by from 4 to 32 lowercase letters or digits.
--
--
--     * __Organizational unit (OU)__ - A string that begins with "ou-" followed by from 4 to 32 lowercase letters or digits (the ID of the root that the OU is in). This string is followed by a second "-" dash and from 8 to 32 additional lowercase letters or digits.
--
--
  , destinationParentId :: Types.DestinationParentId
    -- ^ The unique identifier (ID) of the root or organizational unit that you want to move the account to.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a parent ID string requires one of the following:
--
--     * __Root__ - A string that begins with "r-" followed by from 4 to 32 lowercase letters or digits.
--
--
--     * __Organizational unit (OU)__ - A string that begins with "ou-" followed by from 4 to 32 lowercase letters or digits (the ID of the root that the OU is in). This string is followed by a second "-" dash and from 8 to 32 additional lowercase letters or digits.
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MoveAccount' value with any optional fields omitted.
mkMoveAccount
    :: Types.AccountId -- ^ 'accountId'
    -> Types.SourceParentId -- ^ 'sourceParentId'
    -> Types.DestinationParentId -- ^ 'destinationParentId'
    -> MoveAccount
mkMoveAccount accountId sourceParentId destinationParentId
  = MoveAccount'{accountId, sourceParentId, destinationParentId}

-- | The unique identifier (ID) of the account that you want to move.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for an account ID string requires exactly 12 digits.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maAccountId :: Lens.Lens' MoveAccount Types.AccountId
maAccountId = Lens.field @"accountId"
{-# INLINEABLE maAccountId #-}
{-# DEPRECATED accountId "Use generic-lens or generic-optics with 'accountId' instead"  #-}

-- | The unique identifier (ID) of the root or organizational unit that you want to move the account from.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a parent ID string requires one of the following:
--
--     * __Root__ - A string that begins with "r-" followed by from 4 to 32 lowercase letters or digits.
--
--
--     * __Organizational unit (OU)__ - A string that begins with "ou-" followed by from 4 to 32 lowercase letters or digits (the ID of the root that the OU is in). This string is followed by a second "-" dash and from 8 to 32 additional lowercase letters or digits.
--
--
--
-- /Note:/ Consider using 'sourceParentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maSourceParentId :: Lens.Lens' MoveAccount Types.SourceParentId
maSourceParentId = Lens.field @"sourceParentId"
{-# INLINEABLE maSourceParentId #-}
{-# DEPRECATED sourceParentId "Use generic-lens or generic-optics with 'sourceParentId' instead"  #-}

-- | The unique identifier (ID) of the root or organizational unit that you want to move the account to.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a parent ID string requires one of the following:
--
--     * __Root__ - A string that begins with "r-" followed by from 4 to 32 lowercase letters or digits.
--
--
--     * __Organizational unit (OU)__ - A string that begins with "ou-" followed by from 4 to 32 lowercase letters or digits (the ID of the root that the OU is in). This string is followed by a second "-" dash and from 8 to 32 additional lowercase letters or digits.
--
--
--
-- /Note:/ Consider using 'destinationParentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maDestinationParentId :: Lens.Lens' MoveAccount Types.DestinationParentId
maDestinationParentId = Lens.field @"destinationParentId"
{-# INLINEABLE maDestinationParentId #-}
{-# DEPRECATED destinationParentId "Use generic-lens or generic-optics with 'destinationParentId' instead"  #-}

instance Core.ToQuery MoveAccount where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders MoveAccount where
        toHeaders MoveAccount{..}
          = Core.pure
              ("X-Amz-Target", "AWSOrganizationsV20161128.MoveAccount")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON MoveAccount where
        toJSON MoveAccount{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("AccountId" Core..= accountId),
                  Core.Just ("SourceParentId" Core..= sourceParentId),
                  Core.Just ("DestinationParentId" Core..= destinationParentId)])

instance Core.AWSRequest MoveAccount where
        type Rs MoveAccount = MoveAccountResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull MoveAccountResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkMoveAccountResponse' smart constructor.
data MoveAccountResponse = MoveAccountResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MoveAccountResponse' value with any optional fields omitted.
mkMoveAccountResponse
    :: MoveAccountResponse
mkMoveAccountResponse = MoveAccountResponse'
