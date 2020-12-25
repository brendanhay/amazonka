{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.UnlinkIdentity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Unlinks a federated identity from an existing account. Unlinked logins will be considered new identities next time they are seen. Removing the last linked login will make this identity inaccessible.
--
-- This is a public API. You do not need any credentials to call this API.
module Network.AWS.CognitoIdentity.UnlinkIdentity
  ( -- * Creating a request
    UnlinkIdentity (..),
    mkUnlinkIdentity,

    -- ** Request lenses
    uiIdentityId,
    uiLogins,
    uiLoginsToRemove,

    -- * Destructuring the response
    UnlinkIdentityResponse (..),
    mkUnlinkIdentityResponse,
  )
where

import qualified Network.AWS.CognitoIdentity.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Input to the UnlinkIdentity action.
--
-- /See:/ 'mkUnlinkIdentity' smart constructor.
data UnlinkIdentity = UnlinkIdentity'
  { -- | A unique identifier in the format REGION:GUID.
    identityId :: Types.IdentityId,
    -- | A set of optional name-value pairs that map provider names to provider tokens.
    logins :: Core.HashMap Types.IdentityProviderName Types.IdentityProviderToken,
    -- | Provider names to unlink from this identity.
    loginsToRemove :: [Types.IdentityProviderName]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UnlinkIdentity' value with any optional fields omitted.
mkUnlinkIdentity ::
  -- | 'identityId'
  Types.IdentityId ->
  UnlinkIdentity
mkUnlinkIdentity identityId =
  UnlinkIdentity'
    { identityId,
      logins = Core.mempty,
      loginsToRemove = Core.mempty
    }

-- | A unique identifier in the format REGION:GUID.
--
-- /Note:/ Consider using 'identityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiIdentityId :: Lens.Lens' UnlinkIdentity Types.IdentityId
uiIdentityId = Lens.field @"identityId"
{-# DEPRECATED uiIdentityId "Use generic-lens or generic-optics with 'identityId' instead." #-}

-- | A set of optional name-value pairs that map provider names to provider tokens.
--
-- /Note:/ Consider using 'logins' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiLogins :: Lens.Lens' UnlinkIdentity (Core.HashMap Types.IdentityProviderName Types.IdentityProviderToken)
uiLogins = Lens.field @"logins"
{-# DEPRECATED uiLogins "Use generic-lens or generic-optics with 'logins' instead." #-}

-- | Provider names to unlink from this identity.
--
-- /Note:/ Consider using 'loginsToRemove' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiLoginsToRemove :: Lens.Lens' UnlinkIdentity [Types.IdentityProviderName]
uiLoginsToRemove = Lens.field @"loginsToRemove"
{-# DEPRECATED uiLoginsToRemove "Use generic-lens or generic-optics with 'loginsToRemove' instead." #-}

instance Core.FromJSON UnlinkIdentity where
  toJSON UnlinkIdentity {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("IdentityId" Core..= identityId),
            Core.Just ("Logins" Core..= logins),
            Core.Just ("LoginsToRemove" Core..= loginsToRemove)
          ]
      )

instance Core.AWSRequest UnlinkIdentity where
  type Rs UnlinkIdentity = UnlinkIdentityResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSCognitoIdentityService.UnlinkIdentity")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull UnlinkIdentityResponse'

-- | /See:/ 'mkUnlinkIdentityResponse' smart constructor.
data UnlinkIdentityResponse = UnlinkIdentityResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UnlinkIdentityResponse' value with any optional fields omitted.
mkUnlinkIdentityResponse ::
  UnlinkIdentityResponse
mkUnlinkIdentityResponse = UnlinkIdentityResponse'
