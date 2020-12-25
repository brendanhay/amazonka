{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.UnlinkDeveloperIdentity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Unlinks a @DeveloperUserIdentifier@ from an existing identity. Unlinked developer users will be considered new identities next time they are seen. If, for a given Cognito identity, you remove all federated identities as well as the developer user identifier, the Cognito identity becomes inaccessible.
--
-- You must use AWS Developer credentials to call this API.
module Network.AWS.CognitoIdentity.UnlinkDeveloperIdentity
  ( -- * Creating a request
    UnlinkDeveloperIdentity (..),
    mkUnlinkDeveloperIdentity,

    -- ** Request lenses
    udiIdentityId,
    udiIdentityPoolId,
    udiDeveloperProviderName,
    udiDeveloperUserIdentifier,

    -- * Destructuring the response
    UnlinkDeveloperIdentityResponse (..),
    mkUnlinkDeveloperIdentityResponse,
  )
where

import qualified Network.AWS.CognitoIdentity.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Input to the @UnlinkDeveloperIdentity@ action.
--
-- /See:/ 'mkUnlinkDeveloperIdentity' smart constructor.
data UnlinkDeveloperIdentity = UnlinkDeveloperIdentity'
  { -- | A unique identifier in the format REGION:GUID.
    identityId :: Types.IdentityId,
    -- | An identity pool ID in the format REGION:GUID.
    identityPoolId :: Types.IdentityPoolId,
    -- | The "domain" by which Cognito will refer to your users.
    developerProviderName :: Types.DeveloperProviderName,
    -- | A unique ID used by your backend authentication process to identify a user.
    developerUserIdentifier :: Types.DeveloperUserIdentifier
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UnlinkDeveloperIdentity' value with any optional fields omitted.
mkUnlinkDeveloperIdentity ::
  -- | 'identityId'
  Types.IdentityId ->
  -- | 'identityPoolId'
  Types.IdentityPoolId ->
  -- | 'developerProviderName'
  Types.DeveloperProviderName ->
  -- | 'developerUserIdentifier'
  Types.DeveloperUserIdentifier ->
  UnlinkDeveloperIdentity
mkUnlinkDeveloperIdentity
  identityId
  identityPoolId
  developerProviderName
  developerUserIdentifier =
    UnlinkDeveloperIdentity'
      { identityId,
        identityPoolId,
        developerProviderName,
        developerUserIdentifier
      }

-- | A unique identifier in the format REGION:GUID.
--
-- /Note:/ Consider using 'identityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udiIdentityId :: Lens.Lens' UnlinkDeveloperIdentity Types.IdentityId
udiIdentityId = Lens.field @"identityId"
{-# DEPRECATED udiIdentityId "Use generic-lens or generic-optics with 'identityId' instead." #-}

-- | An identity pool ID in the format REGION:GUID.
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udiIdentityPoolId :: Lens.Lens' UnlinkDeveloperIdentity Types.IdentityPoolId
udiIdentityPoolId = Lens.field @"identityPoolId"
{-# DEPRECATED udiIdentityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead." #-}

-- | The "domain" by which Cognito will refer to your users.
--
-- /Note:/ Consider using 'developerProviderName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udiDeveloperProviderName :: Lens.Lens' UnlinkDeveloperIdentity Types.DeveloperProviderName
udiDeveloperProviderName = Lens.field @"developerProviderName"
{-# DEPRECATED udiDeveloperProviderName "Use generic-lens or generic-optics with 'developerProviderName' instead." #-}

-- | A unique ID used by your backend authentication process to identify a user.
--
-- /Note:/ Consider using 'developerUserIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udiDeveloperUserIdentifier :: Lens.Lens' UnlinkDeveloperIdentity Types.DeveloperUserIdentifier
udiDeveloperUserIdentifier = Lens.field @"developerUserIdentifier"
{-# DEPRECATED udiDeveloperUserIdentifier "Use generic-lens or generic-optics with 'developerUserIdentifier' instead." #-}

instance Core.FromJSON UnlinkDeveloperIdentity where
  toJSON UnlinkDeveloperIdentity {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("IdentityId" Core..= identityId),
            Core.Just ("IdentityPoolId" Core..= identityPoolId),
            Core.Just ("DeveloperProviderName" Core..= developerProviderName),
            Core.Just
              ("DeveloperUserIdentifier" Core..= developerUserIdentifier)
          ]
      )

instance Core.AWSRequest UnlinkDeveloperIdentity where
  type Rs UnlinkDeveloperIdentity = UnlinkDeveloperIdentityResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSCognitoIdentityService.UnlinkDeveloperIdentity"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull UnlinkDeveloperIdentityResponse'

-- | /See:/ 'mkUnlinkDeveloperIdentityResponse' smart constructor.
data UnlinkDeveloperIdentityResponse = UnlinkDeveloperIdentityResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UnlinkDeveloperIdentityResponse' value with any optional fields omitted.
mkUnlinkDeveloperIdentityResponse ::
  UnlinkDeveloperIdentityResponse
mkUnlinkDeveloperIdentityResponse =
  UnlinkDeveloperIdentityResponse'
