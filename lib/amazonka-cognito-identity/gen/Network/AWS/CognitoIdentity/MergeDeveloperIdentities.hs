{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.MergeDeveloperIdentities
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Merges two users having different @IdentityId@ s, existing in the same identity pool, and identified by the same developer provider. You can use this action to request that discrete users be merged and identified as a single user in the Cognito environment. Cognito associates the given source user (@SourceUserIdentifier@ ) with the @IdentityId@ of the @DestinationUserIdentifier@ . Only developer-authenticated users can be merged. If the users to be merged are associated with the same public provider, but as two different users, an exception will be thrown.
--
-- The number of linked logins is limited to 20. So, the number of linked logins for the source user, @SourceUserIdentifier@ , and the destination user, @DestinationUserIdentifier@ , together should not be larger than 20. Otherwise, an exception will be thrown.
-- You must use AWS Developer credentials to call this API.
module Network.AWS.CognitoIdentity.MergeDeveloperIdentities
  ( -- * Creating a request
    MergeDeveloperIdentities (..),
    mkMergeDeveloperIdentities,

    -- ** Request lenses
    mdiSourceUserIdentifier,
    mdiDestinationUserIdentifier,
    mdiDeveloperProviderName,
    mdiIdentityPoolId,

    -- * Destructuring the response
    MergeDeveloperIdentitiesResponse (..),
    mkMergeDeveloperIdentitiesResponse,

    -- ** Response lenses
    mdirrsIdentityId,
    mdirrsResponseStatus,
  )
where

import qualified Network.AWS.CognitoIdentity.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Input to the @MergeDeveloperIdentities@ action.
--
-- /See:/ 'mkMergeDeveloperIdentities' smart constructor.
data MergeDeveloperIdentities = MergeDeveloperIdentities'
  { -- | User identifier for the source user. The value should be a @DeveloperUserIdentifier@ .
    sourceUserIdentifier :: Types.DeveloperUserIdentifier,
    -- | User identifier for the destination user. The value should be a @DeveloperUserIdentifier@ .
    destinationUserIdentifier :: Types.DeveloperUserIdentifier,
    -- | The "domain" by which Cognito will refer to your users. This is a (pseudo) domain name that you provide while creating an identity pool. This name acts as a placeholder that allows your backend and the Cognito service to communicate about the developer provider. For the @DeveloperProviderName@ , you can use letters as well as period (.), underscore (_), and dash (-).
    developerProviderName :: Types.DeveloperProviderName,
    -- | An identity pool ID in the format REGION:GUID.
    identityPoolId :: Types.IdentityPoolId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MergeDeveloperIdentities' value with any optional fields omitted.
mkMergeDeveloperIdentities ::
  -- | 'sourceUserIdentifier'
  Types.DeveloperUserIdentifier ->
  -- | 'destinationUserIdentifier'
  Types.DeveloperUserIdentifier ->
  -- | 'developerProviderName'
  Types.DeveloperProviderName ->
  -- | 'identityPoolId'
  Types.IdentityPoolId ->
  MergeDeveloperIdentities
mkMergeDeveloperIdentities
  sourceUserIdentifier
  destinationUserIdentifier
  developerProviderName
  identityPoolId =
    MergeDeveloperIdentities'
      { sourceUserIdentifier,
        destinationUserIdentifier,
        developerProviderName,
        identityPoolId
      }

-- | User identifier for the source user. The value should be a @DeveloperUserIdentifier@ .
--
-- /Note:/ Consider using 'sourceUserIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdiSourceUserIdentifier :: Lens.Lens' MergeDeveloperIdentities Types.DeveloperUserIdentifier
mdiSourceUserIdentifier = Lens.field @"sourceUserIdentifier"
{-# DEPRECATED mdiSourceUserIdentifier "Use generic-lens or generic-optics with 'sourceUserIdentifier' instead." #-}

-- | User identifier for the destination user. The value should be a @DeveloperUserIdentifier@ .
--
-- /Note:/ Consider using 'destinationUserIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdiDestinationUserIdentifier :: Lens.Lens' MergeDeveloperIdentities Types.DeveloperUserIdentifier
mdiDestinationUserIdentifier = Lens.field @"destinationUserIdentifier"
{-# DEPRECATED mdiDestinationUserIdentifier "Use generic-lens or generic-optics with 'destinationUserIdentifier' instead." #-}

-- | The "domain" by which Cognito will refer to your users. This is a (pseudo) domain name that you provide while creating an identity pool. This name acts as a placeholder that allows your backend and the Cognito service to communicate about the developer provider. For the @DeveloperProviderName@ , you can use letters as well as period (.), underscore (_), and dash (-).
--
-- /Note:/ Consider using 'developerProviderName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdiDeveloperProviderName :: Lens.Lens' MergeDeveloperIdentities Types.DeveloperProviderName
mdiDeveloperProviderName = Lens.field @"developerProviderName"
{-# DEPRECATED mdiDeveloperProviderName "Use generic-lens or generic-optics with 'developerProviderName' instead." #-}

-- | An identity pool ID in the format REGION:GUID.
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdiIdentityPoolId :: Lens.Lens' MergeDeveloperIdentities Types.IdentityPoolId
mdiIdentityPoolId = Lens.field @"identityPoolId"
{-# DEPRECATED mdiIdentityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead." #-}

instance Core.FromJSON MergeDeveloperIdentities where
  toJSON MergeDeveloperIdentities {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("SourceUserIdentifier" Core..= sourceUserIdentifier),
            Core.Just
              ("DestinationUserIdentifier" Core..= destinationUserIdentifier),
            Core.Just ("DeveloperProviderName" Core..= developerProviderName),
            Core.Just ("IdentityPoolId" Core..= identityPoolId)
          ]
      )

instance Core.AWSRequest MergeDeveloperIdentities where
  type Rs MergeDeveloperIdentities = MergeDeveloperIdentitiesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSCognitoIdentityService.MergeDeveloperIdentities"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          MergeDeveloperIdentitiesResponse'
            Core.<$> (x Core..:? "IdentityId") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Returned in response to a successful @MergeDeveloperIdentities@ action.
--
-- /See:/ 'mkMergeDeveloperIdentitiesResponse' smart constructor.
data MergeDeveloperIdentitiesResponse = MergeDeveloperIdentitiesResponse'
  { -- | A unique identifier in the format REGION:GUID.
    identityId :: Core.Maybe Types.IdentityId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MergeDeveloperIdentitiesResponse' value with any optional fields omitted.
mkMergeDeveloperIdentitiesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  MergeDeveloperIdentitiesResponse
mkMergeDeveloperIdentitiesResponse responseStatus =
  MergeDeveloperIdentitiesResponse'
    { identityId = Core.Nothing,
      responseStatus
    }

-- | A unique identifier in the format REGION:GUID.
--
-- /Note:/ Consider using 'identityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdirrsIdentityId :: Lens.Lens' MergeDeveloperIdentitiesResponse (Core.Maybe Types.IdentityId)
mdirrsIdentityId = Lens.field @"identityId"
{-# DEPRECATED mdirrsIdentityId "Use generic-lens or generic-optics with 'identityId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdirrsResponseStatus :: Lens.Lens' MergeDeveloperIdentitiesResponse Core.Int
mdirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED mdirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
