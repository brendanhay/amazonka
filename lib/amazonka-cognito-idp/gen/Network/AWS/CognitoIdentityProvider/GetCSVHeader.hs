{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.GetCSVHeader
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the header information for the .csv file to be used as input for the user import job.
module Network.AWS.CognitoIdentityProvider.GetCSVHeader
  ( -- * Creating a request
    GetCSVHeader (..),
    mkGetCSVHeader,

    -- ** Request lenses
    gcsvhUserPoolId,

    -- * Destructuring the response
    GetCSVHeaderResponse (..),
    mkGetCSVHeaderResponse,

    -- ** Response lenses
    gcsvhrrsCSVHeader,
    gcsvhrrsUserPoolId,
    gcsvhrrsResponseStatus,
  )
where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to get the header information for the .csv file for the user import job.
--
-- /See:/ 'mkGetCSVHeader' smart constructor.
newtype GetCSVHeader = GetCSVHeader'
  { -- | The user pool ID for the user pool that the users are to be imported into.
    userPoolId :: Types.UserPoolId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetCSVHeader' value with any optional fields omitted.
mkGetCSVHeader ::
  -- | 'userPoolId'
  Types.UserPoolId ->
  GetCSVHeader
mkGetCSVHeader userPoolId = GetCSVHeader' {userPoolId}

-- | The user pool ID for the user pool that the users are to be imported into.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsvhUserPoolId :: Lens.Lens' GetCSVHeader Types.UserPoolId
gcsvhUserPoolId = Lens.field @"userPoolId"
{-# DEPRECATED gcsvhUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

instance Core.FromJSON GetCSVHeader where
  toJSON GetCSVHeader {..} =
    Core.object
      (Core.catMaybes [Core.Just ("UserPoolId" Core..= userPoolId)])

instance Core.AWSRequest GetCSVHeader where
  type Rs GetCSVHeader = GetCSVHeaderResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSCognitoIdentityProviderService.GetCSVHeader")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCSVHeaderResponse'
            Core.<$> (x Core..:? "CSVHeader")
            Core.<*> (x Core..:? "UserPoolId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the response from the server to the request to get the header information for the .csv file for the user import job.
--
-- /See:/ 'mkGetCSVHeaderResponse' smart constructor.
data GetCSVHeaderResponse = GetCSVHeaderResponse'
  { -- | The header information for the .csv file for the user import job.
    cSVHeader :: Core.Maybe [Types.StringType],
    -- | The user pool ID for the user pool that the users are to be imported into.
    userPoolId :: Core.Maybe Types.UserPoolId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCSVHeaderResponse' value with any optional fields omitted.
mkGetCSVHeaderResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetCSVHeaderResponse
mkGetCSVHeaderResponse responseStatus =
  GetCSVHeaderResponse'
    { cSVHeader = Core.Nothing,
      userPoolId = Core.Nothing,
      responseStatus
    }

-- | The header information for the .csv file for the user import job.
--
-- /Note:/ Consider using 'cSVHeader' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsvhrrsCSVHeader :: Lens.Lens' GetCSVHeaderResponse (Core.Maybe [Types.StringType])
gcsvhrrsCSVHeader = Lens.field @"cSVHeader"
{-# DEPRECATED gcsvhrrsCSVHeader "Use generic-lens or generic-optics with 'cSVHeader' instead." #-}

-- | The user pool ID for the user pool that the users are to be imported into.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsvhrrsUserPoolId :: Lens.Lens' GetCSVHeaderResponse (Core.Maybe Types.UserPoolId)
gcsvhrrsUserPoolId = Lens.field @"userPoolId"
{-# DEPRECATED gcsvhrrsUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsvhrrsResponseStatus :: Lens.Lens' GetCSVHeaderResponse Core.Int
gcsvhrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gcsvhrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
