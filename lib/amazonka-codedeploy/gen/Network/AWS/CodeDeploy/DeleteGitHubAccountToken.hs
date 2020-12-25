{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.DeleteGitHubAccountToken
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a GitHub account connection.
module Network.AWS.CodeDeploy.DeleteGitHubAccountToken
  ( -- * Creating a request
    DeleteGitHubAccountToken (..),
    mkDeleteGitHubAccountToken,

    -- ** Request lenses
    dghatTokenName,

    -- * Destructuring the response
    DeleteGitHubAccountTokenResponse (..),
    mkDeleteGitHubAccountTokenResponse,

    -- ** Response lenses
    dghatrrsTokenName,
    dghatrrsResponseStatus,
  )
where

import qualified Network.AWS.CodeDeploy.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @DeleteGitHubAccount@ operation.
--
-- /See:/ 'mkDeleteGitHubAccountToken' smart constructor.
newtype DeleteGitHubAccountToken = DeleteGitHubAccountToken'
  { -- | The name of the GitHub account connection to delete.
    tokenName :: Core.Maybe Types.TokenName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteGitHubAccountToken' value with any optional fields omitted.
mkDeleteGitHubAccountToken ::
  DeleteGitHubAccountToken
mkDeleteGitHubAccountToken =
  DeleteGitHubAccountToken' {tokenName = Core.Nothing}

-- | The name of the GitHub account connection to delete.
--
-- /Note:/ Consider using 'tokenName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dghatTokenName :: Lens.Lens' DeleteGitHubAccountToken (Core.Maybe Types.TokenName)
dghatTokenName = Lens.field @"tokenName"
{-# DEPRECATED dghatTokenName "Use generic-lens or generic-optics with 'tokenName' instead." #-}

instance Core.FromJSON DeleteGitHubAccountToken where
  toJSON DeleteGitHubAccountToken {..} =
    Core.object
      (Core.catMaybes [("tokenName" Core..=) Core.<$> tokenName])

instance Core.AWSRequest DeleteGitHubAccountToken where
  type Rs DeleteGitHubAccountToken = DeleteGitHubAccountTokenResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "CodeDeploy_20141006.DeleteGitHubAccountToken")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteGitHubAccountTokenResponse'
            Core.<$> (x Core..:? "tokenName") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the output of a @DeleteGitHubAccountToken@ operation.
--
-- /See:/ 'mkDeleteGitHubAccountTokenResponse' smart constructor.
data DeleteGitHubAccountTokenResponse = DeleteGitHubAccountTokenResponse'
  { -- | The name of the GitHub account connection that was deleted.
    tokenName :: Core.Maybe Types.TokenName,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteGitHubAccountTokenResponse' value with any optional fields omitted.
mkDeleteGitHubAccountTokenResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteGitHubAccountTokenResponse
mkDeleteGitHubAccountTokenResponse responseStatus =
  DeleteGitHubAccountTokenResponse'
    { tokenName = Core.Nothing,
      responseStatus
    }

-- | The name of the GitHub account connection that was deleted.
--
-- /Note:/ Consider using 'tokenName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dghatrrsTokenName :: Lens.Lens' DeleteGitHubAccountTokenResponse (Core.Maybe Types.TokenName)
dghatrrsTokenName = Lens.field @"tokenName"
{-# DEPRECATED dghatrrsTokenName "Use generic-lens or generic-optics with 'tokenName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dghatrrsResponseStatus :: Lens.Lens' DeleteGitHubAccountTokenResponse Core.Int
dghatrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dghatrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
