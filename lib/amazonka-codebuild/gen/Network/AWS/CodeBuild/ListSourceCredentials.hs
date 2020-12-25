{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.ListSourceCredentials
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of @SourceCredentialsInfo@ objects.
module Network.AWS.CodeBuild.ListSourceCredentials
  ( -- * Creating a request
    ListSourceCredentials (..),
    mkListSourceCredentials,

    -- * Destructuring the response
    ListSourceCredentialsResponse (..),
    mkListSourceCredentialsResponse,

    -- ** Response lenses
    lscrrsSourceCredentialsInfos,
    lscrrsResponseStatus,
  )
where

import qualified Network.AWS.CodeBuild.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListSourceCredentials' smart constructor.
data ListSourceCredentials = ListSourceCredentials'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListSourceCredentials' value with any optional fields omitted.
mkListSourceCredentials ::
  ListSourceCredentials
mkListSourceCredentials = ListSourceCredentials'

instance Core.FromJSON ListSourceCredentials where
  toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest ListSourceCredentials where
  type Rs ListSourceCredentials = ListSourceCredentialsResponse
  request x@_ =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "CodeBuild_20161006.ListSourceCredentials")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSourceCredentialsResponse'
            Core.<$> (x Core..:? "sourceCredentialsInfos")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkListSourceCredentialsResponse' smart constructor.
data ListSourceCredentialsResponse = ListSourceCredentialsResponse'
  { -- | A list of @SourceCredentialsInfo@ objects. Each @SourceCredentialsInfo@ object includes the authentication type, token ARN, and type of source provider for one set of credentials.
    sourceCredentialsInfos :: Core.Maybe [Types.SourceCredentialsInfo],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListSourceCredentialsResponse' value with any optional fields omitted.
mkListSourceCredentialsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListSourceCredentialsResponse
mkListSourceCredentialsResponse responseStatus =
  ListSourceCredentialsResponse'
    { sourceCredentialsInfos =
        Core.Nothing,
      responseStatus
    }

-- | A list of @SourceCredentialsInfo@ objects. Each @SourceCredentialsInfo@ object includes the authentication type, token ARN, and type of source provider for one set of credentials.
--
-- /Note:/ Consider using 'sourceCredentialsInfos' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lscrrsSourceCredentialsInfos :: Lens.Lens' ListSourceCredentialsResponse (Core.Maybe [Types.SourceCredentialsInfo])
lscrrsSourceCredentialsInfos = Lens.field @"sourceCredentialsInfos"
{-# DEPRECATED lscrrsSourceCredentialsInfos "Use generic-lens or generic-optics with 'sourceCredentialsInfos' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lscrrsResponseStatus :: Lens.Lens' ListSourceCredentialsResponse Core.Int
lscrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lscrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
