{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.PutAppsList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an AWS Firewall Manager applications list.
module Network.AWS.FMS.PutAppsList
  ( -- * Creating a request
    PutAppsList (..),
    mkPutAppsList,

    -- ** Request lenses
    palAppsList,
    palTagList,

    -- * Destructuring the response
    PutAppsListResponse (..),
    mkPutAppsListResponse,

    -- ** Response lenses
    palrrsAppsList,
    palrrsAppsListArn,
    palrrsResponseStatus,
  )
where

import qualified Network.AWS.FMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutAppsList' smart constructor.
data PutAppsList = PutAppsList'
  { -- | The details of the AWS Firewall Manager applications list to be created.
    appsList :: Types.AppsListData,
    -- | The tags associated with the resource.
    tagList :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'PutAppsList' value with any optional fields omitted.
mkPutAppsList ::
  -- | 'appsList'
  Types.AppsListData ->
  PutAppsList
mkPutAppsList appsList =
  PutAppsList' {appsList, tagList = Core.Nothing}

-- | The details of the AWS Firewall Manager applications list to be created.
--
-- /Note:/ Consider using 'appsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
palAppsList :: Lens.Lens' PutAppsList Types.AppsListData
palAppsList = Lens.field @"appsList"
{-# DEPRECATED palAppsList "Use generic-lens or generic-optics with 'appsList' instead." #-}

-- | The tags associated with the resource.
--
-- /Note:/ Consider using 'tagList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
palTagList :: Lens.Lens' PutAppsList (Core.Maybe [Types.Tag])
palTagList = Lens.field @"tagList"
{-# DEPRECATED palTagList "Use generic-lens or generic-optics with 'tagList' instead." #-}

instance Core.FromJSON PutAppsList where
  toJSON PutAppsList {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("AppsList" Core..= appsList),
            ("TagList" Core..=) Core.<$> tagList
          ]
      )

instance Core.AWSRequest PutAppsList where
  type Rs PutAppsList = PutAppsListResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSFMS_20180101.PutAppsList")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          PutAppsListResponse'
            Core.<$> (x Core..:? "AppsList")
            Core.<*> (x Core..:? "AppsListArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkPutAppsListResponse' smart constructor.
data PutAppsListResponse = PutAppsListResponse'
  { -- | The details of the AWS Firewall Manager applications list.
    appsList :: Core.Maybe Types.AppsListData,
    -- | The Amazon Resource Name (ARN) of the applications list.
    appsListArn :: Core.Maybe Types.ResourceArn,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'PutAppsListResponse' value with any optional fields omitted.
mkPutAppsListResponse ::
  -- | 'responseStatus'
  Core.Int ->
  PutAppsListResponse
mkPutAppsListResponse responseStatus =
  PutAppsListResponse'
    { appsList = Core.Nothing,
      appsListArn = Core.Nothing,
      responseStatus
    }

-- | The details of the AWS Firewall Manager applications list.
--
-- /Note:/ Consider using 'appsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
palrrsAppsList :: Lens.Lens' PutAppsListResponse (Core.Maybe Types.AppsListData)
palrrsAppsList = Lens.field @"appsList"
{-# DEPRECATED palrrsAppsList "Use generic-lens or generic-optics with 'appsList' instead." #-}

-- | The Amazon Resource Name (ARN) of the applications list.
--
-- /Note:/ Consider using 'appsListArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
palrrsAppsListArn :: Lens.Lens' PutAppsListResponse (Core.Maybe Types.ResourceArn)
palrrsAppsListArn = Lens.field @"appsListArn"
{-# DEPRECATED palrrsAppsListArn "Use generic-lens or generic-optics with 'appsListArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
palrrsResponseStatus :: Lens.Lens' PutAppsListResponse Core.Int
palrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED palrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
