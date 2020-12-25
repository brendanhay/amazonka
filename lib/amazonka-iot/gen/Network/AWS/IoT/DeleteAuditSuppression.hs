{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DeleteAuditSuppression
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Device Defender audit suppression.
module Network.AWS.IoT.DeleteAuditSuppression
  ( -- * Creating a request
    DeleteAuditSuppression (..),
    mkDeleteAuditSuppression,

    -- ** Request lenses
    dasCheckName,
    dasResourceIdentifier,

    -- * Destructuring the response
    DeleteAuditSuppressionResponse (..),
    mkDeleteAuditSuppressionResponse,

    -- ** Response lenses
    dasrrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteAuditSuppression' smart constructor.
data DeleteAuditSuppression = DeleteAuditSuppression'
  { checkName :: Types.CheckName,
    resourceIdentifier :: Types.ResourceIdentifier
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAuditSuppression' value with any optional fields omitted.
mkDeleteAuditSuppression ::
  -- | 'checkName'
  Types.CheckName ->
  -- | 'resourceIdentifier'
  Types.ResourceIdentifier ->
  DeleteAuditSuppression
mkDeleteAuditSuppression checkName resourceIdentifier =
  DeleteAuditSuppression' {checkName, resourceIdentifier}

-- | Undocumented field.
--
-- /Note:/ Consider using 'checkName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasCheckName :: Lens.Lens' DeleteAuditSuppression Types.CheckName
dasCheckName = Lens.field @"checkName"
{-# DEPRECATED dasCheckName "Use generic-lens or generic-optics with 'checkName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'resourceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasResourceIdentifier :: Lens.Lens' DeleteAuditSuppression Types.ResourceIdentifier
dasResourceIdentifier = Lens.field @"resourceIdentifier"
{-# DEPRECATED dasResourceIdentifier "Use generic-lens or generic-optics with 'resourceIdentifier' instead." #-}

instance Core.FromJSON DeleteAuditSuppression where
  toJSON DeleteAuditSuppression {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("checkName" Core..= checkName),
            Core.Just ("resourceIdentifier" Core..= resourceIdentifier)
          ]
      )

instance Core.AWSRequest DeleteAuditSuppression where
  type Rs DeleteAuditSuppression = DeleteAuditSuppressionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/audit/suppressions/delete",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteAuditSuppressionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteAuditSuppressionResponse' smart constructor.
newtype DeleteAuditSuppressionResponse = DeleteAuditSuppressionResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAuditSuppressionResponse' value with any optional fields omitted.
mkDeleteAuditSuppressionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteAuditSuppressionResponse
mkDeleteAuditSuppressionResponse responseStatus =
  DeleteAuditSuppressionResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasrrsResponseStatus :: Lens.Lens' DeleteAuditSuppressionResponse Core.Int
dasrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dasrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
