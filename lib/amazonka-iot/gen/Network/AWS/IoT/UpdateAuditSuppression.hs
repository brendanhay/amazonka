{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.UpdateAuditSuppression
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a Device Defender audit suppression.
module Network.AWS.IoT.UpdateAuditSuppression
  ( -- * Creating a request
    UpdateAuditSuppression (..),
    mkUpdateAuditSuppression,

    -- ** Request lenses
    uasCheckName,
    uasResourceIdentifier,
    uasDescription,
    uasExpirationDate,
    uasSuppressIndefinitely,

    -- * Destructuring the response
    UpdateAuditSuppressionResponse (..),
    mkUpdateAuditSuppressionResponse,

    -- ** Response lenses
    uasrrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateAuditSuppression' smart constructor.
data UpdateAuditSuppression = UpdateAuditSuppression'
  { checkName :: Types.CheckName,
    resourceIdentifier :: Types.ResourceIdentifier,
    -- | The description of the audit suppression.
    description :: Core.Maybe Types.Description,
    -- | The expiration date (epoch timestamp in seconds) that you want the suppression to adhere to.
    expirationDate :: Core.Maybe Core.NominalDiffTime,
    -- | Indicates whether a suppression should exist indefinitely or not.
    suppressIndefinitely :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UpdateAuditSuppression' value with any optional fields omitted.
mkUpdateAuditSuppression ::
  -- | 'checkName'
  Types.CheckName ->
  -- | 'resourceIdentifier'
  Types.ResourceIdentifier ->
  UpdateAuditSuppression
mkUpdateAuditSuppression checkName resourceIdentifier =
  UpdateAuditSuppression'
    { checkName,
      resourceIdentifier,
      description = Core.Nothing,
      expirationDate = Core.Nothing,
      suppressIndefinitely = Core.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'checkName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasCheckName :: Lens.Lens' UpdateAuditSuppression Types.CheckName
uasCheckName = Lens.field @"checkName"
{-# DEPRECATED uasCheckName "Use generic-lens or generic-optics with 'checkName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'resourceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasResourceIdentifier :: Lens.Lens' UpdateAuditSuppression Types.ResourceIdentifier
uasResourceIdentifier = Lens.field @"resourceIdentifier"
{-# DEPRECATED uasResourceIdentifier "Use generic-lens or generic-optics with 'resourceIdentifier' instead." #-}

-- | The description of the audit suppression.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasDescription :: Lens.Lens' UpdateAuditSuppression (Core.Maybe Types.Description)
uasDescription = Lens.field @"description"
{-# DEPRECATED uasDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The expiration date (epoch timestamp in seconds) that you want the suppression to adhere to.
--
-- /Note:/ Consider using 'expirationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasExpirationDate :: Lens.Lens' UpdateAuditSuppression (Core.Maybe Core.NominalDiffTime)
uasExpirationDate = Lens.field @"expirationDate"
{-# DEPRECATED uasExpirationDate "Use generic-lens or generic-optics with 'expirationDate' instead." #-}

-- | Indicates whether a suppression should exist indefinitely or not.
--
-- /Note:/ Consider using 'suppressIndefinitely' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasSuppressIndefinitely :: Lens.Lens' UpdateAuditSuppression (Core.Maybe Core.Bool)
uasSuppressIndefinitely = Lens.field @"suppressIndefinitely"
{-# DEPRECATED uasSuppressIndefinitely "Use generic-lens or generic-optics with 'suppressIndefinitely' instead." #-}

instance Core.FromJSON UpdateAuditSuppression where
  toJSON UpdateAuditSuppression {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("checkName" Core..= checkName),
            Core.Just ("resourceIdentifier" Core..= resourceIdentifier),
            ("description" Core..=) Core.<$> description,
            ("expirationDate" Core..=) Core.<$> expirationDate,
            ("suppressIndefinitely" Core..=) Core.<$> suppressIndefinitely
          ]
      )

instance Core.AWSRequest UpdateAuditSuppression where
  type Rs UpdateAuditSuppression = UpdateAuditSuppressionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PATCH,
        Core._rqPath = Core.rawPath "/audit/suppressions/update",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateAuditSuppressionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateAuditSuppressionResponse' smart constructor.
newtype UpdateAuditSuppressionResponse = UpdateAuditSuppressionResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateAuditSuppressionResponse' value with any optional fields omitted.
mkUpdateAuditSuppressionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateAuditSuppressionResponse
mkUpdateAuditSuppressionResponse responseStatus =
  UpdateAuditSuppressionResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasrrsResponseStatus :: Lens.Lens' UpdateAuditSuppressionResponse Core.Int
uasrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED uasrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
