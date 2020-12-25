{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DescribeAuditSuppression
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a Device Defender audit suppression.
module Network.AWS.IoT.DescribeAuditSuppression
  ( -- * Creating a request
    DescribeAuditSuppression (..),
    mkDescribeAuditSuppression,

    -- ** Request lenses
    dCheckName,
    dResourceIdentifier,

    -- * Destructuring the response
    DescribeAuditSuppressionResponse (..),
    mkDescribeAuditSuppressionResponse,

    -- ** Response lenses
    dasrfrsCheckName,
    dasrfrsDescription,
    dasrfrsExpirationDate,
    dasrfrsResourceIdentifier,
    dasrfrsSuppressIndefinitely,
    dasrfrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeAuditSuppression' smart constructor.
data DescribeAuditSuppression = DescribeAuditSuppression'
  { checkName :: Types.AuditCheckName,
    resourceIdentifier :: Types.ResourceIdentifier
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAuditSuppression' value with any optional fields omitted.
mkDescribeAuditSuppression ::
  -- | 'checkName'
  Types.AuditCheckName ->
  -- | 'resourceIdentifier'
  Types.ResourceIdentifier ->
  DescribeAuditSuppression
mkDescribeAuditSuppression checkName resourceIdentifier =
  DescribeAuditSuppression' {checkName, resourceIdentifier}

-- | Undocumented field.
--
-- /Note:/ Consider using 'checkName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCheckName :: Lens.Lens' DescribeAuditSuppression Types.AuditCheckName
dCheckName = Lens.field @"checkName"
{-# DEPRECATED dCheckName "Use generic-lens or generic-optics with 'checkName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'resourceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dResourceIdentifier :: Lens.Lens' DescribeAuditSuppression Types.ResourceIdentifier
dResourceIdentifier = Lens.field @"resourceIdentifier"
{-# DEPRECATED dResourceIdentifier "Use generic-lens or generic-optics with 'resourceIdentifier' instead." #-}

instance Core.FromJSON DescribeAuditSuppression where
  toJSON DescribeAuditSuppression {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("checkName" Core..= checkName),
            Core.Just ("resourceIdentifier" Core..= resourceIdentifier)
          ]
      )

instance Core.AWSRequest DescribeAuditSuppression where
  type Rs DescribeAuditSuppression = DescribeAuditSuppressionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/audit/suppressions/describe",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAuditSuppressionResponse'
            Core.<$> (x Core..:? "checkName")
            Core.<*> (x Core..:? "description")
            Core.<*> (x Core..:? "expirationDate")
            Core.<*> (x Core..:? "resourceIdentifier")
            Core.<*> (x Core..:? "suppressIndefinitely")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeAuditSuppressionResponse' smart constructor.
data DescribeAuditSuppressionResponse = DescribeAuditSuppressionResponse'
  { checkName :: Core.Maybe Types.AuditCheckName,
    -- | The description of the audit suppression.
    description :: Core.Maybe Types.Description,
    -- | The epoch timestamp in seconds at which this suppression expires.
    expirationDate :: Core.Maybe Core.NominalDiffTime,
    resourceIdentifier :: Core.Maybe Types.ResourceIdentifier,
    -- | Indicates whether a suppression should exist indefinitely or not.
    suppressIndefinitely :: Core.Maybe Core.Bool,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeAuditSuppressionResponse' value with any optional fields omitted.
mkDescribeAuditSuppressionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeAuditSuppressionResponse
mkDescribeAuditSuppressionResponse responseStatus =
  DescribeAuditSuppressionResponse'
    { checkName = Core.Nothing,
      description = Core.Nothing,
      expirationDate = Core.Nothing,
      resourceIdentifier = Core.Nothing,
      suppressIndefinitely = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'checkName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasrfrsCheckName :: Lens.Lens' DescribeAuditSuppressionResponse (Core.Maybe Types.AuditCheckName)
dasrfrsCheckName = Lens.field @"checkName"
{-# DEPRECATED dasrfrsCheckName "Use generic-lens or generic-optics with 'checkName' instead." #-}

-- | The description of the audit suppression.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasrfrsDescription :: Lens.Lens' DescribeAuditSuppressionResponse (Core.Maybe Types.Description)
dasrfrsDescription = Lens.field @"description"
{-# DEPRECATED dasrfrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The epoch timestamp in seconds at which this suppression expires.
--
-- /Note:/ Consider using 'expirationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasrfrsExpirationDate :: Lens.Lens' DescribeAuditSuppressionResponse (Core.Maybe Core.NominalDiffTime)
dasrfrsExpirationDate = Lens.field @"expirationDate"
{-# DEPRECATED dasrfrsExpirationDate "Use generic-lens or generic-optics with 'expirationDate' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'resourceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasrfrsResourceIdentifier :: Lens.Lens' DescribeAuditSuppressionResponse (Core.Maybe Types.ResourceIdentifier)
dasrfrsResourceIdentifier = Lens.field @"resourceIdentifier"
{-# DEPRECATED dasrfrsResourceIdentifier "Use generic-lens or generic-optics with 'resourceIdentifier' instead." #-}

-- | Indicates whether a suppression should exist indefinitely or not.
--
-- /Note:/ Consider using 'suppressIndefinitely' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasrfrsSuppressIndefinitely :: Lens.Lens' DescribeAuditSuppressionResponse (Core.Maybe Core.Bool)
dasrfrsSuppressIndefinitely = Lens.field @"suppressIndefinitely"
{-# DEPRECATED dasrfrsSuppressIndefinitely "Use generic-lens or generic-optics with 'suppressIndefinitely' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasrfrsResponseStatus :: Lens.Lens' DescribeAuditSuppressionResponse Core.Int
dasrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dasrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
