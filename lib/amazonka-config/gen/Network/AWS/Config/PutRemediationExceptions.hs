{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.PutRemediationExceptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A remediation exception is when a specific resource is no longer considered for auto-remediation. This API adds a new exception or updates an exisiting exception for a specific resource with a specific AWS Config rule.
module Network.AWS.Config.PutRemediationExceptions
  ( -- * Creating a request
    PutRemediationExceptions (..),
    mkPutRemediationExceptions,

    -- ** Request lenses
    preConfigRuleName,
    preResourceKeys,
    preExpirationTime,
    preMessage,

    -- * Destructuring the response
    PutRemediationExceptionsResponse (..),
    mkPutRemediationExceptionsResponse,

    -- ** Response lenses
    prerrsFailedBatches,
    prerrsResponseStatus,
  )
where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutRemediationExceptions' smart constructor.
data PutRemediationExceptions = PutRemediationExceptions'
  { -- | The name of the AWS Config rule for which you want to create remediation exception.
    configRuleName :: Types.ConfigRuleName,
    -- | An exception list of resource exception keys to be processed with the current request. AWS Config adds exception for each resource key. For example, AWS Config adds 3 exceptions for 3 resource keys.
    resourceKeys :: Core.NonEmpty Types.RemediationExceptionResourceKey,
    -- | The exception is automatically deleted after the expiration date.
    expirationTime :: Core.Maybe Core.NominalDiffTime,
    -- | The message contains an explanation of the exception.
    message :: Core.Maybe Types.StringWithCharLimit1024
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'PutRemediationExceptions' value with any optional fields omitted.
mkPutRemediationExceptions ::
  -- | 'configRuleName'
  Types.ConfigRuleName ->
  -- | 'resourceKeys'
  Core.NonEmpty Types.RemediationExceptionResourceKey ->
  PutRemediationExceptions
mkPutRemediationExceptions configRuleName resourceKeys =
  PutRemediationExceptions'
    { configRuleName,
      resourceKeys,
      expirationTime = Core.Nothing,
      message = Core.Nothing
    }

-- | The name of the AWS Config rule for which you want to create remediation exception.
--
-- /Note:/ Consider using 'configRuleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
preConfigRuleName :: Lens.Lens' PutRemediationExceptions Types.ConfigRuleName
preConfigRuleName = Lens.field @"configRuleName"
{-# DEPRECATED preConfigRuleName "Use generic-lens or generic-optics with 'configRuleName' instead." #-}

-- | An exception list of resource exception keys to be processed with the current request. AWS Config adds exception for each resource key. For example, AWS Config adds 3 exceptions for 3 resource keys.
--
-- /Note:/ Consider using 'resourceKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
preResourceKeys :: Lens.Lens' PutRemediationExceptions (Core.NonEmpty Types.RemediationExceptionResourceKey)
preResourceKeys = Lens.field @"resourceKeys"
{-# DEPRECATED preResourceKeys "Use generic-lens or generic-optics with 'resourceKeys' instead." #-}

-- | The exception is automatically deleted after the expiration date.
--
-- /Note:/ Consider using 'expirationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
preExpirationTime :: Lens.Lens' PutRemediationExceptions (Core.Maybe Core.NominalDiffTime)
preExpirationTime = Lens.field @"expirationTime"
{-# DEPRECATED preExpirationTime "Use generic-lens or generic-optics with 'expirationTime' instead." #-}

-- | The message contains an explanation of the exception.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
preMessage :: Lens.Lens' PutRemediationExceptions (Core.Maybe Types.StringWithCharLimit1024)
preMessage = Lens.field @"message"
{-# DEPRECATED preMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Core.FromJSON PutRemediationExceptions where
  toJSON PutRemediationExceptions {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ConfigRuleName" Core..= configRuleName),
            Core.Just ("ResourceKeys" Core..= resourceKeys),
            ("ExpirationTime" Core..=) Core.<$> expirationTime,
            ("Message" Core..=) Core.<$> message
          ]
      )

instance Core.AWSRequest PutRemediationExceptions where
  type Rs PutRemediationExceptions = PutRemediationExceptionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "StarlingDoveService.PutRemediationExceptions")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          PutRemediationExceptionsResponse'
            Core.<$> (x Core..:? "FailedBatches")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkPutRemediationExceptionsResponse' smart constructor.
data PutRemediationExceptionsResponse = PutRemediationExceptionsResponse'
  { -- | Returns a list of failed remediation exceptions batch objects. Each object in the batch consists of a list of failed items and failure messages.
    failedBatches :: Core.Maybe [Types.FailedRemediationExceptionBatch],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'PutRemediationExceptionsResponse' value with any optional fields omitted.
mkPutRemediationExceptionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  PutRemediationExceptionsResponse
mkPutRemediationExceptionsResponse responseStatus =
  PutRemediationExceptionsResponse'
    { failedBatches = Core.Nothing,
      responseStatus
    }

-- | Returns a list of failed remediation exceptions batch objects. Each object in the batch consists of a list of failed items and failure messages.
--
-- /Note:/ Consider using 'failedBatches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prerrsFailedBatches :: Lens.Lens' PutRemediationExceptionsResponse (Core.Maybe [Types.FailedRemediationExceptionBatch])
prerrsFailedBatches = Lens.field @"failedBatches"
{-# DEPRECATED prerrsFailedBatches "Use generic-lens or generic-optics with 'failedBatches' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prerrsResponseStatus :: Lens.Lens' PutRemediationExceptionsResponse Core.Int
prerrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED prerrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
