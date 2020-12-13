{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.GetProvisionedConcurrencyConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the provisioned concurrency configuration for a function's alias or version.
module Network.AWS.Lambda.GetProvisionedConcurrencyConfig
  ( -- * Creating a request
    GetProvisionedConcurrencyConfig (..),
    mkGetProvisionedConcurrencyConfig,

    -- ** Request lenses
    gpccFunctionName,
    gpccQualifier,

    -- * Destructuring the response
    GetProvisionedConcurrencyConfigResponse (..),
    mkGetProvisionedConcurrencyConfigResponse,

    -- ** Response lenses
    gpccrsStatus,
    gpccrsRequestedProvisionedConcurrentExecutions,
    gpccrsAvailableProvisionedConcurrentExecutions,
    gpccrsStatusReason,
    gpccrsAllocatedProvisionedConcurrentExecutions,
    gpccrsLastModified,
    gpccrsResponseStatus,
  )
where

import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetProvisionedConcurrencyConfig' smart constructor.
data GetProvisionedConcurrencyConfig = GetProvisionedConcurrencyConfig'
  { -- | The name of the Lambda function.
    --
    -- __Name formats__
    --
    --     * __Function name__ - @my-function@ .
    --
    --
    --     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .
    --
    --
    --     * __Partial ARN__ - @123456789012:function:my-function@ .
    --
    --
    -- The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
    functionName :: Lude.Text,
    -- | The version number or alias name.
    qualifier :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetProvisionedConcurrencyConfig' with the minimum fields required to make a request.
--
-- * 'functionName' - The name of the Lambda function.
--
-- __Name formats__
--
--     * __Function name__ - @my-function@ .
--
--
--     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .
--
--
--     * __Partial ARN__ - @123456789012:function:my-function@ .
--
--
-- The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
-- * 'qualifier' - The version number or alias name.
mkGetProvisionedConcurrencyConfig ::
  -- | 'functionName'
  Lude.Text ->
  -- | 'qualifier'
  Lude.Text ->
  GetProvisionedConcurrencyConfig
mkGetProvisionedConcurrencyConfig pFunctionName_ pQualifier_ =
  GetProvisionedConcurrencyConfig'
    { functionName = pFunctionName_,
      qualifier = pQualifier_
    }

-- | The name of the Lambda function.
--
-- __Name formats__
--
--     * __Function name__ - @my-function@ .
--
--
--     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .
--
--
--     * __Partial ARN__ - @123456789012:function:my-function@ .
--
--
-- The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
--
-- /Note:/ Consider using 'functionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpccFunctionName :: Lens.Lens' GetProvisionedConcurrencyConfig Lude.Text
gpccFunctionName = Lens.lens (functionName :: GetProvisionedConcurrencyConfig -> Lude.Text) (\s a -> s {functionName = a} :: GetProvisionedConcurrencyConfig)
{-# DEPRECATED gpccFunctionName "Use generic-lens or generic-optics with 'functionName' instead." #-}

-- | The version number or alias name.
--
-- /Note:/ Consider using 'qualifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpccQualifier :: Lens.Lens' GetProvisionedConcurrencyConfig Lude.Text
gpccQualifier = Lens.lens (qualifier :: GetProvisionedConcurrencyConfig -> Lude.Text) (\s a -> s {qualifier = a} :: GetProvisionedConcurrencyConfig)
{-# DEPRECATED gpccQualifier "Use generic-lens or generic-optics with 'qualifier' instead." #-}

instance Lude.AWSRequest GetProvisionedConcurrencyConfig where
  type
    Rs GetProvisionedConcurrencyConfig =
      GetProvisionedConcurrencyConfigResponse
  request = Req.get lambdaService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetProvisionedConcurrencyConfigResponse'
            Lude.<$> (x Lude..?> "Status")
            Lude.<*> (x Lude..?> "RequestedProvisionedConcurrentExecutions")
            Lude.<*> (x Lude..?> "AvailableProvisionedConcurrentExecutions")
            Lude.<*> (x Lude..?> "StatusReason")
            Lude.<*> (x Lude..?> "AllocatedProvisionedConcurrentExecutions")
            Lude.<*> (x Lude..?> "LastModified")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetProvisionedConcurrencyConfig where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetProvisionedConcurrencyConfig where
  toPath GetProvisionedConcurrencyConfig' {..} =
    Lude.mconcat
      [ "/2019-09-30/functions/",
        Lude.toBS functionName,
        "/provisioned-concurrency"
      ]

instance Lude.ToQuery GetProvisionedConcurrencyConfig where
  toQuery GetProvisionedConcurrencyConfig' {..} =
    Lude.mconcat ["Qualifier" Lude.=: qualifier]

-- | /See:/ 'mkGetProvisionedConcurrencyConfigResponse' smart constructor.
data GetProvisionedConcurrencyConfigResponse = GetProvisionedConcurrencyConfigResponse'
  { -- | The status of the allocation process.
    status :: Lude.Maybe ProvisionedConcurrencyStatusEnum,
    -- | The amount of provisioned concurrency requested.
    requestedProvisionedConcurrentExecutions :: Lude.Maybe Lude.Natural,
    -- | The amount of provisioned concurrency available.
    availableProvisionedConcurrentExecutions :: Lude.Maybe Lude.Natural,
    -- | For failed allocations, the reason that provisioned concurrency could not be allocated.
    statusReason :: Lude.Maybe Lude.Text,
    -- | The amount of provisioned concurrency allocated.
    allocatedProvisionedConcurrentExecutions :: Lude.Maybe Lude.Natural,
    -- | The date and time that a user last updated the configuration, in <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601 format> .
    lastModified :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetProvisionedConcurrencyConfigResponse' with the minimum fields required to make a request.
--
-- * 'status' - The status of the allocation process.
-- * 'requestedProvisionedConcurrentExecutions' - The amount of provisioned concurrency requested.
-- * 'availableProvisionedConcurrentExecutions' - The amount of provisioned concurrency available.
-- * 'statusReason' - For failed allocations, the reason that provisioned concurrency could not be allocated.
-- * 'allocatedProvisionedConcurrentExecutions' - The amount of provisioned concurrency allocated.
-- * 'lastModified' - The date and time that a user last updated the configuration, in <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601 format> .
-- * 'responseStatus' - The response status code.
mkGetProvisionedConcurrencyConfigResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetProvisionedConcurrencyConfigResponse
mkGetProvisionedConcurrencyConfigResponse pResponseStatus_ =
  GetProvisionedConcurrencyConfigResponse'
    { status = Lude.Nothing,
      requestedProvisionedConcurrentExecutions =
        Lude.Nothing,
      availableProvisionedConcurrentExecutions =
        Lude.Nothing,
      statusReason = Lude.Nothing,
      allocatedProvisionedConcurrentExecutions =
        Lude.Nothing,
      lastModified = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The status of the allocation process.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpccrsStatus :: Lens.Lens' GetProvisionedConcurrencyConfigResponse (Lude.Maybe ProvisionedConcurrencyStatusEnum)
gpccrsStatus = Lens.lens (status :: GetProvisionedConcurrencyConfigResponse -> Lude.Maybe ProvisionedConcurrencyStatusEnum) (\s a -> s {status = a} :: GetProvisionedConcurrencyConfigResponse)
{-# DEPRECATED gpccrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The amount of provisioned concurrency requested.
--
-- /Note:/ Consider using 'requestedProvisionedConcurrentExecutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpccrsRequestedProvisionedConcurrentExecutions :: Lens.Lens' GetProvisionedConcurrencyConfigResponse (Lude.Maybe Lude.Natural)
gpccrsRequestedProvisionedConcurrentExecutions = Lens.lens (requestedProvisionedConcurrentExecutions :: GetProvisionedConcurrencyConfigResponse -> Lude.Maybe Lude.Natural) (\s a -> s {requestedProvisionedConcurrentExecutions = a} :: GetProvisionedConcurrencyConfigResponse)
{-# DEPRECATED gpccrsRequestedProvisionedConcurrentExecutions "Use generic-lens or generic-optics with 'requestedProvisionedConcurrentExecutions' instead." #-}

-- | The amount of provisioned concurrency available.
--
-- /Note:/ Consider using 'availableProvisionedConcurrentExecutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpccrsAvailableProvisionedConcurrentExecutions :: Lens.Lens' GetProvisionedConcurrencyConfigResponse (Lude.Maybe Lude.Natural)
gpccrsAvailableProvisionedConcurrentExecutions = Lens.lens (availableProvisionedConcurrentExecutions :: GetProvisionedConcurrencyConfigResponse -> Lude.Maybe Lude.Natural) (\s a -> s {availableProvisionedConcurrentExecutions = a} :: GetProvisionedConcurrencyConfigResponse)
{-# DEPRECATED gpccrsAvailableProvisionedConcurrentExecutions "Use generic-lens or generic-optics with 'availableProvisionedConcurrentExecutions' instead." #-}

-- | For failed allocations, the reason that provisioned concurrency could not be allocated.
--
-- /Note:/ Consider using 'statusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpccrsStatusReason :: Lens.Lens' GetProvisionedConcurrencyConfigResponse (Lude.Maybe Lude.Text)
gpccrsStatusReason = Lens.lens (statusReason :: GetProvisionedConcurrencyConfigResponse -> Lude.Maybe Lude.Text) (\s a -> s {statusReason = a} :: GetProvisionedConcurrencyConfigResponse)
{-# DEPRECATED gpccrsStatusReason "Use generic-lens or generic-optics with 'statusReason' instead." #-}

-- | The amount of provisioned concurrency allocated.
--
-- /Note:/ Consider using 'allocatedProvisionedConcurrentExecutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpccrsAllocatedProvisionedConcurrentExecutions :: Lens.Lens' GetProvisionedConcurrencyConfigResponse (Lude.Maybe Lude.Natural)
gpccrsAllocatedProvisionedConcurrentExecutions = Lens.lens (allocatedProvisionedConcurrentExecutions :: GetProvisionedConcurrencyConfigResponse -> Lude.Maybe Lude.Natural) (\s a -> s {allocatedProvisionedConcurrentExecutions = a} :: GetProvisionedConcurrencyConfigResponse)
{-# DEPRECATED gpccrsAllocatedProvisionedConcurrentExecutions "Use generic-lens or generic-optics with 'allocatedProvisionedConcurrentExecutions' instead." #-}

-- | The date and time that a user last updated the configuration, in <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601 format> .
--
-- /Note:/ Consider using 'lastModified' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpccrsLastModified :: Lens.Lens' GetProvisionedConcurrencyConfigResponse (Lude.Maybe Lude.Text)
gpccrsLastModified = Lens.lens (lastModified :: GetProvisionedConcurrencyConfigResponse -> Lude.Maybe Lude.Text) (\s a -> s {lastModified = a} :: GetProvisionedConcurrencyConfigResponse)
{-# DEPRECATED gpccrsLastModified "Use generic-lens or generic-optics with 'lastModified' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpccrsResponseStatus :: Lens.Lens' GetProvisionedConcurrencyConfigResponse Lude.Int
gpccrsResponseStatus = Lens.lens (responseStatus :: GetProvisionedConcurrencyConfigResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetProvisionedConcurrencyConfigResponse)
{-# DEPRECATED gpccrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
