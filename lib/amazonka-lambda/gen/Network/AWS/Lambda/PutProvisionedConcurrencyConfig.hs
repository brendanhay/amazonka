{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.PutProvisionedConcurrencyConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a provisioned concurrency configuration to a function's alias or version.
module Network.AWS.Lambda.PutProvisionedConcurrencyConfig
  ( -- * Creating a request
    PutProvisionedConcurrencyConfig (..),
    mkPutProvisionedConcurrencyConfig,

    -- ** Request lenses
    ppccProvisionedConcurrentExecutions,
    ppccFunctionName,
    ppccQualifier,

    -- * Destructuring the response
    PutProvisionedConcurrencyConfigResponse (..),
    mkPutProvisionedConcurrencyConfigResponse,

    -- ** Response lenses
    ppccrsStatus,
    ppccrsRequestedProvisionedConcurrentExecutions,
    ppccrsAvailableProvisionedConcurrentExecutions,
    ppccrsStatusReason,
    ppccrsAllocatedProvisionedConcurrentExecutions,
    ppccrsLastModified,
    ppccrsResponseStatus,
  )
where

import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutProvisionedConcurrencyConfig' smart constructor.
data PutProvisionedConcurrencyConfig = PutProvisionedConcurrencyConfig'
  { -- | The amount of provisioned concurrency to allocate for the version or alias.
    provisionedConcurrentExecutions :: Lude.Natural,
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
    functionName :: Lude.Text,
    -- | The version number or alias name.
    qualifier :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutProvisionedConcurrencyConfig' with the minimum fields required to make a request.
--
-- * 'provisionedConcurrentExecutions' - The amount of provisioned concurrency to allocate for the version or alias.
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
mkPutProvisionedConcurrencyConfig ::
  -- | 'provisionedConcurrentExecutions'
  Lude.Natural ->
  -- | 'functionName'
  Lude.Text ->
  -- | 'qualifier'
  Lude.Text ->
  PutProvisionedConcurrencyConfig
mkPutProvisionedConcurrencyConfig
  pProvisionedConcurrentExecutions_
  pFunctionName_
  pQualifier_ =
    PutProvisionedConcurrencyConfig'
      { provisionedConcurrentExecutions =
          pProvisionedConcurrentExecutions_,
        functionName = pFunctionName_,
        qualifier = pQualifier_
      }

-- | The amount of provisioned concurrency to allocate for the version or alias.
--
-- /Note:/ Consider using 'provisionedConcurrentExecutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppccProvisionedConcurrentExecutions :: Lens.Lens' PutProvisionedConcurrencyConfig Lude.Natural
ppccProvisionedConcurrentExecutions = Lens.lens (provisionedConcurrentExecutions :: PutProvisionedConcurrencyConfig -> Lude.Natural) (\s a -> s {provisionedConcurrentExecutions = a} :: PutProvisionedConcurrencyConfig)
{-# DEPRECATED ppccProvisionedConcurrentExecutions "Use generic-lens or generic-optics with 'provisionedConcurrentExecutions' instead." #-}

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
ppccFunctionName :: Lens.Lens' PutProvisionedConcurrencyConfig Lude.Text
ppccFunctionName = Lens.lens (functionName :: PutProvisionedConcurrencyConfig -> Lude.Text) (\s a -> s {functionName = a} :: PutProvisionedConcurrencyConfig)
{-# DEPRECATED ppccFunctionName "Use generic-lens or generic-optics with 'functionName' instead." #-}

-- | The version number or alias name.
--
-- /Note:/ Consider using 'qualifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppccQualifier :: Lens.Lens' PutProvisionedConcurrencyConfig Lude.Text
ppccQualifier = Lens.lens (qualifier :: PutProvisionedConcurrencyConfig -> Lude.Text) (\s a -> s {qualifier = a} :: PutProvisionedConcurrencyConfig)
{-# DEPRECATED ppccQualifier "Use generic-lens or generic-optics with 'qualifier' instead." #-}

instance Lude.AWSRequest PutProvisionedConcurrencyConfig where
  type
    Rs PutProvisionedConcurrencyConfig =
      PutProvisionedConcurrencyConfigResponse
  request = Req.putJSON lambdaService
  response =
    Res.receiveJSON
      ( \s h x ->
          PutProvisionedConcurrencyConfigResponse'
            Lude.<$> (x Lude..?> "Status")
            Lude.<*> (x Lude..?> "RequestedProvisionedConcurrentExecutions")
            Lude.<*> (x Lude..?> "AvailableProvisionedConcurrentExecutions")
            Lude.<*> (x Lude..?> "StatusReason")
            Lude.<*> (x Lude..?> "AllocatedProvisionedConcurrentExecutions")
            Lude.<*> (x Lude..?> "LastModified")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutProvisionedConcurrencyConfig where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON PutProvisionedConcurrencyConfig where
  toJSON PutProvisionedConcurrencyConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ( "ProvisionedConcurrentExecutions"
                  Lude..= provisionedConcurrentExecutions
              )
          ]
      )

instance Lude.ToPath PutProvisionedConcurrencyConfig where
  toPath PutProvisionedConcurrencyConfig' {..} =
    Lude.mconcat
      [ "/2019-09-30/functions/",
        Lude.toBS functionName,
        "/provisioned-concurrency"
      ]

instance Lude.ToQuery PutProvisionedConcurrencyConfig where
  toQuery PutProvisionedConcurrencyConfig' {..} =
    Lude.mconcat ["Qualifier" Lude.=: qualifier]

-- | /See:/ 'mkPutProvisionedConcurrencyConfigResponse' smart constructor.
data PutProvisionedConcurrencyConfigResponse = PutProvisionedConcurrencyConfigResponse'
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

-- | Creates a value of 'PutProvisionedConcurrencyConfigResponse' with the minimum fields required to make a request.
--
-- * 'status' - The status of the allocation process.
-- * 'requestedProvisionedConcurrentExecutions' - The amount of provisioned concurrency requested.
-- * 'availableProvisionedConcurrentExecutions' - The amount of provisioned concurrency available.
-- * 'statusReason' - For failed allocations, the reason that provisioned concurrency could not be allocated.
-- * 'allocatedProvisionedConcurrentExecutions' - The amount of provisioned concurrency allocated.
-- * 'lastModified' - The date and time that a user last updated the configuration, in <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601 format> .
-- * 'responseStatus' - The response status code.
mkPutProvisionedConcurrencyConfigResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutProvisionedConcurrencyConfigResponse
mkPutProvisionedConcurrencyConfigResponse pResponseStatus_ =
  PutProvisionedConcurrencyConfigResponse'
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
ppccrsStatus :: Lens.Lens' PutProvisionedConcurrencyConfigResponse (Lude.Maybe ProvisionedConcurrencyStatusEnum)
ppccrsStatus = Lens.lens (status :: PutProvisionedConcurrencyConfigResponse -> Lude.Maybe ProvisionedConcurrencyStatusEnum) (\s a -> s {status = a} :: PutProvisionedConcurrencyConfigResponse)
{-# DEPRECATED ppccrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The amount of provisioned concurrency requested.
--
-- /Note:/ Consider using 'requestedProvisionedConcurrentExecutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppccrsRequestedProvisionedConcurrentExecutions :: Lens.Lens' PutProvisionedConcurrencyConfigResponse (Lude.Maybe Lude.Natural)
ppccrsRequestedProvisionedConcurrentExecutions = Lens.lens (requestedProvisionedConcurrentExecutions :: PutProvisionedConcurrencyConfigResponse -> Lude.Maybe Lude.Natural) (\s a -> s {requestedProvisionedConcurrentExecutions = a} :: PutProvisionedConcurrencyConfigResponse)
{-# DEPRECATED ppccrsRequestedProvisionedConcurrentExecutions "Use generic-lens or generic-optics with 'requestedProvisionedConcurrentExecutions' instead." #-}

-- | The amount of provisioned concurrency available.
--
-- /Note:/ Consider using 'availableProvisionedConcurrentExecutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppccrsAvailableProvisionedConcurrentExecutions :: Lens.Lens' PutProvisionedConcurrencyConfigResponse (Lude.Maybe Lude.Natural)
ppccrsAvailableProvisionedConcurrentExecutions = Lens.lens (availableProvisionedConcurrentExecutions :: PutProvisionedConcurrencyConfigResponse -> Lude.Maybe Lude.Natural) (\s a -> s {availableProvisionedConcurrentExecutions = a} :: PutProvisionedConcurrencyConfigResponse)
{-# DEPRECATED ppccrsAvailableProvisionedConcurrentExecutions "Use generic-lens or generic-optics with 'availableProvisionedConcurrentExecutions' instead." #-}

-- | For failed allocations, the reason that provisioned concurrency could not be allocated.
--
-- /Note:/ Consider using 'statusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppccrsStatusReason :: Lens.Lens' PutProvisionedConcurrencyConfigResponse (Lude.Maybe Lude.Text)
ppccrsStatusReason = Lens.lens (statusReason :: PutProvisionedConcurrencyConfigResponse -> Lude.Maybe Lude.Text) (\s a -> s {statusReason = a} :: PutProvisionedConcurrencyConfigResponse)
{-# DEPRECATED ppccrsStatusReason "Use generic-lens or generic-optics with 'statusReason' instead." #-}

-- | The amount of provisioned concurrency allocated.
--
-- /Note:/ Consider using 'allocatedProvisionedConcurrentExecutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppccrsAllocatedProvisionedConcurrentExecutions :: Lens.Lens' PutProvisionedConcurrencyConfigResponse (Lude.Maybe Lude.Natural)
ppccrsAllocatedProvisionedConcurrentExecutions = Lens.lens (allocatedProvisionedConcurrentExecutions :: PutProvisionedConcurrencyConfigResponse -> Lude.Maybe Lude.Natural) (\s a -> s {allocatedProvisionedConcurrentExecutions = a} :: PutProvisionedConcurrencyConfigResponse)
{-# DEPRECATED ppccrsAllocatedProvisionedConcurrentExecutions "Use generic-lens or generic-optics with 'allocatedProvisionedConcurrentExecutions' instead." #-}

-- | The date and time that a user last updated the configuration, in <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601 format> .
--
-- /Note:/ Consider using 'lastModified' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppccrsLastModified :: Lens.Lens' PutProvisionedConcurrencyConfigResponse (Lude.Maybe Lude.Text)
ppccrsLastModified = Lens.lens (lastModified :: PutProvisionedConcurrencyConfigResponse -> Lude.Maybe Lude.Text) (\s a -> s {lastModified = a} :: PutProvisionedConcurrencyConfigResponse)
{-# DEPRECATED ppccrsLastModified "Use generic-lens or generic-optics with 'lastModified' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppccrsResponseStatus :: Lens.Lens' PutProvisionedConcurrencyConfigResponse Lude.Int
ppccrsResponseStatus = Lens.lens (responseStatus :: PutProvisionedConcurrencyConfigResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutProvisionedConcurrencyConfigResponse)
{-# DEPRECATED ppccrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
