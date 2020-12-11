{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.StartRemediationExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Runs an on-demand remediation for the specified AWS Config rules against the last known remediation configuration. It runs an execution against the current state of your resources. Remediation execution is asynchronous.
--
-- You can specify up to 100 resource keys per request. An existing StartRemediationExecution call for the specified resource keys must complete before you can call the API again.
module Network.AWS.Config.StartRemediationExecution
  ( -- * Creating a request
    StartRemediationExecution (..),
    mkStartRemediationExecution,

    -- ** Request lenses
    sreConfigRuleName,
    sreResourceKeys,

    -- * Destructuring the response
    StartRemediationExecutionResponse (..),
    mkStartRemediationExecutionResponse,

    -- ** Response lenses
    srersFailureMessage,
    srersFailedItems,
    srersResponseStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartRemediationExecution' smart constructor.
data StartRemediationExecution = StartRemediationExecution'
  { configRuleName ::
      Lude.Text,
    resourceKeys ::
      Lude.NonEmpty ResourceKey
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartRemediationExecution' with the minimum fields required to make a request.
--
-- * 'configRuleName' - The list of names of AWS Config rules that you want to run remediation execution for.
-- * 'resourceKeys' - A list of resource keys to be processed with the current request. Each element in the list consists of the resource type and resource ID.
mkStartRemediationExecution ::
  -- | 'configRuleName'
  Lude.Text ->
  -- | 'resourceKeys'
  Lude.NonEmpty ResourceKey ->
  StartRemediationExecution
mkStartRemediationExecution pConfigRuleName_ pResourceKeys_ =
  StartRemediationExecution'
    { configRuleName = pConfigRuleName_,
      resourceKeys = pResourceKeys_
    }

-- | The list of names of AWS Config rules that you want to run remediation execution for.
--
-- /Note:/ Consider using 'configRuleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sreConfigRuleName :: Lens.Lens' StartRemediationExecution Lude.Text
sreConfigRuleName = Lens.lens (configRuleName :: StartRemediationExecution -> Lude.Text) (\s a -> s {configRuleName = a} :: StartRemediationExecution)
{-# DEPRECATED sreConfigRuleName "Use generic-lens or generic-optics with 'configRuleName' instead." #-}

-- | A list of resource keys to be processed with the current request. Each element in the list consists of the resource type and resource ID.
--
-- /Note:/ Consider using 'resourceKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sreResourceKeys :: Lens.Lens' StartRemediationExecution (Lude.NonEmpty ResourceKey)
sreResourceKeys = Lens.lens (resourceKeys :: StartRemediationExecution -> Lude.NonEmpty ResourceKey) (\s a -> s {resourceKeys = a} :: StartRemediationExecution)
{-# DEPRECATED sreResourceKeys "Use generic-lens or generic-optics with 'resourceKeys' instead." #-}

instance Lude.AWSRequest StartRemediationExecution where
  type
    Rs StartRemediationExecution =
      StartRemediationExecutionResponse
  request = Req.postJSON configService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartRemediationExecutionResponse'
            Lude.<$> (x Lude..?> "FailureMessage")
            Lude.<*> (x Lude..?> "FailedItems")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartRemediationExecution where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StarlingDoveService.StartRemediationExecution" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartRemediationExecution where
  toJSON StartRemediationExecution' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ConfigRuleName" Lude..= configRuleName),
            Lude.Just ("ResourceKeys" Lude..= resourceKeys)
          ]
      )

instance Lude.ToPath StartRemediationExecution where
  toPath = Lude.const "/"

instance Lude.ToQuery StartRemediationExecution where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartRemediationExecutionResponse' smart constructor.
data StartRemediationExecutionResponse = StartRemediationExecutionResponse'
  { failureMessage ::
      Lude.Maybe Lude.Text,
    failedItems ::
      Lude.Maybe
        ( Lude.NonEmpty
            ResourceKey
        ),
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartRemediationExecutionResponse' with the minimum fields required to make a request.
--
-- * 'failedItems' - For resources that have failed to start execution, the API returns a resource key object.
-- * 'failureMessage' - Returns a failure message. For example, the resource is already compliant.
-- * 'responseStatus' - The response status code.
mkStartRemediationExecutionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartRemediationExecutionResponse
mkStartRemediationExecutionResponse pResponseStatus_ =
  StartRemediationExecutionResponse'
    { failureMessage = Lude.Nothing,
      failedItems = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Returns a failure message. For example, the resource is already compliant.
--
-- /Note:/ Consider using 'failureMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srersFailureMessage :: Lens.Lens' StartRemediationExecutionResponse (Lude.Maybe Lude.Text)
srersFailureMessage = Lens.lens (failureMessage :: StartRemediationExecutionResponse -> Lude.Maybe Lude.Text) (\s a -> s {failureMessage = a} :: StartRemediationExecutionResponse)
{-# DEPRECATED srersFailureMessage "Use generic-lens or generic-optics with 'failureMessage' instead." #-}

-- | For resources that have failed to start execution, the API returns a resource key object.
--
-- /Note:/ Consider using 'failedItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srersFailedItems :: Lens.Lens' StartRemediationExecutionResponse (Lude.Maybe (Lude.NonEmpty ResourceKey))
srersFailedItems = Lens.lens (failedItems :: StartRemediationExecutionResponse -> Lude.Maybe (Lude.NonEmpty ResourceKey)) (\s a -> s {failedItems = a} :: StartRemediationExecutionResponse)
{-# DEPRECATED srersFailedItems "Use generic-lens or generic-optics with 'failedItems' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srersResponseStatus :: Lens.Lens' StartRemediationExecutionResponse Lude.Int
srersResponseStatus = Lens.lens (responseStatus :: StartRemediationExecutionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartRemediationExecutionResponse)
{-# DEPRECATED srersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
