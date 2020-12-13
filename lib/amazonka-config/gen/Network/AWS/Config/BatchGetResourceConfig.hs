{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.BatchGetResourceConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the current configuration for one or more requested resources. The operation also returns a list of resources that are not processed in the current request. If there are no unprocessed resources, the operation returns an empty unprocessedResourceKeys list.
module Network.AWS.Config.BatchGetResourceConfig
  ( -- * Creating a request
    BatchGetResourceConfig (..),
    mkBatchGetResourceConfig,

    -- ** Request lenses
    bgrcResourceKeys,

    -- * Destructuring the response
    BatchGetResourceConfigResponse (..),
    mkBatchGetResourceConfigResponse,

    -- ** Response lenses
    bgrcrsBaseConfigurationItems,
    bgrcrsUnprocessedResourceKeys,
    bgrcrsResponseStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkBatchGetResourceConfig' smart constructor.
newtype BatchGetResourceConfig = BatchGetResourceConfig'
  { -- | A list of resource keys to be processed with the current request. Each element in the list consists of the resource type and resource ID.
    resourceKeys :: Lude.NonEmpty ResourceKey
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchGetResourceConfig' with the minimum fields required to make a request.
--
-- * 'resourceKeys' - A list of resource keys to be processed with the current request. Each element in the list consists of the resource type and resource ID.
mkBatchGetResourceConfig ::
  -- | 'resourceKeys'
  Lude.NonEmpty ResourceKey ->
  BatchGetResourceConfig
mkBatchGetResourceConfig pResourceKeys_ =
  BatchGetResourceConfig' {resourceKeys = pResourceKeys_}

-- | A list of resource keys to be processed with the current request. Each element in the list consists of the resource type and resource ID.
--
-- /Note:/ Consider using 'resourceKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgrcResourceKeys :: Lens.Lens' BatchGetResourceConfig (Lude.NonEmpty ResourceKey)
bgrcResourceKeys = Lens.lens (resourceKeys :: BatchGetResourceConfig -> Lude.NonEmpty ResourceKey) (\s a -> s {resourceKeys = a} :: BatchGetResourceConfig)
{-# DEPRECATED bgrcResourceKeys "Use generic-lens or generic-optics with 'resourceKeys' instead." #-}

instance Lude.AWSRequest BatchGetResourceConfig where
  type Rs BatchGetResourceConfig = BatchGetResourceConfigResponse
  request = Req.postJSON configService
  response =
    Res.receiveJSON
      ( \s h x ->
          BatchGetResourceConfigResponse'
            Lude.<$> (x Lude..?> "baseConfigurationItems" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "unprocessedResourceKeys")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders BatchGetResourceConfig where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("StarlingDoveService.BatchGetResourceConfig" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON BatchGetResourceConfig where
  toJSON BatchGetResourceConfig' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("resourceKeys" Lude..= resourceKeys)])

instance Lude.ToPath BatchGetResourceConfig where
  toPath = Lude.const "/"

instance Lude.ToQuery BatchGetResourceConfig where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkBatchGetResourceConfigResponse' smart constructor.
data BatchGetResourceConfigResponse = BatchGetResourceConfigResponse'
  { -- | A list that contains the current configuration of one or more resources.
    baseConfigurationItems :: Lude.Maybe [BaseConfigurationItem],
    -- | A list of resource keys that were not processed with the current response. The unprocessesResourceKeys value is in the same form as ResourceKeys, so the value can be directly provided to a subsequent BatchGetResourceConfig operation. If there are no unprocessed resource keys, the response contains an empty unprocessedResourceKeys list.
    unprocessedResourceKeys :: Lude.Maybe (Lude.NonEmpty ResourceKey),
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchGetResourceConfigResponse' with the minimum fields required to make a request.
--
-- * 'baseConfigurationItems' - A list that contains the current configuration of one or more resources.
-- * 'unprocessedResourceKeys' - A list of resource keys that were not processed with the current response. The unprocessesResourceKeys value is in the same form as ResourceKeys, so the value can be directly provided to a subsequent BatchGetResourceConfig operation. If there are no unprocessed resource keys, the response contains an empty unprocessedResourceKeys list.
-- * 'responseStatus' - The response status code.
mkBatchGetResourceConfigResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  BatchGetResourceConfigResponse
mkBatchGetResourceConfigResponse pResponseStatus_ =
  BatchGetResourceConfigResponse'
    { baseConfigurationItems =
        Lude.Nothing,
      unprocessedResourceKeys = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list that contains the current configuration of one or more resources.
--
-- /Note:/ Consider using 'baseConfigurationItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgrcrsBaseConfigurationItems :: Lens.Lens' BatchGetResourceConfigResponse (Lude.Maybe [BaseConfigurationItem])
bgrcrsBaseConfigurationItems = Lens.lens (baseConfigurationItems :: BatchGetResourceConfigResponse -> Lude.Maybe [BaseConfigurationItem]) (\s a -> s {baseConfigurationItems = a} :: BatchGetResourceConfigResponse)
{-# DEPRECATED bgrcrsBaseConfigurationItems "Use generic-lens or generic-optics with 'baseConfigurationItems' instead." #-}

-- | A list of resource keys that were not processed with the current response. The unprocessesResourceKeys value is in the same form as ResourceKeys, so the value can be directly provided to a subsequent BatchGetResourceConfig operation. If there are no unprocessed resource keys, the response contains an empty unprocessedResourceKeys list.
--
-- /Note:/ Consider using 'unprocessedResourceKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgrcrsUnprocessedResourceKeys :: Lens.Lens' BatchGetResourceConfigResponse (Lude.Maybe (Lude.NonEmpty ResourceKey))
bgrcrsUnprocessedResourceKeys = Lens.lens (unprocessedResourceKeys :: BatchGetResourceConfigResponse -> Lude.Maybe (Lude.NonEmpty ResourceKey)) (\s a -> s {unprocessedResourceKeys = a} :: BatchGetResourceConfigResponse)
{-# DEPRECATED bgrcrsUnprocessedResourceKeys "Use generic-lens or generic-optics with 'unprocessedResourceKeys' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgrcrsResponseStatus :: Lens.Lens' BatchGetResourceConfigResponse Lude.Int
bgrcrsResponseStatus = Lens.lens (responseStatus :: BatchGetResourceConfigResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: BatchGetResourceConfigResponse)
{-# DEPRECATED bgrcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
