{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.BatchGetAggregateResourceConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the current configuration items for resources that are present in your AWS Config aggregator. The operation also returns a list of resources that are not processed in the current request. If there are no unprocessed resources, the operation returns an empty @unprocessedResourceIdentifiers@ list.
module Network.AWS.Config.BatchGetAggregateResourceConfig
  ( -- * Creating a request
    BatchGetAggregateResourceConfig (..),
    mkBatchGetAggregateResourceConfig,

    -- ** Request lenses
    bgarcConfigurationAggregatorName,
    bgarcResourceIdentifiers,

    -- * Destructuring the response
    BatchGetAggregateResourceConfigResponse (..),
    mkBatchGetAggregateResourceConfigResponse,

    -- ** Response lenses
    bgarcrsBaseConfigurationItems,
    bgarcrsUnprocessedResourceIdentifiers,
    bgarcrsResponseStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkBatchGetAggregateResourceConfig' smart constructor.
data BatchGetAggregateResourceConfig = BatchGetAggregateResourceConfig'
  { configurationAggregatorName ::
      Lude.Text,
    resourceIdentifiers ::
      Lude.NonEmpty
        AggregateResourceIdentifier
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchGetAggregateResourceConfig' with the minimum fields required to make a request.
--
-- * 'configurationAggregatorName' - The name of the configuration aggregator.
-- * 'resourceIdentifiers' - A list of aggregate ResourceIdentifiers objects.
mkBatchGetAggregateResourceConfig ::
  -- | 'configurationAggregatorName'
  Lude.Text ->
  -- | 'resourceIdentifiers'
  Lude.NonEmpty AggregateResourceIdentifier ->
  BatchGetAggregateResourceConfig
mkBatchGetAggregateResourceConfig
  pConfigurationAggregatorName_
  pResourceIdentifiers_ =
    BatchGetAggregateResourceConfig'
      { configurationAggregatorName =
          pConfigurationAggregatorName_,
        resourceIdentifiers = pResourceIdentifiers_
      }

-- | The name of the configuration aggregator.
--
-- /Note:/ Consider using 'configurationAggregatorName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgarcConfigurationAggregatorName :: Lens.Lens' BatchGetAggregateResourceConfig Lude.Text
bgarcConfigurationAggregatorName = Lens.lens (configurationAggregatorName :: BatchGetAggregateResourceConfig -> Lude.Text) (\s a -> s {configurationAggregatorName = a} :: BatchGetAggregateResourceConfig)
{-# DEPRECATED bgarcConfigurationAggregatorName "Use generic-lens or generic-optics with 'configurationAggregatorName' instead." #-}

-- | A list of aggregate ResourceIdentifiers objects.
--
-- /Note:/ Consider using 'resourceIdentifiers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgarcResourceIdentifiers :: Lens.Lens' BatchGetAggregateResourceConfig (Lude.NonEmpty AggregateResourceIdentifier)
bgarcResourceIdentifiers = Lens.lens (resourceIdentifiers :: BatchGetAggregateResourceConfig -> Lude.NonEmpty AggregateResourceIdentifier) (\s a -> s {resourceIdentifiers = a} :: BatchGetAggregateResourceConfig)
{-# DEPRECATED bgarcResourceIdentifiers "Use generic-lens or generic-optics with 'resourceIdentifiers' instead." #-}

instance Lude.AWSRequest BatchGetAggregateResourceConfig where
  type
    Rs BatchGetAggregateResourceConfig =
      BatchGetAggregateResourceConfigResponse
  request = Req.postJSON configService
  response =
    Res.receiveJSON
      ( \s h x ->
          BatchGetAggregateResourceConfigResponse'
            Lude.<$> (x Lude..?> "BaseConfigurationItems" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "UnprocessedResourceIdentifiers" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders BatchGetAggregateResourceConfig where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StarlingDoveService.BatchGetAggregateResourceConfig" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON BatchGetAggregateResourceConfig where
  toJSON BatchGetAggregateResourceConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ( "ConfigurationAggregatorName"
                  Lude..= configurationAggregatorName
              ),
            Lude.Just ("ResourceIdentifiers" Lude..= resourceIdentifiers)
          ]
      )

instance Lude.ToPath BatchGetAggregateResourceConfig where
  toPath = Lude.const "/"

instance Lude.ToQuery BatchGetAggregateResourceConfig where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkBatchGetAggregateResourceConfigResponse' smart constructor.
data BatchGetAggregateResourceConfigResponse = BatchGetAggregateResourceConfigResponse'
  { baseConfigurationItems ::
      Lude.Maybe
        [BaseConfigurationItem],
    unprocessedResourceIdentifiers ::
      Lude.Maybe
        [AggregateResourceIdentifier],
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

-- | Creates a value of 'BatchGetAggregateResourceConfigResponse' with the minimum fields required to make a request.
--
-- * 'baseConfigurationItems' - A list that contains the current configuration of one or more resources.
-- * 'responseStatus' - The response status code.
-- * 'unprocessedResourceIdentifiers' - A list of resource identifiers that were not processed with current scope. The list is empty if all the resources are processed.
mkBatchGetAggregateResourceConfigResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  BatchGetAggregateResourceConfigResponse
mkBatchGetAggregateResourceConfigResponse pResponseStatus_ =
  BatchGetAggregateResourceConfigResponse'
    { baseConfigurationItems =
        Lude.Nothing,
      unprocessedResourceIdentifiers = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list that contains the current configuration of one or more resources.
--
-- /Note:/ Consider using 'baseConfigurationItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgarcrsBaseConfigurationItems :: Lens.Lens' BatchGetAggregateResourceConfigResponse (Lude.Maybe [BaseConfigurationItem])
bgarcrsBaseConfigurationItems = Lens.lens (baseConfigurationItems :: BatchGetAggregateResourceConfigResponse -> Lude.Maybe [BaseConfigurationItem]) (\s a -> s {baseConfigurationItems = a} :: BatchGetAggregateResourceConfigResponse)
{-# DEPRECATED bgarcrsBaseConfigurationItems "Use generic-lens or generic-optics with 'baseConfigurationItems' instead." #-}

-- | A list of resource identifiers that were not processed with current scope. The list is empty if all the resources are processed.
--
-- /Note:/ Consider using 'unprocessedResourceIdentifiers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgarcrsUnprocessedResourceIdentifiers :: Lens.Lens' BatchGetAggregateResourceConfigResponse (Lude.Maybe [AggregateResourceIdentifier])
bgarcrsUnprocessedResourceIdentifiers = Lens.lens (unprocessedResourceIdentifiers :: BatchGetAggregateResourceConfigResponse -> Lude.Maybe [AggregateResourceIdentifier]) (\s a -> s {unprocessedResourceIdentifiers = a} :: BatchGetAggregateResourceConfigResponse)
{-# DEPRECATED bgarcrsUnprocessedResourceIdentifiers "Use generic-lens or generic-optics with 'unprocessedResourceIdentifiers' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgarcrsResponseStatus :: Lens.Lens' BatchGetAggregateResourceConfigResponse Lude.Int
bgarcrsResponseStatus = Lens.lens (responseStatus :: BatchGetAggregateResourceConfigResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: BatchGetAggregateResourceConfigResponse)
{-# DEPRECATED bgarcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
