{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.BatchGetOnPremisesInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about one or more on-premises instances. The maximum number of on-premises instances that can be returned is 25.
module Network.AWS.CodeDeploy.BatchGetOnPremisesInstances
  ( -- * Creating a request
    BatchGetOnPremisesInstances (..),
    mkBatchGetOnPremisesInstances,

    -- ** Request lenses
    bgopiInstanceNames,

    -- * Destructuring the response
    BatchGetOnPremisesInstancesResponse (..),
    mkBatchGetOnPremisesInstancesResponse,

    -- ** Response lenses
    bgopirsInstanceInfos,
    bgopirsResponseStatus,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @BatchGetOnPremisesInstances@ operation.
--
-- /See:/ 'mkBatchGetOnPremisesInstances' smart constructor.
newtype BatchGetOnPremisesInstances = BatchGetOnPremisesInstances'
  { -- | The names of the on-premises instances about which to get information. The maximum number of instance names you can specify is 25.
    instanceNames :: [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchGetOnPremisesInstances' with the minimum fields required to make a request.
--
-- * 'instanceNames' - The names of the on-premises instances about which to get information. The maximum number of instance names you can specify is 25.
mkBatchGetOnPremisesInstances ::
  BatchGetOnPremisesInstances
mkBatchGetOnPremisesInstances =
  BatchGetOnPremisesInstances' {instanceNames = Lude.mempty}

-- | The names of the on-premises instances about which to get information. The maximum number of instance names you can specify is 25.
--
-- /Note:/ Consider using 'instanceNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgopiInstanceNames :: Lens.Lens' BatchGetOnPremisesInstances [Lude.Text]
bgopiInstanceNames = Lens.lens (instanceNames :: BatchGetOnPremisesInstances -> [Lude.Text]) (\s a -> s {instanceNames = a} :: BatchGetOnPremisesInstances)
{-# DEPRECATED bgopiInstanceNames "Use generic-lens or generic-optics with 'instanceNames' instead." #-}

instance Lude.AWSRequest BatchGetOnPremisesInstances where
  type
    Rs BatchGetOnPremisesInstances =
      BatchGetOnPremisesInstancesResponse
  request = Req.postJSON codeDeployService
  response =
    Res.receiveJSON
      ( \s h x ->
          BatchGetOnPremisesInstancesResponse'
            Lude.<$> (x Lude..?> "instanceInfos" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders BatchGetOnPremisesInstances where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "CodeDeploy_20141006.BatchGetOnPremisesInstances" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON BatchGetOnPremisesInstances where
  toJSON BatchGetOnPremisesInstances' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("instanceNames" Lude..= instanceNames)]
      )

instance Lude.ToPath BatchGetOnPremisesInstances where
  toPath = Lude.const "/"

instance Lude.ToQuery BatchGetOnPremisesInstances where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @BatchGetOnPremisesInstances@ operation.
--
-- /See:/ 'mkBatchGetOnPremisesInstancesResponse' smart constructor.
data BatchGetOnPremisesInstancesResponse = BatchGetOnPremisesInstancesResponse'
  { -- | Information about the on-premises instances.
    instanceInfos :: Lude.Maybe [InstanceInfo],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchGetOnPremisesInstancesResponse' with the minimum fields required to make a request.
--
-- * 'instanceInfos' - Information about the on-premises instances.
-- * 'responseStatus' - The response status code.
mkBatchGetOnPremisesInstancesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  BatchGetOnPremisesInstancesResponse
mkBatchGetOnPremisesInstancesResponse pResponseStatus_ =
  BatchGetOnPremisesInstancesResponse'
    { instanceInfos =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the on-premises instances.
--
-- /Note:/ Consider using 'instanceInfos' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgopirsInstanceInfos :: Lens.Lens' BatchGetOnPremisesInstancesResponse (Lude.Maybe [InstanceInfo])
bgopirsInstanceInfos = Lens.lens (instanceInfos :: BatchGetOnPremisesInstancesResponse -> Lude.Maybe [InstanceInfo]) (\s a -> s {instanceInfos = a} :: BatchGetOnPremisesInstancesResponse)
{-# DEPRECATED bgopirsInstanceInfos "Use generic-lens or generic-optics with 'instanceInfos' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgopirsResponseStatus :: Lens.Lens' BatchGetOnPremisesInstancesResponse Lude.Int
bgopirsResponseStatus = Lens.lens (responseStatus :: BatchGetOnPremisesInstancesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: BatchGetOnPremisesInstancesResponse)
{-# DEPRECATED bgopirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
