{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStore.PutMetricPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The metric policy that you want to add to the container. A metric policy allows AWS Elemental MediaStore to send metrics to Amazon CloudWatch. It takes up to 20 minutes for the new policy to take effect.
module Network.AWS.MediaStore.PutMetricPolicy
  ( -- * Creating a request
    PutMetricPolicy (..),
    mkPutMetricPolicy,

    -- ** Request lenses
    pmpMetricPolicy,
    pmpContainerName,

    -- * Destructuring the response
    PutMetricPolicyResponse (..),
    mkPutMetricPolicyResponse,

    -- ** Response lenses
    pmprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaStore.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutMetricPolicy' smart constructor.
data PutMetricPolicy = PutMetricPolicy'
  { -- | The metric policy that you want to associate with the container. In the policy, you must indicate whether you want MediaStore to send container-level metrics. You can also include up to five rules to define groups of objects that you want MediaStore to send object-level metrics for. If you include rules in the policy, construct each rule with both of the following:
    --
    --
    --     * An object group that defines which objects to include in the group. The definition can be a path or a file name, but it can't have more than 900 characters. Valid characters are: a-z, A-Z, 0-9, _ (underscore), = (equal), : (colon), . (period), - (hyphen), ~ (tilde), / (forward slash), and * (asterisk). Wildcards (*) are acceptable.
    --
    --
    --     * An object group name that allows you to refer to the object group. The name can't have more than 30 characters. Valid characters are: a-z, A-Z, 0-9, and _ (underscore).
    metricPolicy :: MetricPolicy,
    -- | The name of the container that you want to add the metric policy to.
    containerName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutMetricPolicy' with the minimum fields required to make a request.
--
-- * 'metricPolicy' - The metric policy that you want to associate with the container. In the policy, you must indicate whether you want MediaStore to send container-level metrics. You can also include up to five rules to define groups of objects that you want MediaStore to send object-level metrics for. If you include rules in the policy, construct each rule with both of the following:
--
--
--     * An object group that defines which objects to include in the group. The definition can be a path or a file name, but it can't have more than 900 characters. Valid characters are: a-z, A-Z, 0-9, _ (underscore), = (equal), : (colon), . (period), - (hyphen), ~ (tilde), / (forward slash), and * (asterisk). Wildcards (*) are acceptable.
--
--
--     * An object group name that allows you to refer to the object group. The name can't have more than 30 characters. Valid characters are: a-z, A-Z, 0-9, and _ (underscore).
--
--
-- * 'containerName' - The name of the container that you want to add the metric policy to.
mkPutMetricPolicy ::
  -- | 'metricPolicy'
  MetricPolicy ->
  -- | 'containerName'
  Lude.Text ->
  PutMetricPolicy
mkPutMetricPolicy pMetricPolicy_ pContainerName_ =
  PutMetricPolicy'
    { metricPolicy = pMetricPolicy_,
      containerName = pContainerName_
    }

-- | The metric policy that you want to associate with the container. In the policy, you must indicate whether you want MediaStore to send container-level metrics. You can also include up to five rules to define groups of objects that you want MediaStore to send object-level metrics for. If you include rules in the policy, construct each rule with both of the following:
--
--
--     * An object group that defines which objects to include in the group. The definition can be a path or a file name, but it can't have more than 900 characters. Valid characters are: a-z, A-Z, 0-9, _ (underscore), = (equal), : (colon), . (period), - (hyphen), ~ (tilde), / (forward slash), and * (asterisk). Wildcards (*) are acceptable.
--
--
--     * An object group name that allows you to refer to the object group. The name can't have more than 30 characters. Valid characters are: a-z, A-Z, 0-9, and _ (underscore).
--
--
--
-- /Note:/ Consider using 'metricPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmpMetricPolicy :: Lens.Lens' PutMetricPolicy MetricPolicy
pmpMetricPolicy = Lens.lens (metricPolicy :: PutMetricPolicy -> MetricPolicy) (\s a -> s {metricPolicy = a} :: PutMetricPolicy)
{-# DEPRECATED pmpMetricPolicy "Use generic-lens or generic-optics with 'metricPolicy' instead." #-}

-- | The name of the container that you want to add the metric policy to.
--
-- /Note:/ Consider using 'containerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmpContainerName :: Lens.Lens' PutMetricPolicy Lude.Text
pmpContainerName = Lens.lens (containerName :: PutMetricPolicy -> Lude.Text) (\s a -> s {containerName = a} :: PutMetricPolicy)
{-# DEPRECATED pmpContainerName "Use generic-lens or generic-optics with 'containerName' instead." #-}

instance Lude.AWSRequest PutMetricPolicy where
  type Rs PutMetricPolicy = PutMetricPolicyResponse
  request = Req.postJSON mediaStoreService
  response =
    Res.receiveEmpty
      ( \s h x ->
          PutMetricPolicyResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutMetricPolicy where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("MediaStore_20170901.PutMetricPolicy" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutMetricPolicy where
  toJSON PutMetricPolicy' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("MetricPolicy" Lude..= metricPolicy),
            Lude.Just ("ContainerName" Lude..= containerName)
          ]
      )

instance Lude.ToPath PutMetricPolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery PutMetricPolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutMetricPolicyResponse' smart constructor.
newtype PutMetricPolicyResponse = PutMetricPolicyResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutMetricPolicyResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkPutMetricPolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutMetricPolicyResponse
mkPutMetricPolicyResponse pResponseStatus_ =
  PutMetricPolicyResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmprsResponseStatus :: Lens.Lens' PutMetricPolicyResponse Lude.Int
pmprsResponseStatus = Lens.lens (responseStatus :: PutMetricPolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutMetricPolicyResponse)
{-# DEPRECATED pmprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
