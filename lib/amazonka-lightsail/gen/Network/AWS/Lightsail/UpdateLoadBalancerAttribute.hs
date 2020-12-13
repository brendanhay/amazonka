{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.UpdateLoadBalancerAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified attribute for a load balancer. You can only update one attribute at a time.
--
-- The @update load balancer attribute@ operation supports tag-based access control via resource tags applied to the resource identified by @load balancer name@ . For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.UpdateLoadBalancerAttribute
  ( -- * Creating a request
    UpdateLoadBalancerAttribute (..),
    mkUpdateLoadBalancerAttribute,

    -- ** Request lenses
    ulbaAttributeValue,
    ulbaLoadBalancerName,
    ulbaAttributeName,

    -- * Destructuring the response
    UpdateLoadBalancerAttributeResponse (..),
    mkUpdateLoadBalancerAttributeResponse,

    -- ** Response lenses
    ulbarsOperations,
    ulbarsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateLoadBalancerAttribute' smart constructor.
data UpdateLoadBalancerAttribute = UpdateLoadBalancerAttribute'
  { -- | The value that you want to specify for the attribute name.
    attributeValue :: Lude.Text,
    -- | The name of the load balancer that you want to modify (e.g., @my-load-balancer@ .
    loadBalancerName :: Lude.Text,
    -- | The name of the attribute you want to update. Valid values are below.
    attributeName :: LoadBalancerAttributeName
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateLoadBalancerAttribute' with the minimum fields required to make a request.
--
-- * 'attributeValue' - The value that you want to specify for the attribute name.
-- * 'loadBalancerName' - The name of the load balancer that you want to modify (e.g., @my-load-balancer@ .
-- * 'attributeName' - The name of the attribute you want to update. Valid values are below.
mkUpdateLoadBalancerAttribute ::
  -- | 'attributeValue'
  Lude.Text ->
  -- | 'loadBalancerName'
  Lude.Text ->
  -- | 'attributeName'
  LoadBalancerAttributeName ->
  UpdateLoadBalancerAttribute
mkUpdateLoadBalancerAttribute
  pAttributeValue_
  pLoadBalancerName_
  pAttributeName_ =
    UpdateLoadBalancerAttribute'
      { attributeValue = pAttributeValue_,
        loadBalancerName = pLoadBalancerName_,
        attributeName = pAttributeName_
      }

-- | The value that you want to specify for the attribute name.
--
-- /Note:/ Consider using 'attributeValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulbaAttributeValue :: Lens.Lens' UpdateLoadBalancerAttribute Lude.Text
ulbaAttributeValue = Lens.lens (attributeValue :: UpdateLoadBalancerAttribute -> Lude.Text) (\s a -> s {attributeValue = a} :: UpdateLoadBalancerAttribute)
{-# DEPRECATED ulbaAttributeValue "Use generic-lens or generic-optics with 'attributeValue' instead." #-}

-- | The name of the load balancer that you want to modify (e.g., @my-load-balancer@ .
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulbaLoadBalancerName :: Lens.Lens' UpdateLoadBalancerAttribute Lude.Text
ulbaLoadBalancerName = Lens.lens (loadBalancerName :: UpdateLoadBalancerAttribute -> Lude.Text) (\s a -> s {loadBalancerName = a} :: UpdateLoadBalancerAttribute)
{-# DEPRECATED ulbaLoadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead." #-}

-- | The name of the attribute you want to update. Valid values are below.
--
-- /Note:/ Consider using 'attributeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulbaAttributeName :: Lens.Lens' UpdateLoadBalancerAttribute LoadBalancerAttributeName
ulbaAttributeName = Lens.lens (attributeName :: UpdateLoadBalancerAttribute -> LoadBalancerAttributeName) (\s a -> s {attributeName = a} :: UpdateLoadBalancerAttribute)
{-# DEPRECATED ulbaAttributeName "Use generic-lens or generic-optics with 'attributeName' instead." #-}

instance Lude.AWSRequest UpdateLoadBalancerAttribute where
  type
    Rs UpdateLoadBalancerAttribute =
      UpdateLoadBalancerAttributeResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateLoadBalancerAttributeResponse'
            Lude.<$> (x Lude..?> "operations" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateLoadBalancerAttribute where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Lightsail_20161128.UpdateLoadBalancerAttribute" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateLoadBalancerAttribute where
  toJSON UpdateLoadBalancerAttribute' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("attributeValue" Lude..= attributeValue),
            Lude.Just ("loadBalancerName" Lude..= loadBalancerName),
            Lude.Just ("attributeName" Lude..= attributeName)
          ]
      )

instance Lude.ToPath UpdateLoadBalancerAttribute where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateLoadBalancerAttribute where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateLoadBalancerAttributeResponse' smart constructor.
data UpdateLoadBalancerAttributeResponse = UpdateLoadBalancerAttributeResponse'
  { -- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
    operations :: Lude.Maybe [Operation],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateLoadBalancerAttributeResponse' with the minimum fields required to make a request.
--
-- * 'operations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
-- * 'responseStatus' - The response status code.
mkUpdateLoadBalancerAttributeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateLoadBalancerAttributeResponse
mkUpdateLoadBalancerAttributeResponse pResponseStatus_ =
  UpdateLoadBalancerAttributeResponse'
    { operations = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulbarsOperations :: Lens.Lens' UpdateLoadBalancerAttributeResponse (Lude.Maybe [Operation])
ulbarsOperations = Lens.lens (operations :: UpdateLoadBalancerAttributeResponse -> Lude.Maybe [Operation]) (\s a -> s {operations = a} :: UpdateLoadBalancerAttributeResponse)
{-# DEPRECATED ulbarsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulbarsResponseStatus :: Lens.Lens' UpdateLoadBalancerAttributeResponse Lude.Int
ulbarsResponseStatus = Lens.lens (responseStatus :: UpdateLoadBalancerAttributeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateLoadBalancerAttributeResponse)
{-# DEPRECATED ulbarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
