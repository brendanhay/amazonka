{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ConfirmProductInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Determines whether a product code is associated with an instance. This action can only be used by the owner of the product code. It is useful when a product code owner must verify whether another user's instance is eligible for support.
module Network.AWS.EC2.ConfirmProductInstance
  ( -- * Creating a request
    ConfirmProductInstance (..),
    mkConfirmProductInstance,

    -- ** Request lenses
    cpiDryRun,
    cpiInstanceId,
    cpiProductCode,

    -- * Destructuring the response
    ConfirmProductInstanceResponse (..),
    mkConfirmProductInstanceResponse,

    -- ** Response lenses
    cpirsReturn,
    cpirsOwnerId,
    cpirsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkConfirmProductInstance' smart constructor.
data ConfirmProductInstance = ConfirmProductInstance'
  { dryRun ::
      Lude.Maybe Lude.Bool,
    instanceId :: Lude.Text,
    productCode :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConfirmProductInstance' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'instanceId' - The ID of the instance.
-- * 'productCode' - The product code. This must be a product code that you own.
mkConfirmProductInstance ::
  -- | 'instanceId'
  Lude.Text ->
  -- | 'productCode'
  Lude.Text ->
  ConfirmProductInstance
mkConfirmProductInstance pInstanceId_ pProductCode_ =
  ConfirmProductInstance'
    { dryRun = Lude.Nothing,
      instanceId = pInstanceId_,
      productCode = pProductCode_
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpiDryRun :: Lens.Lens' ConfirmProductInstance (Lude.Maybe Lude.Bool)
cpiDryRun = Lens.lens (dryRun :: ConfirmProductInstance -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: ConfirmProductInstance)
{-# DEPRECATED cpiDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpiInstanceId :: Lens.Lens' ConfirmProductInstance Lude.Text
cpiInstanceId = Lens.lens (instanceId :: ConfirmProductInstance -> Lude.Text) (\s a -> s {instanceId = a} :: ConfirmProductInstance)
{-# DEPRECATED cpiInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The product code. This must be a product code that you own.
--
-- /Note:/ Consider using 'productCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpiProductCode :: Lens.Lens' ConfirmProductInstance Lude.Text
cpiProductCode = Lens.lens (productCode :: ConfirmProductInstance -> Lude.Text) (\s a -> s {productCode = a} :: ConfirmProductInstance)
{-# DEPRECATED cpiProductCode "Use generic-lens or generic-optics with 'productCode' instead." #-}

instance Lude.AWSRequest ConfirmProductInstance where
  type Rs ConfirmProductInstance = ConfirmProductInstanceResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          ConfirmProductInstanceResponse'
            Lude.<$> (x Lude..@? "return")
            Lude.<*> (x Lude..@? "ownerId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ConfirmProductInstance where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ConfirmProductInstance where
  toPath = Lude.const "/"

instance Lude.ToQuery ConfirmProductInstance where
  toQuery ConfirmProductInstance' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ConfirmProductInstance" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun,
        "InstanceId" Lude.=: instanceId,
        "ProductCode" Lude.=: productCode
      ]

-- | /See:/ 'mkConfirmProductInstanceResponse' smart constructor.
data ConfirmProductInstanceResponse = ConfirmProductInstanceResponse'
  { return ::
      Lude.Maybe Lude.Bool,
    ownerId ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConfirmProductInstanceResponse' with the minimum fields required to make a request.
--
-- * 'ownerId' - The AWS account ID of the instance owner. This is only present if the product code is attached to the instance.
-- * 'responseStatus' - The response status code.
-- * 'return' - The return value of the request. Returns @true@ if the specified product code is owned by the requester and associated with the specified instance.
mkConfirmProductInstanceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ConfirmProductInstanceResponse
mkConfirmProductInstanceResponse pResponseStatus_ =
  ConfirmProductInstanceResponse'
    { return = Lude.Nothing,
      ownerId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The return value of the request. Returns @true@ if the specified product code is owned by the requester and associated with the specified instance.
--
-- /Note:/ Consider using 'return' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpirsReturn :: Lens.Lens' ConfirmProductInstanceResponse (Lude.Maybe Lude.Bool)
cpirsReturn = Lens.lens (return :: ConfirmProductInstanceResponse -> Lude.Maybe Lude.Bool) (\s a -> s {return = a} :: ConfirmProductInstanceResponse)
{-# DEPRECATED cpirsReturn "Use generic-lens or generic-optics with 'return' instead." #-}

-- | The AWS account ID of the instance owner. This is only present if the product code is attached to the instance.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpirsOwnerId :: Lens.Lens' ConfirmProductInstanceResponse (Lude.Maybe Lude.Text)
cpirsOwnerId = Lens.lens (ownerId :: ConfirmProductInstanceResponse -> Lude.Maybe Lude.Text) (\s a -> s {ownerId = a} :: ConfirmProductInstanceResponse)
{-# DEPRECATED cpirsOwnerId "Use generic-lens or generic-optics with 'ownerId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpirsResponseStatus :: Lens.Lens' ConfirmProductInstanceResponse Lude.Int
cpirsResponseStatus = Lens.lens (responseStatus :: ConfirmProductInstanceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ConfirmProductInstanceResponse)
{-# DEPRECATED cpirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
