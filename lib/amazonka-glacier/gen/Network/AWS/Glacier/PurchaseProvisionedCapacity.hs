{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.PurchaseProvisionedCapacity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation purchases a provisioned capacity unit for an AWS account.
module Network.AWS.Glacier.PurchaseProvisionedCapacity
  ( -- * Creating a request
    PurchaseProvisionedCapacity (..),
    mkPurchaseProvisionedCapacity,

    -- ** Request lenses
    ppcAccountId,

    -- * Destructuring the response
    PurchaseProvisionedCapacityResponse (..),
    mkPurchaseProvisionedCapacityResponse,

    -- ** Response lenses
    ppcrsCapacityId,
    ppcrsResponseStatus,
  )
where

import Network.AWS.Glacier.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPurchaseProvisionedCapacity' smart constructor.
newtype PurchaseProvisionedCapacity = PurchaseProvisionedCapacity'
  { -- | The AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '-' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, don't include any hyphens ('-') in the ID.
    accountId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PurchaseProvisionedCapacity' with the minimum fields required to make a request.
--
-- * 'accountId' - The AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '-' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, don't include any hyphens ('-') in the ID.
mkPurchaseProvisionedCapacity ::
  -- | 'accountId'
  Lude.Text ->
  PurchaseProvisionedCapacity
mkPurchaseProvisionedCapacity pAccountId_ =
  PurchaseProvisionedCapacity' {accountId = pAccountId_}

-- | The AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '-' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, don't include any hyphens ('-') in the ID.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppcAccountId :: Lens.Lens' PurchaseProvisionedCapacity Lude.Text
ppcAccountId = Lens.lens (accountId :: PurchaseProvisionedCapacity -> Lude.Text) (\s a -> s {accountId = a} :: PurchaseProvisionedCapacity)
{-# DEPRECATED ppcAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

instance Lude.AWSRequest PurchaseProvisionedCapacity where
  type
    Rs PurchaseProvisionedCapacity =
      PurchaseProvisionedCapacityResponse
  request = Req.postJSON glacierService
  response =
    Res.receiveEmpty
      ( \s h x ->
          PurchaseProvisionedCapacityResponse'
            Lude.<$> (h Lude..#? "x-amz-capacity-id")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PurchaseProvisionedCapacity where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON PurchaseProvisionedCapacity where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath PurchaseProvisionedCapacity where
  toPath PurchaseProvisionedCapacity' {..} =
    Lude.mconcat ["/", Lude.toBS accountId, "/provisioned-capacity"]

instance Lude.ToQuery PurchaseProvisionedCapacity where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPurchaseProvisionedCapacityResponse' smart constructor.
data PurchaseProvisionedCapacityResponse = PurchaseProvisionedCapacityResponse'
  { -- | The ID that identifies the provisioned capacity unit.
    capacityId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PurchaseProvisionedCapacityResponse' with the minimum fields required to make a request.
--
-- * 'capacityId' - The ID that identifies the provisioned capacity unit.
-- * 'responseStatus' - The response status code.
mkPurchaseProvisionedCapacityResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PurchaseProvisionedCapacityResponse
mkPurchaseProvisionedCapacityResponse pResponseStatus_ =
  PurchaseProvisionedCapacityResponse'
    { capacityId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ID that identifies the provisioned capacity unit.
--
-- /Note:/ Consider using 'capacityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppcrsCapacityId :: Lens.Lens' PurchaseProvisionedCapacityResponse (Lude.Maybe Lude.Text)
ppcrsCapacityId = Lens.lens (capacityId :: PurchaseProvisionedCapacityResponse -> Lude.Maybe Lude.Text) (\s a -> s {capacityId = a} :: PurchaseProvisionedCapacityResponse)
{-# DEPRECATED ppcrsCapacityId "Use generic-lens or generic-optics with 'capacityId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppcrsResponseStatus :: Lens.Lens' PurchaseProvisionedCapacityResponse Lude.Int
ppcrsResponseStatus = Lens.lens (responseStatus :: PurchaseProvisionedCapacityResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PurchaseProvisionedCapacityResponse)
{-# DEPRECATED ppcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
