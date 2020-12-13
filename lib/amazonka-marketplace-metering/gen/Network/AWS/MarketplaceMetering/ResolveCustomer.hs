{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MarketplaceMetering.ResolveCustomer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- ResolveCustomer is called by a SaaS application during the registration process. When a buyer visits your website during the registration process, the buyer submits a registration token through their browser. The registration token is resolved through this API to obtain a CustomerIdentifier and product code.
module Network.AWS.MarketplaceMetering.ResolveCustomer
  ( -- * Creating a request
    ResolveCustomer (..),
    mkResolveCustomer,

    -- ** Request lenses
    rcRegistrationToken,

    -- * Destructuring the response
    ResolveCustomerResponse (..),
    mkResolveCustomerResponse,

    -- ** Response lenses
    rcrsCustomerIdentifier,
    rcrsProductCode,
    rcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MarketplaceMetering.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains input to the ResolveCustomer operation.
--
-- /See:/ 'mkResolveCustomer' smart constructor.
newtype ResolveCustomer = ResolveCustomer'
  { -- | When a buyer visits your website during the registration process, the buyer submits a registration token through the browser. The registration token is resolved to obtain a CustomerIdentifier and product code.
    registrationToken :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResolveCustomer' with the minimum fields required to make a request.
--
-- * 'registrationToken' - When a buyer visits your website during the registration process, the buyer submits a registration token through the browser. The registration token is resolved to obtain a CustomerIdentifier and product code.
mkResolveCustomer ::
  -- | 'registrationToken'
  Lude.Text ->
  ResolveCustomer
mkResolveCustomer pRegistrationToken_ =
  ResolveCustomer' {registrationToken = pRegistrationToken_}

-- | When a buyer visits your website during the registration process, the buyer submits a registration token through the browser. The registration token is resolved to obtain a CustomerIdentifier and product code.
--
-- /Note:/ Consider using 'registrationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcRegistrationToken :: Lens.Lens' ResolveCustomer Lude.Text
rcRegistrationToken = Lens.lens (registrationToken :: ResolveCustomer -> Lude.Text) (\s a -> s {registrationToken = a} :: ResolveCustomer)
{-# DEPRECATED rcRegistrationToken "Use generic-lens or generic-optics with 'registrationToken' instead." #-}

instance Lude.AWSRequest ResolveCustomer where
  type Rs ResolveCustomer = ResolveCustomerResponse
  request = Req.postJSON marketplaceMeteringService
  response =
    Res.receiveJSON
      ( \s h x ->
          ResolveCustomerResponse'
            Lude.<$> (x Lude..?> "CustomerIdentifier")
            Lude.<*> (x Lude..?> "ProductCode")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ResolveCustomer where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSMPMeteringService.ResolveCustomer" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ResolveCustomer where
  toJSON ResolveCustomer' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("RegistrationToken" Lude..= registrationToken)]
      )

instance Lude.ToPath ResolveCustomer where
  toPath = Lude.const "/"

instance Lude.ToQuery ResolveCustomer where
  toQuery = Lude.const Lude.mempty

-- | The result of the ResolveCustomer operation. Contains the CustomerIdentifier and product code.
--
-- /See:/ 'mkResolveCustomerResponse' smart constructor.
data ResolveCustomerResponse = ResolveCustomerResponse'
  { -- | The CustomerIdentifier is used to identify an individual customer in your application. Calls to BatchMeterUsage require CustomerIdentifiers for each UsageRecord.
    customerIdentifier :: Lude.Maybe Lude.Text,
    -- | The product code is returned to confirm that the buyer is registering for your product. Subsequent BatchMeterUsage calls should be made using this product code.
    productCode :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResolveCustomerResponse' with the minimum fields required to make a request.
--
-- * 'customerIdentifier' - The CustomerIdentifier is used to identify an individual customer in your application. Calls to BatchMeterUsage require CustomerIdentifiers for each UsageRecord.
-- * 'productCode' - The product code is returned to confirm that the buyer is registering for your product. Subsequent BatchMeterUsage calls should be made using this product code.
-- * 'responseStatus' - The response status code.
mkResolveCustomerResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ResolveCustomerResponse
mkResolveCustomerResponse pResponseStatus_ =
  ResolveCustomerResponse'
    { customerIdentifier = Lude.Nothing,
      productCode = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The CustomerIdentifier is used to identify an individual customer in your application. Calls to BatchMeterUsage require CustomerIdentifiers for each UsageRecord.
--
-- /Note:/ Consider using 'customerIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcrsCustomerIdentifier :: Lens.Lens' ResolveCustomerResponse (Lude.Maybe Lude.Text)
rcrsCustomerIdentifier = Lens.lens (customerIdentifier :: ResolveCustomerResponse -> Lude.Maybe Lude.Text) (\s a -> s {customerIdentifier = a} :: ResolveCustomerResponse)
{-# DEPRECATED rcrsCustomerIdentifier "Use generic-lens or generic-optics with 'customerIdentifier' instead." #-}

-- | The product code is returned to confirm that the buyer is registering for your product. Subsequent BatchMeterUsage calls should be made using this product code.
--
-- /Note:/ Consider using 'productCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcrsProductCode :: Lens.Lens' ResolveCustomerResponse (Lude.Maybe Lude.Text)
rcrsProductCode = Lens.lens (productCode :: ResolveCustomerResponse -> Lude.Maybe Lude.Text) (\s a -> s {productCode = a} :: ResolveCustomerResponse)
{-# DEPRECATED rcrsProductCode "Use generic-lens or generic-optics with 'productCode' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcrsResponseStatus :: Lens.Lens' ResolveCustomerResponse Lude.Int
rcrsResponseStatus = Lens.lens (responseStatus :: ResolveCustomerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ResolveCustomerResponse)
{-# DEPRECATED rcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
