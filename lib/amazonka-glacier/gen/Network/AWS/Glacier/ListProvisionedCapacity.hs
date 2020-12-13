{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.ListProvisionedCapacity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation lists the provisioned capacity units for the specified AWS account.
module Network.AWS.Glacier.ListProvisionedCapacity
  ( -- * Creating a request
    ListProvisionedCapacity (..),
    mkListProvisionedCapacity,

    -- ** Request lenses
    lpcAccountId,

    -- * Destructuring the response
    ListProvisionedCapacityResponse (..),
    mkListProvisionedCapacityResponse,

    -- ** Response lenses
    lpcrsProvisionedCapacityList,
    lpcrsResponseStatus,
  )
where

import Network.AWS.Glacier.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListProvisionedCapacity' smart constructor.
newtype ListProvisionedCapacity = ListProvisionedCapacity'
  { -- | The AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '-' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, don't include any hyphens ('-') in the ID.
    accountId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListProvisionedCapacity' with the minimum fields required to make a request.
--
-- * 'accountId' - The AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '-' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, don't include any hyphens ('-') in the ID.
mkListProvisionedCapacity ::
  -- | 'accountId'
  Lude.Text ->
  ListProvisionedCapacity
mkListProvisionedCapacity pAccountId_ =
  ListProvisionedCapacity' {accountId = pAccountId_}

-- | The AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '-' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, don't include any hyphens ('-') in the ID.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpcAccountId :: Lens.Lens' ListProvisionedCapacity Lude.Text
lpcAccountId = Lens.lens (accountId :: ListProvisionedCapacity -> Lude.Text) (\s a -> s {accountId = a} :: ListProvisionedCapacity)
{-# DEPRECATED lpcAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

instance Lude.AWSRequest ListProvisionedCapacity where
  type Rs ListProvisionedCapacity = ListProvisionedCapacityResponse
  request = Req.get glacierService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListProvisionedCapacityResponse'
            Lude.<$> (x Lude..?> "ProvisionedCapacityList" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListProvisionedCapacity where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListProvisionedCapacity where
  toPath ListProvisionedCapacity' {..} =
    Lude.mconcat ["/", Lude.toBS accountId, "/provisioned-capacity"]

instance Lude.ToQuery ListProvisionedCapacity where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListProvisionedCapacityResponse' smart constructor.
data ListProvisionedCapacityResponse = ListProvisionedCapacityResponse'
  { -- | The response body contains the following JSON fields.
    provisionedCapacityList :: Lude.Maybe [ProvisionedCapacityDescription],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListProvisionedCapacityResponse' with the minimum fields required to make a request.
--
-- * 'provisionedCapacityList' - The response body contains the following JSON fields.
-- * 'responseStatus' - The response status code.
mkListProvisionedCapacityResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListProvisionedCapacityResponse
mkListProvisionedCapacityResponse pResponseStatus_ =
  ListProvisionedCapacityResponse'
    { provisionedCapacityList =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The response body contains the following JSON fields.
--
-- /Note:/ Consider using 'provisionedCapacityList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpcrsProvisionedCapacityList :: Lens.Lens' ListProvisionedCapacityResponse (Lude.Maybe [ProvisionedCapacityDescription])
lpcrsProvisionedCapacityList = Lens.lens (provisionedCapacityList :: ListProvisionedCapacityResponse -> Lude.Maybe [ProvisionedCapacityDescription]) (\s a -> s {provisionedCapacityList = a} :: ListProvisionedCapacityResponse)
{-# DEPRECATED lpcrsProvisionedCapacityList "Use generic-lens or generic-optics with 'provisionedCapacityList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpcrsResponseStatus :: Lens.Lens' ListProvisionedCapacityResponse Lude.Int
lpcrsResponseStatus = Lens.lens (responseStatus :: ListProvisionedCapacityResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListProvisionedCapacityResponse)
{-# DEPRECATED lpcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
