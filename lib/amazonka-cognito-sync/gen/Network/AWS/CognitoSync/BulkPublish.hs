{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.BulkPublish
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates a bulk publish of all existing datasets for an Identity Pool to the configured stream. Customers are limited to one successful bulk publish per 24 hours. Bulk publish is an asynchronous request, customers can see the status of the request via the GetBulkPublishDetails operation.
--
-- This API can only be called with developer credentials. You cannot call this API with the temporary user credentials provided by Cognito Identity.
module Network.AWS.CognitoSync.BulkPublish
  ( -- * Creating a request
    BulkPublish (..),
    mkBulkPublish,

    -- ** Request lenses
    bpIdentityPoolId,

    -- * Destructuring the response
    BulkPublishResponse (..),
    mkBulkPublishResponse,

    -- ** Response lenses
    bprsIdentityPoolId,
    bprsResponseStatus,
  )
where

import Network.AWS.CognitoSync.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the BulkPublish operation.
--
-- /See:/ 'mkBulkPublish' smart constructor.
newtype BulkPublish = BulkPublish'
  { -- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
    identityPoolId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BulkPublish' with the minimum fields required to make a request.
--
-- * 'identityPoolId' - A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
mkBulkPublish ::
  -- | 'identityPoolId'
  Lude.Text ->
  BulkPublish
mkBulkPublish pIdentityPoolId_ =
  BulkPublish' {identityPoolId = pIdentityPoolId_}

-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpIdentityPoolId :: Lens.Lens' BulkPublish Lude.Text
bpIdentityPoolId = Lens.lens (identityPoolId :: BulkPublish -> Lude.Text) (\s a -> s {identityPoolId = a} :: BulkPublish)
{-# DEPRECATED bpIdentityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead." #-}

instance Lude.AWSRequest BulkPublish where
  type Rs BulkPublish = BulkPublishResponse
  request = Req.postJSON cognitoSyncService
  response =
    Res.receiveJSON
      ( \s h x ->
          BulkPublishResponse'
            Lude.<$> (x Lude..?> "IdentityPoolId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders BulkPublish where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON BulkPublish where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath BulkPublish where
  toPath BulkPublish' {..} =
    Lude.mconcat
      ["/identitypools/", Lude.toBS identityPoolId, "/bulkpublish"]

instance Lude.ToQuery BulkPublish where
  toQuery = Lude.const Lude.mempty

-- | The output for the BulkPublish operation.
--
-- /See:/ 'mkBulkPublishResponse' smart constructor.
data BulkPublishResponse = BulkPublishResponse'
  { -- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
    identityPoolId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BulkPublishResponse' with the minimum fields required to make a request.
--
-- * 'identityPoolId' - A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
-- * 'responseStatus' - The response status code.
mkBulkPublishResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  BulkPublishResponse
mkBulkPublishResponse pResponseStatus_ =
  BulkPublishResponse'
    { identityPoolId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bprsIdentityPoolId :: Lens.Lens' BulkPublishResponse (Lude.Maybe Lude.Text)
bprsIdentityPoolId = Lens.lens (identityPoolId :: BulkPublishResponse -> Lude.Maybe Lude.Text) (\s a -> s {identityPoolId = a} :: BulkPublishResponse)
{-# DEPRECATED bprsIdentityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bprsResponseStatus :: Lens.Lens' BulkPublishResponse Lude.Int
bprsResponseStatus = Lens.lens (responseStatus :: BulkPublishResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: BulkPublishResponse)
{-# DEPRECATED bprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
