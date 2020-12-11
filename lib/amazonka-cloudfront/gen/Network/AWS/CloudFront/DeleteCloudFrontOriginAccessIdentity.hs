{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.DeleteCloudFrontOriginAccessIdentity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete an origin access identity.
module Network.AWS.CloudFront.DeleteCloudFrontOriginAccessIdentity
  ( -- * Creating a request
    DeleteCloudFrontOriginAccessIdentity (..),
    mkDeleteCloudFrontOriginAccessIdentity,

    -- ** Request lenses
    dcfoaiIfMatch,
    dcfoaiId,

    -- * Destructuring the response
    DeleteCloudFrontOriginAccessIdentityResponse (..),
    mkDeleteCloudFrontOriginAccessIdentityResponse,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Deletes a origin access identity.
--
-- /See:/ 'mkDeleteCloudFrontOriginAccessIdentity' smart constructor.
data DeleteCloudFrontOriginAccessIdentity = DeleteCloudFrontOriginAccessIdentity'
  { ifMatch ::
      Lude.Maybe
        Lude.Text,
    id :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteCloudFrontOriginAccessIdentity' with the minimum fields required to make a request.
--
-- * 'id' - The origin access identity's ID.
-- * 'ifMatch' - The value of the @ETag@ header you received from a previous @GET@ or @PUT@ request. For example: @E2QWRUHAPOMQZL@ .
mkDeleteCloudFrontOriginAccessIdentity ::
  -- | 'id'
  Lude.Text ->
  DeleteCloudFrontOriginAccessIdentity
mkDeleteCloudFrontOriginAccessIdentity pId_ =
  DeleteCloudFrontOriginAccessIdentity'
    { ifMatch = Lude.Nothing,
      id = pId_
    }

-- | The value of the @ETag@ header you received from a previous @GET@ or @PUT@ request. For example: @E2QWRUHAPOMQZL@ .
--
-- /Note:/ Consider using 'ifMatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcfoaiIfMatch :: Lens.Lens' DeleteCloudFrontOriginAccessIdentity (Lude.Maybe Lude.Text)
dcfoaiIfMatch = Lens.lens (ifMatch :: DeleteCloudFrontOriginAccessIdentity -> Lude.Maybe Lude.Text) (\s a -> s {ifMatch = a} :: DeleteCloudFrontOriginAccessIdentity)
{-# DEPRECATED dcfoaiIfMatch "Use generic-lens or generic-optics with 'ifMatch' instead." #-}

-- | The origin access identity's ID.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcfoaiId :: Lens.Lens' DeleteCloudFrontOriginAccessIdentity Lude.Text
dcfoaiId = Lens.lens (id :: DeleteCloudFrontOriginAccessIdentity -> Lude.Text) (\s a -> s {id = a} :: DeleteCloudFrontOriginAccessIdentity)
{-# DEPRECATED dcfoaiId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest DeleteCloudFrontOriginAccessIdentity where
  type
    Rs DeleteCloudFrontOriginAccessIdentity =
      DeleteCloudFrontOriginAccessIdentityResponse
  request = Req.delete cloudFrontService
  response =
    Res.receiveNull DeleteCloudFrontOriginAccessIdentityResponse'

instance Lude.ToHeaders DeleteCloudFrontOriginAccessIdentity where
  toHeaders DeleteCloudFrontOriginAccessIdentity' {..} =
    Lude.mconcat ["If-Match" Lude.=# ifMatch]

instance Lude.ToPath DeleteCloudFrontOriginAccessIdentity where
  toPath DeleteCloudFrontOriginAccessIdentity' {..} =
    Lude.mconcat
      ["/2020-05-31/origin-access-identity/cloudfront/", Lude.toBS id]

instance Lude.ToQuery DeleteCloudFrontOriginAccessIdentity where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteCloudFrontOriginAccessIdentityResponse' smart constructor.
data DeleteCloudFrontOriginAccessIdentityResponse = DeleteCloudFrontOriginAccessIdentityResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteCloudFrontOriginAccessIdentityResponse' with the minimum fields required to make a request.
mkDeleteCloudFrontOriginAccessIdentityResponse ::
  DeleteCloudFrontOriginAccessIdentityResponse
mkDeleteCloudFrontOriginAccessIdentityResponse =
  DeleteCloudFrontOriginAccessIdentityResponse'
