{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.UpdateOriginRequestPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an origin request policy configuration.
--
-- When you update an origin request policy configuration, all the fields are updated with the values provided in the request. You cannot update some fields independent of others. To update an origin request policy configuration:
--
--     * Use @GetOriginRequestPolicyConfig@ to get the current configuration.
--
--
--     * Locally modify the fields in the origin request policy configuration that you want to update.
--
--
--     * Call @UpdateOriginRequestPolicy@ by providing the entire origin request policy configuration, including the fields that you modified and those that you didn’t.
module Network.AWS.CloudFront.UpdateOriginRequestPolicy
  ( -- * Creating a request
    UpdateOriginRequestPolicy (..),
    mkUpdateOriginRequestPolicy,

    -- ** Request lenses
    uorpIfMatch,
    uorpOriginRequestPolicyConfig,
    uorpId,

    -- * Destructuring the response
    UpdateOriginRequestPolicyResponse (..),
    mkUpdateOriginRequestPolicyResponse,

    -- ** Response lenses
    uorprsETag,
    uorprsOriginRequestPolicy,
    uorprsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateOriginRequestPolicy' smart constructor.
data UpdateOriginRequestPolicy = UpdateOriginRequestPolicy'
  { ifMatch ::
      Lude.Maybe Lude.Text,
    originRequestPolicyConfig ::
      OriginRequestPolicyConfig,
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

-- | Creates a value of 'UpdateOriginRequestPolicy' with the minimum fields required to make a request.
--
-- * 'id' - The unique identifier for the origin request policy that you are updating. The identifier is returned in a cache behavior’s @OriginRequestPolicyId@ field in the response to @GetDistributionConfig@ .
-- * 'ifMatch' - The version of the origin request policy that you are updating. The version is returned in the origin request policy’s @ETag@ field in the response to @GetOriginRequestPolicyConfig@ .
-- * 'originRequestPolicyConfig' - An origin request policy configuration.
mkUpdateOriginRequestPolicy ::
  -- | 'originRequestPolicyConfig'
  OriginRequestPolicyConfig ->
  -- | 'id'
  Lude.Text ->
  UpdateOriginRequestPolicy
mkUpdateOriginRequestPolicy pOriginRequestPolicyConfig_ pId_ =
  UpdateOriginRequestPolicy'
    { ifMatch = Lude.Nothing,
      originRequestPolicyConfig = pOriginRequestPolicyConfig_,
      id = pId_
    }

-- | The version of the origin request policy that you are updating. The version is returned in the origin request policy’s @ETag@ field in the response to @GetOriginRequestPolicyConfig@ .
--
-- /Note:/ Consider using 'ifMatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uorpIfMatch :: Lens.Lens' UpdateOriginRequestPolicy (Lude.Maybe Lude.Text)
uorpIfMatch = Lens.lens (ifMatch :: UpdateOriginRequestPolicy -> Lude.Maybe Lude.Text) (\s a -> s {ifMatch = a} :: UpdateOriginRequestPolicy)
{-# DEPRECATED uorpIfMatch "Use generic-lens or generic-optics with 'ifMatch' instead." #-}

-- | An origin request policy configuration.
--
-- /Note:/ Consider using 'originRequestPolicyConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uorpOriginRequestPolicyConfig :: Lens.Lens' UpdateOriginRequestPolicy OriginRequestPolicyConfig
uorpOriginRequestPolicyConfig = Lens.lens (originRequestPolicyConfig :: UpdateOriginRequestPolicy -> OriginRequestPolicyConfig) (\s a -> s {originRequestPolicyConfig = a} :: UpdateOriginRequestPolicy)
{-# DEPRECATED uorpOriginRequestPolicyConfig "Use generic-lens or generic-optics with 'originRequestPolicyConfig' instead." #-}

-- | The unique identifier for the origin request policy that you are updating. The identifier is returned in a cache behavior’s @OriginRequestPolicyId@ field in the response to @GetDistributionConfig@ .
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uorpId :: Lens.Lens' UpdateOriginRequestPolicy Lude.Text
uorpId = Lens.lens (id :: UpdateOriginRequestPolicy -> Lude.Text) (\s a -> s {id = a} :: UpdateOriginRequestPolicy)
{-# DEPRECATED uorpId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest UpdateOriginRequestPolicy where
  type
    Rs UpdateOriginRequestPolicy =
      UpdateOriginRequestPolicyResponse
  request = Req.putXML cloudFrontService
  response =
    Res.receiveXML
      ( \s h x ->
          UpdateOriginRequestPolicyResponse'
            Lude.<$> (h Lude..#? "ETag")
            Lude.<*> (Lude.parseXML x)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToElement UpdateOriginRequestPolicy where
  toElement =
    Lude.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}OriginRequestPolicyConfig"
      Lude.. originRequestPolicyConfig

instance Lude.ToHeaders UpdateOriginRequestPolicy where
  toHeaders UpdateOriginRequestPolicy' {..} =
    Lude.mconcat ["If-Match" Lude.=# ifMatch]

instance Lude.ToPath UpdateOriginRequestPolicy where
  toPath UpdateOriginRequestPolicy' {..} =
    Lude.mconcat ["/2020-05-31/origin-request-policy/", Lude.toBS id]

instance Lude.ToQuery UpdateOriginRequestPolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateOriginRequestPolicyResponse' smart constructor.
data UpdateOriginRequestPolicyResponse = UpdateOriginRequestPolicyResponse'
  { eTag ::
      Lude.Maybe Lude.Text,
    originRequestPolicy ::
      Lude.Maybe
        OriginRequestPolicy,
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

-- | Creates a value of 'UpdateOriginRequestPolicyResponse' with the minimum fields required to make a request.
--
-- * 'eTag' - The current version of the origin request policy.
-- * 'originRequestPolicy' - An origin request policy.
-- * 'responseStatus' - The response status code.
mkUpdateOriginRequestPolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateOriginRequestPolicyResponse
mkUpdateOriginRequestPolicyResponse pResponseStatus_ =
  UpdateOriginRequestPolicyResponse'
    { eTag = Lude.Nothing,
      originRequestPolicy = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The current version of the origin request policy.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uorprsETag :: Lens.Lens' UpdateOriginRequestPolicyResponse (Lude.Maybe Lude.Text)
uorprsETag = Lens.lens (eTag :: UpdateOriginRequestPolicyResponse -> Lude.Maybe Lude.Text) (\s a -> s {eTag = a} :: UpdateOriginRequestPolicyResponse)
{-# DEPRECATED uorprsETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | An origin request policy.
--
-- /Note:/ Consider using 'originRequestPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uorprsOriginRequestPolicy :: Lens.Lens' UpdateOriginRequestPolicyResponse (Lude.Maybe OriginRequestPolicy)
uorprsOriginRequestPolicy = Lens.lens (originRequestPolicy :: UpdateOriginRequestPolicyResponse -> Lude.Maybe OriginRequestPolicy) (\s a -> s {originRequestPolicy = a} :: UpdateOriginRequestPolicyResponse)
{-# DEPRECATED uorprsOriginRequestPolicy "Use generic-lens or generic-optics with 'originRequestPolicy' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uorprsResponseStatus :: Lens.Lens' UpdateOriginRequestPolicyResponse Lude.Int
uorprsResponseStatus = Lens.lens (responseStatus :: UpdateOriginRequestPolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateOriginRequestPolicyResponse)
{-# DEPRECATED uorprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
