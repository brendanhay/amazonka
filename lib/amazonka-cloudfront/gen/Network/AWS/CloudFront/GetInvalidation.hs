{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.GetInvalidation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the information about an invalidation.
module Network.AWS.CloudFront.GetInvalidation
  ( -- * Creating a request
    GetInvalidation (..),
    mkGetInvalidation,

    -- ** Request lenses
    giDistributionId,
    giId,

    -- * Destructuring the response
    GetInvalidationResponse (..),
    mkGetInvalidationResponse,

    -- ** Response lenses
    girsInvalidation,
    girsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The request to get an invalidation's information.
--
-- /See:/ 'mkGetInvalidation' smart constructor.
data GetInvalidation = GetInvalidation'
  { distributionId ::
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

-- | Creates a value of 'GetInvalidation' with the minimum fields required to make a request.
--
-- * 'distributionId' - The distribution's ID.
-- * 'id' - The identifier for the invalidation request, for example, @IDFDVBD632BHDS5@ .
mkGetInvalidation ::
  -- | 'distributionId'
  Lude.Text ->
  -- | 'id'
  Lude.Text ->
  GetInvalidation
mkGetInvalidation pDistributionId_ pId_ =
  GetInvalidation' {distributionId = pDistributionId_, id = pId_}

-- | The distribution's ID.
--
-- /Note:/ Consider using 'distributionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giDistributionId :: Lens.Lens' GetInvalidation Lude.Text
giDistributionId = Lens.lens (distributionId :: GetInvalidation -> Lude.Text) (\s a -> s {distributionId = a} :: GetInvalidation)
{-# DEPRECATED giDistributionId "Use generic-lens or generic-optics with 'distributionId' instead." #-}

-- | The identifier for the invalidation request, for example, @IDFDVBD632BHDS5@ .
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giId :: Lens.Lens' GetInvalidation Lude.Text
giId = Lens.lens (id :: GetInvalidation -> Lude.Text) (\s a -> s {id = a} :: GetInvalidation)
{-# DEPRECATED giId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest GetInvalidation where
  type Rs GetInvalidation = GetInvalidationResponse
  request = Req.get cloudFrontService
  response =
    Res.receiveXML
      ( \s h x ->
          GetInvalidationResponse'
            Lude.<$> (Lude.parseXML x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetInvalidation where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetInvalidation where
  toPath GetInvalidation' {..} =
    Lude.mconcat
      [ "/2020-05-31/distribution/",
        Lude.toBS distributionId,
        "/invalidation/",
        Lude.toBS id
      ]

instance Lude.ToQuery GetInvalidation where
  toQuery = Lude.const Lude.mempty

-- | The returned result of the corresponding request.
--
-- /See:/ 'mkGetInvalidationResponse' smart constructor.
data GetInvalidationResponse = GetInvalidationResponse'
  { invalidation ::
      Lude.Maybe Invalidation,
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

-- | Creates a value of 'GetInvalidationResponse' with the minimum fields required to make a request.
--
-- * 'invalidation' - The invalidation's information. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/InvalidationDatatype.html Invalidation Complex Type> .
-- * 'responseStatus' - The response status code.
mkGetInvalidationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetInvalidationResponse
mkGetInvalidationResponse pResponseStatus_ =
  GetInvalidationResponse'
    { invalidation = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The invalidation's information. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/InvalidationDatatype.html Invalidation Complex Type> .
--
-- /Note:/ Consider using 'invalidation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girsInvalidation :: Lens.Lens' GetInvalidationResponse (Lude.Maybe Invalidation)
girsInvalidation = Lens.lens (invalidation :: GetInvalidationResponse -> Lude.Maybe Invalidation) (\s a -> s {invalidation = a} :: GetInvalidationResponse)
{-# DEPRECATED girsInvalidation "Use generic-lens or generic-optics with 'invalidation' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girsResponseStatus :: Lens.Lens' GetInvalidationResponse Lude.Int
girsResponseStatus = Lens.lens (responseStatus :: GetInvalidationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetInvalidationResponse)
{-# DEPRECATED girsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
