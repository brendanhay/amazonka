{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.CreateInvalidation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a new invalidation.
module Network.AWS.CloudFront.CreateInvalidation
  ( -- * Creating a request
    CreateInvalidation (..),
    mkCreateInvalidation,

    -- ** Request lenses
    ciDistributionId,
    ciInvalidationBatch,

    -- * Destructuring the response
    CreateInvalidationResponse (..),
    mkCreateInvalidationResponse,

    -- ** Response lenses
    cirsInvalidation,
    cirsLocation,
    cirsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The request to create an invalidation.
--
-- /See:/ 'mkCreateInvalidation' smart constructor.
data CreateInvalidation = CreateInvalidation'
  { distributionId ::
      Lude.Text,
    invalidationBatch :: InvalidationBatch
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateInvalidation' with the minimum fields required to make a request.
--
-- * 'distributionId' - The distribution's id.
-- * 'invalidationBatch' - The batch information for the invalidation.
mkCreateInvalidation ::
  -- | 'distributionId'
  Lude.Text ->
  -- | 'invalidationBatch'
  InvalidationBatch ->
  CreateInvalidation
mkCreateInvalidation pDistributionId_ pInvalidationBatch_ =
  CreateInvalidation'
    { distributionId = pDistributionId_,
      invalidationBatch = pInvalidationBatch_
    }

-- | The distribution's id.
--
-- /Note:/ Consider using 'distributionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciDistributionId :: Lens.Lens' CreateInvalidation Lude.Text
ciDistributionId = Lens.lens (distributionId :: CreateInvalidation -> Lude.Text) (\s a -> s {distributionId = a} :: CreateInvalidation)
{-# DEPRECATED ciDistributionId "Use generic-lens or generic-optics with 'distributionId' instead." #-}

-- | The batch information for the invalidation.
--
-- /Note:/ Consider using 'invalidationBatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciInvalidationBatch :: Lens.Lens' CreateInvalidation InvalidationBatch
ciInvalidationBatch = Lens.lens (invalidationBatch :: CreateInvalidation -> InvalidationBatch) (\s a -> s {invalidationBatch = a} :: CreateInvalidation)
{-# DEPRECATED ciInvalidationBatch "Use generic-lens or generic-optics with 'invalidationBatch' instead." #-}

instance Lude.AWSRequest CreateInvalidation where
  type Rs CreateInvalidation = CreateInvalidationResponse
  request = Req.postXML cloudFrontService
  response =
    Res.receiveXML
      ( \s h x ->
          CreateInvalidationResponse'
            Lude.<$> (Lude.parseXML x)
            Lude.<*> (h Lude..#? "Location")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToElement CreateInvalidation where
  toElement =
    Lude.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}InvalidationBatch"
      Lude.. invalidationBatch

instance Lude.ToHeaders CreateInvalidation where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateInvalidation where
  toPath CreateInvalidation' {..} =
    Lude.mconcat
      [ "/2020-05-31/distribution/",
        Lude.toBS distributionId,
        "/invalidation"
      ]

instance Lude.ToQuery CreateInvalidation where
  toQuery = Lude.const Lude.mempty

-- | The returned result of the corresponding request.
--
-- /See:/ 'mkCreateInvalidationResponse' smart constructor.
data CreateInvalidationResponse = CreateInvalidationResponse'
  { invalidation ::
      Lude.Maybe Invalidation,
    location :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'CreateInvalidationResponse' with the minimum fields required to make a request.
--
-- * 'invalidation' - The invalidation's information.
-- * 'location' - The fully qualified URI of the distribution and invalidation batch request, including the @Invalidation ID@ .
-- * 'responseStatus' - The response status code.
mkCreateInvalidationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateInvalidationResponse
mkCreateInvalidationResponse pResponseStatus_ =
  CreateInvalidationResponse'
    { invalidation = Lude.Nothing,
      location = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The invalidation's information.
--
-- /Note:/ Consider using 'invalidation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cirsInvalidation :: Lens.Lens' CreateInvalidationResponse (Lude.Maybe Invalidation)
cirsInvalidation = Lens.lens (invalidation :: CreateInvalidationResponse -> Lude.Maybe Invalidation) (\s a -> s {invalidation = a} :: CreateInvalidationResponse)
{-# DEPRECATED cirsInvalidation "Use generic-lens or generic-optics with 'invalidation' instead." #-}

-- | The fully qualified URI of the distribution and invalidation batch request, including the @Invalidation ID@ .
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cirsLocation :: Lens.Lens' CreateInvalidationResponse (Lude.Maybe Lude.Text)
cirsLocation = Lens.lens (location :: CreateInvalidationResponse -> Lude.Maybe Lude.Text) (\s a -> s {location = a} :: CreateInvalidationResponse)
{-# DEPRECATED cirsLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cirsResponseStatus :: Lens.Lens' CreateInvalidationResponse Lude.Int
cirsResponseStatus = Lens.lens (responseStatus :: CreateInvalidationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateInvalidationResponse)
{-# DEPRECATED cirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
