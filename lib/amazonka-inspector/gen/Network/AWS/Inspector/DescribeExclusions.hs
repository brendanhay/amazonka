{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.DescribeExclusions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the exclusions that are specified by the exclusions' ARNs.
module Network.AWS.Inspector.DescribeExclusions
  ( -- * Creating a request
    DescribeExclusions (..),
    mkDescribeExclusions,

    -- ** Request lenses
    deLocale,
    deExclusionARNs,

    -- * Destructuring the response
    DescribeExclusionsResponse (..),
    mkDescribeExclusionsResponse,

    -- ** Response lenses
    dersResponseStatus,
    dersExclusions,
    dersFailedItems,
  )
where

import Network.AWS.Inspector.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeExclusions' smart constructor.
data DescribeExclusions = DescribeExclusions'
  { locale ::
      Lude.Maybe Locale,
    exclusionARNs :: Lude.NonEmpty Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeExclusions' with the minimum fields required to make a request.
--
-- * 'exclusionARNs' - The list of ARNs that specify the exclusions that you want to describe.
-- * 'locale' - The locale into which you want to translate the exclusion's title, description, and recommendation.
mkDescribeExclusions ::
  -- | 'exclusionARNs'
  Lude.NonEmpty Lude.Text ->
  DescribeExclusions
mkDescribeExclusions pExclusionARNs_ =
  DescribeExclusions'
    { locale = Lude.Nothing,
      exclusionARNs = pExclusionARNs_
    }

-- | The locale into which you want to translate the exclusion's title, description, and recommendation.
--
-- /Note:/ Consider using 'locale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deLocale :: Lens.Lens' DescribeExclusions (Lude.Maybe Locale)
deLocale = Lens.lens (locale :: DescribeExclusions -> Lude.Maybe Locale) (\s a -> s {locale = a} :: DescribeExclusions)
{-# DEPRECATED deLocale "Use generic-lens or generic-optics with 'locale' instead." #-}

-- | The list of ARNs that specify the exclusions that you want to describe.
--
-- /Note:/ Consider using 'exclusionARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deExclusionARNs :: Lens.Lens' DescribeExclusions (Lude.NonEmpty Lude.Text)
deExclusionARNs = Lens.lens (exclusionARNs :: DescribeExclusions -> Lude.NonEmpty Lude.Text) (\s a -> s {exclusionARNs = a} :: DescribeExclusions)
{-# DEPRECATED deExclusionARNs "Use generic-lens or generic-optics with 'exclusionARNs' instead." #-}

instance Lude.AWSRequest DescribeExclusions where
  type Rs DescribeExclusions = DescribeExclusionsResponse
  request = Req.postJSON inspectorService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeExclusionsResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..?> "exclusions" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "failedItems" Lude..!@ Lude.mempty)
      )

instance Lude.ToHeaders DescribeExclusions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("InspectorService.DescribeExclusions" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeExclusions where
  toJSON DescribeExclusions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("locale" Lude..=) Lude.<$> locale,
            Lude.Just ("exclusionArns" Lude..= exclusionARNs)
          ]
      )

instance Lude.ToPath DescribeExclusions where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeExclusions where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeExclusionsResponse' smart constructor.
data DescribeExclusionsResponse = DescribeExclusionsResponse'
  { responseStatus ::
      Lude.Int,
    exclusions ::
      Lude.HashMap Lude.Text (Exclusion),
    failedItems ::
      Lude.HashMap
        Lude.Text
        (FailedItemDetails)
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeExclusionsResponse' with the minimum fields required to make a request.
--
-- * 'exclusions' - Information about the exclusions.
-- * 'failedItems' - Exclusion details that cannot be described. An error code is provided for each failed item.
-- * 'responseStatus' - The response status code.
mkDescribeExclusionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeExclusionsResponse
mkDescribeExclusionsResponse pResponseStatus_ =
  DescribeExclusionsResponse'
    { responseStatus = pResponseStatus_,
      exclusions = Lude.mempty,
      failedItems = Lude.mempty
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dersResponseStatus :: Lens.Lens' DescribeExclusionsResponse Lude.Int
dersResponseStatus = Lens.lens (responseStatus :: DescribeExclusionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeExclusionsResponse)
{-# DEPRECATED dersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Information about the exclusions.
--
-- /Note:/ Consider using 'exclusions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dersExclusions :: Lens.Lens' DescribeExclusionsResponse (Lude.HashMap Lude.Text (Exclusion))
dersExclusions = Lens.lens (exclusions :: DescribeExclusionsResponse -> Lude.HashMap Lude.Text (Exclusion)) (\s a -> s {exclusions = a} :: DescribeExclusionsResponse)
{-# DEPRECATED dersExclusions "Use generic-lens or generic-optics with 'exclusions' instead." #-}

-- | Exclusion details that cannot be described. An error code is provided for each failed item.
--
-- /Note:/ Consider using 'failedItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dersFailedItems :: Lens.Lens' DescribeExclusionsResponse (Lude.HashMap Lude.Text (FailedItemDetails))
dersFailedItems = Lens.lens (failedItems :: DescribeExclusionsResponse -> Lude.HashMap Lude.Text (FailedItemDetails)) (\s a -> s {failedItems = a} :: DescribeExclusionsResponse)
{-# DEPRECATED dersFailedItems "Use generic-lens or generic-optics with 'failedItems' instead." #-}
