{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.GetSearchSuggestions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- An auto-complete API for the search functionality in the Amazon SageMaker console. It returns suggestions of possible matches for the property name to use in @Search@ queries. Provides suggestions for @HyperParameters@ , @Tags@ , and @Metrics@ .
module Network.AWS.SageMaker.GetSearchSuggestions
  ( -- * Creating a request
    GetSearchSuggestions (..),
    mkGetSearchSuggestions,

    -- ** Request lenses
    gssSuggestionQuery,
    gssResource,

    -- * Destructuring the response
    GetSearchSuggestionsResponse (..),
    mkGetSearchSuggestionsResponse,

    -- ** Response lenses
    gssrsPropertyNameSuggestions,
    gssrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkGetSearchSuggestions' smart constructor.
data GetSearchSuggestions = GetSearchSuggestions'
  { suggestionQuery ::
      Lude.Maybe SuggestionQuery,
    resource :: ResourceType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSearchSuggestions' with the minimum fields required to make a request.
--
-- * 'resource' - The name of the Amazon SageMaker resource to search for.
-- * 'suggestionQuery' - Limits the property names that are included in the response.
mkGetSearchSuggestions ::
  -- | 'resource'
  ResourceType ->
  GetSearchSuggestions
mkGetSearchSuggestions pResource_ =
  GetSearchSuggestions'
    { suggestionQuery = Lude.Nothing,
      resource = pResource_
    }

-- | Limits the property names that are included in the response.
--
-- /Note:/ Consider using 'suggestionQuery' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gssSuggestionQuery :: Lens.Lens' GetSearchSuggestions (Lude.Maybe SuggestionQuery)
gssSuggestionQuery = Lens.lens (suggestionQuery :: GetSearchSuggestions -> Lude.Maybe SuggestionQuery) (\s a -> s {suggestionQuery = a} :: GetSearchSuggestions)
{-# DEPRECATED gssSuggestionQuery "Use generic-lens or generic-optics with 'suggestionQuery' instead." #-}

-- | The name of the Amazon SageMaker resource to search for.
--
-- /Note:/ Consider using 'resource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gssResource :: Lens.Lens' GetSearchSuggestions ResourceType
gssResource = Lens.lens (resource :: GetSearchSuggestions -> ResourceType) (\s a -> s {resource = a} :: GetSearchSuggestions)
{-# DEPRECATED gssResource "Use generic-lens or generic-optics with 'resource' instead." #-}

instance Lude.AWSRequest GetSearchSuggestions where
  type Rs GetSearchSuggestions = GetSearchSuggestionsResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetSearchSuggestionsResponse'
            Lude.<$> (x Lude..?> "PropertyNameSuggestions" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetSearchSuggestions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.GetSearchSuggestions" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetSearchSuggestions where
  toJSON GetSearchSuggestions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SuggestionQuery" Lude..=) Lude.<$> suggestionQuery,
            Lude.Just ("Resource" Lude..= resource)
          ]
      )

instance Lude.ToPath GetSearchSuggestions where
  toPath = Lude.const "/"

instance Lude.ToQuery GetSearchSuggestions where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetSearchSuggestionsResponse' smart constructor.
data GetSearchSuggestionsResponse = GetSearchSuggestionsResponse'
  { propertyNameSuggestions ::
      Lude.Maybe
        [PropertyNameSuggestion],
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

-- | Creates a value of 'GetSearchSuggestionsResponse' with the minimum fields required to make a request.
--
-- * 'propertyNameSuggestions' - A list of property names for a @Resource@ that match a @SuggestionQuery@ .
-- * 'responseStatus' - The response status code.
mkGetSearchSuggestionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetSearchSuggestionsResponse
mkGetSearchSuggestionsResponse pResponseStatus_ =
  GetSearchSuggestionsResponse'
    { propertyNameSuggestions =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of property names for a @Resource@ that match a @SuggestionQuery@ .
--
-- /Note:/ Consider using 'propertyNameSuggestions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gssrsPropertyNameSuggestions :: Lens.Lens' GetSearchSuggestionsResponse (Lude.Maybe [PropertyNameSuggestion])
gssrsPropertyNameSuggestions = Lens.lens (propertyNameSuggestions :: GetSearchSuggestionsResponse -> Lude.Maybe [PropertyNameSuggestion]) (\s a -> s {propertyNameSuggestions = a} :: GetSearchSuggestionsResponse)
{-# DEPRECATED gssrsPropertyNameSuggestions "Use generic-lens or generic-optics with 'propertyNameSuggestions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gssrsResponseStatus :: Lens.Lens' GetSearchSuggestionsResponse Lude.Int
gssrsResponseStatus = Lens.lens (responseStatus :: GetSearchSuggestionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetSearchSuggestionsResponse)
{-# DEPRECATED gssrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
