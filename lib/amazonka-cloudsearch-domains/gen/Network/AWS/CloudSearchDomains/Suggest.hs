{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearchDomains.Suggest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves autocomplete suggestions for a partial query string. You can use suggestions enable you to display likely matches before users finish typing. In Amazon CloudSearch, suggestions are based on the contents of a particular text field. When you request suggestions, Amazon CloudSearch finds all of the documents whose values in the suggester field start with the specified query string. The beginning of the field must match the query string to be considered a match.
--
-- For more information about configuring suggesters and retrieving suggestions, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/getting-suggestions.html Getting Suggestions> in the /Amazon CloudSearch Developer Guide/ .
-- The endpoint for submitting @Suggest@ requests is domain-specific. You submit suggest requests to a domain's search endpoint. To get the search endpoint for your domain, use the Amazon CloudSearch configuration service @DescribeDomains@ action. A domain's endpoints are also displayed on the domain dashboard in the Amazon CloudSearch console.
module Network.AWS.CloudSearchDomains.Suggest
  ( -- * Creating a request
    Suggest (..),
    mkSuggest,

    -- ** Request lenses
    sfSize,
    sfQuery,
    sfSuggester,

    -- * Destructuring the response
    SuggestResponse (..),
    mkSuggestResponse,

    -- ** Response lenses
    srsSuggest,
    srsStatus,
    srsResponseStatus,
  )
where

import Network.AWS.CloudSearchDomains.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Container for the parameters to the @Suggest@ request.
--
-- /See:/ 'mkSuggest' smart constructor.
data Suggest = Suggest'
  { -- | Specifies the maximum number of suggestions to return.
    size :: Lude.Maybe Lude.Integer,
    -- | Specifies the string for which you want to get suggestions.
    query :: Lude.Text,
    -- | Specifies the name of the suggester to use to find suggested matches.
    suggester :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Suggest' with the minimum fields required to make a request.
--
-- * 'size' - Specifies the maximum number of suggestions to return.
-- * 'query' - Specifies the string for which you want to get suggestions.
-- * 'suggester' - Specifies the name of the suggester to use to find suggested matches.
mkSuggest ::
  -- | 'query'
  Lude.Text ->
  -- | 'suggester'
  Lude.Text ->
  Suggest
mkSuggest pQuery_ pSuggester_ =
  Suggest'
    { size = Lude.Nothing,
      query = pQuery_,
      suggester = pSuggester_
    }

-- | Specifies the maximum number of suggestions to return.
--
-- /Note:/ Consider using 'size' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfSize :: Lens.Lens' Suggest (Lude.Maybe Lude.Integer)
sfSize = Lens.lens (size :: Suggest -> Lude.Maybe Lude.Integer) (\s a -> s {size = a} :: Suggest)
{-# DEPRECATED sfSize "Use generic-lens or generic-optics with 'size' instead." #-}

-- | Specifies the string for which you want to get suggestions.
--
-- /Note:/ Consider using 'query' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfQuery :: Lens.Lens' Suggest Lude.Text
sfQuery = Lens.lens (query :: Suggest -> Lude.Text) (\s a -> s {query = a} :: Suggest)
{-# DEPRECATED sfQuery "Use generic-lens or generic-optics with 'query' instead." #-}

-- | Specifies the name of the suggester to use to find suggested matches.
--
-- /Note:/ Consider using 'suggester' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfSuggester :: Lens.Lens' Suggest Lude.Text
sfSuggester = Lens.lens (suggester :: Suggest -> Lude.Text) (\s a -> s {suggester = a} :: Suggest)
{-# DEPRECATED sfSuggester "Use generic-lens or generic-optics with 'suggester' instead." #-}

instance Lude.AWSRequest Suggest where
  type Rs Suggest = SuggestResponse
  request = Req.get cloudSearchDomainsService
  response =
    Res.receiveJSON
      ( \s h x ->
          SuggestResponse'
            Lude.<$> (x Lude..?> "suggest")
            Lude.<*> (x Lude..?> "status")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders Suggest where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath Suggest where
  toPath = Lude.const "/2013-01-01/suggest"

instance Lude.ToQuery Suggest where
  toQuery Suggest' {..} =
    Lude.mconcat
      [ "size" Lude.=: size,
        "q" Lude.=: query,
        "suggester" Lude.=: suggester,
        "format=sdk&pretty=true"
      ]

-- | Contains the response to a @Suggest@ request.
--
-- /See:/ 'mkSuggestResponse' smart constructor.
data SuggestResponse = SuggestResponse'
  { -- | Container for the matching search suggestion information.
    suggest :: Lude.Maybe SuggestModel,
    -- | The status of a @SuggestRequest@ . Contains the resource ID (@rid@ ) and how long it took to process the request (@timems@ ).
    status :: Lude.Maybe SuggestStatus,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SuggestResponse' with the minimum fields required to make a request.
--
-- * 'suggest' - Container for the matching search suggestion information.
-- * 'status' - The status of a @SuggestRequest@ . Contains the resource ID (@rid@ ) and how long it took to process the request (@timems@ ).
-- * 'responseStatus' - The response status code.
mkSuggestResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  SuggestResponse
mkSuggestResponse pResponseStatus_ =
  SuggestResponse'
    { suggest = Lude.Nothing,
      status = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Container for the matching search suggestion information.
--
-- /Note:/ Consider using 'suggest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsSuggest :: Lens.Lens' SuggestResponse (Lude.Maybe SuggestModel)
srsSuggest = Lens.lens (suggest :: SuggestResponse -> Lude.Maybe SuggestModel) (\s a -> s {suggest = a} :: SuggestResponse)
{-# DEPRECATED srsSuggest "Use generic-lens or generic-optics with 'suggest' instead." #-}

-- | The status of a @SuggestRequest@ . Contains the resource ID (@rid@ ) and how long it took to process the request (@timems@ ).
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsStatus :: Lens.Lens' SuggestResponse (Lude.Maybe SuggestStatus)
srsStatus = Lens.lens (status :: SuggestResponse -> Lude.Maybe SuggestStatus) (\s a -> s {status = a} :: SuggestResponse)
{-# DEPRECATED srsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResponseStatus :: Lens.Lens' SuggestResponse Lude.Int
srsResponseStatus = Lens.lens (responseStatus :: SuggestResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SuggestResponse)
{-# DEPRECATED srsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
