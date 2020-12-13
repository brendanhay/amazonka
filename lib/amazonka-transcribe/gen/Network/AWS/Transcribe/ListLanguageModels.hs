{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.ListLanguageModels
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides more information about the custom language models you've created. You can use the information in this list to find a specific custom language model. You can then use the operation to get more information about it.
module Network.AWS.Transcribe.ListLanguageModels
  ( -- * Creating a request
    ListLanguageModels (..),
    mkListLanguageModels,

    -- ** Request lenses
    llmNameContains,
    llmNextToken,
    llmStatusEquals,
    llmMaxResults,

    -- * Destructuring the response
    ListLanguageModelsResponse (..),
    mkListLanguageModelsResponse,

    -- ** Response lenses
    llmrsNextToken,
    llmrsModels,
    llmrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Transcribe.Types

-- | /See:/ 'mkListLanguageModels' smart constructor.
data ListLanguageModels = ListLanguageModels'
  { -- | When specified, the custom language model names returned contain the substring you've specified.
    nameContains :: Lude.Maybe Lude.Text,
    -- | When included, fetches the next set of jobs if the result of the previous request was truncated.
    nextToken :: Lude.Maybe Lude.Text,
    -- | When specified, returns only custom language models with the specified status. Language models are ordered by creation date, with the newest models first. If you don't specify a status, Amazon Transcribe returns all custom language models ordered by date.
    statusEquals :: Lude.Maybe ModelStatus,
    -- | The maximum number of language models to return in the response. If there are fewer results in the list, the response contains only the actual results.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListLanguageModels' with the minimum fields required to make a request.
--
-- * 'nameContains' - When specified, the custom language model names returned contain the substring you've specified.
-- * 'nextToken' - When included, fetches the next set of jobs if the result of the previous request was truncated.
-- * 'statusEquals' - When specified, returns only custom language models with the specified status. Language models are ordered by creation date, with the newest models first. If you don't specify a status, Amazon Transcribe returns all custom language models ordered by date.
-- * 'maxResults' - The maximum number of language models to return in the response. If there are fewer results in the list, the response contains only the actual results.
mkListLanguageModels ::
  ListLanguageModels
mkListLanguageModels =
  ListLanguageModels'
    { nameContains = Lude.Nothing,
      nextToken = Lude.Nothing,
      statusEquals = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | When specified, the custom language model names returned contain the substring you've specified.
--
-- /Note:/ Consider using 'nameContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llmNameContains :: Lens.Lens' ListLanguageModels (Lude.Maybe Lude.Text)
llmNameContains = Lens.lens (nameContains :: ListLanguageModels -> Lude.Maybe Lude.Text) (\s a -> s {nameContains = a} :: ListLanguageModels)
{-# DEPRECATED llmNameContains "Use generic-lens or generic-optics with 'nameContains' instead." #-}

-- | When included, fetches the next set of jobs if the result of the previous request was truncated.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llmNextToken :: Lens.Lens' ListLanguageModels (Lude.Maybe Lude.Text)
llmNextToken = Lens.lens (nextToken :: ListLanguageModels -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListLanguageModels)
{-# DEPRECATED llmNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | When specified, returns only custom language models with the specified status. Language models are ordered by creation date, with the newest models first. If you don't specify a status, Amazon Transcribe returns all custom language models ordered by date.
--
-- /Note:/ Consider using 'statusEquals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llmStatusEquals :: Lens.Lens' ListLanguageModels (Lude.Maybe ModelStatus)
llmStatusEquals = Lens.lens (statusEquals :: ListLanguageModels -> Lude.Maybe ModelStatus) (\s a -> s {statusEquals = a} :: ListLanguageModels)
{-# DEPRECATED llmStatusEquals "Use generic-lens or generic-optics with 'statusEquals' instead." #-}

-- | The maximum number of language models to return in the response. If there are fewer results in the list, the response contains only the actual results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llmMaxResults :: Lens.Lens' ListLanguageModels (Lude.Maybe Lude.Natural)
llmMaxResults = Lens.lens (maxResults :: ListLanguageModels -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListLanguageModels)
{-# DEPRECATED llmMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Lude.AWSRequest ListLanguageModels where
  type Rs ListLanguageModels = ListLanguageModelsResponse
  request = Req.postJSON transcribeService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListLanguageModelsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Models" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListLanguageModels where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Transcribe.ListLanguageModels" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListLanguageModels where
  toJSON ListLanguageModels' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NameContains" Lude..=) Lude.<$> nameContains,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("StatusEquals" Lude..=) Lude.<$> statusEquals,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListLanguageModels where
  toPath = Lude.const "/"

instance Lude.ToQuery ListLanguageModels where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListLanguageModelsResponse' smart constructor.
data ListLanguageModelsResponse = ListLanguageModelsResponse'
  { -- | The operation returns a page of jobs at a time. The maximum size of the list is set by the MaxResults parameter. If there are more language models in the list than the page size, Amazon Transcribe returns the @NextPage@ token. Include the token in the next request to the operation to return the next page of language models.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A list of objects containing information about custom language models.
    models :: Lude.Maybe [LanguageModel],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListLanguageModelsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The operation returns a page of jobs at a time. The maximum size of the list is set by the MaxResults parameter. If there are more language models in the list than the page size, Amazon Transcribe returns the @NextPage@ token. Include the token in the next request to the operation to return the next page of language models.
-- * 'models' - A list of objects containing information about custom language models.
-- * 'responseStatus' - The response status code.
mkListLanguageModelsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListLanguageModelsResponse
mkListLanguageModelsResponse pResponseStatus_ =
  ListLanguageModelsResponse'
    { nextToken = Lude.Nothing,
      models = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The operation returns a page of jobs at a time. The maximum size of the list is set by the MaxResults parameter. If there are more language models in the list than the page size, Amazon Transcribe returns the @NextPage@ token. Include the token in the next request to the operation to return the next page of language models.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llmrsNextToken :: Lens.Lens' ListLanguageModelsResponse (Lude.Maybe Lude.Text)
llmrsNextToken = Lens.lens (nextToken :: ListLanguageModelsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListLanguageModelsResponse)
{-# DEPRECATED llmrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of objects containing information about custom language models.
--
-- /Note:/ Consider using 'models' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llmrsModels :: Lens.Lens' ListLanguageModelsResponse (Lude.Maybe [LanguageModel])
llmrsModels = Lens.lens (models :: ListLanguageModelsResponse -> Lude.Maybe [LanguageModel]) (\s a -> s {models = a} :: ListLanguageModelsResponse)
{-# DEPRECATED llmrsModels "Use generic-lens or generic-optics with 'models' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llmrsResponseStatus :: Lens.Lens' ListLanguageModelsResponse Lude.Int
llmrsResponseStatus = Lens.lens (responseStatus :: ListLanguageModelsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListLanguageModelsResponse)
{-# DEPRECATED llmrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
