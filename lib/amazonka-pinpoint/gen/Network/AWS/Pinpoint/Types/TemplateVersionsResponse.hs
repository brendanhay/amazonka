{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.TemplateVersionsResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.TemplateVersionsResponse
  ( TemplateVersionsResponse (..),

    -- * Smart constructor
    mkTemplateVersionsResponse,

    -- * Lenses
    tvRequestId,
    tvNextToken,
    tvItem,
    tvMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.TemplateVersionResponse
import qualified Network.AWS.Prelude as Lude

-- | Provides information about all the versions of a specific message template.
--
-- /See:/ 'mkTemplateVersionsResponse' smart constructor.
data TemplateVersionsResponse = TemplateVersionsResponse'
  { -- | The unique identifier for the request to retrieve information about all the versions of the message template.
    requestId :: Lude.Maybe Lude.Text,
    -- | The string to use in a subsequent request to get the next page of results in a paginated response. This value is null if there are no additional pages.
    nextToken :: Lude.Maybe Lude.Text,
    -- | An array of responses, one for each version of the message template.
    item :: [TemplateVersionResponse],
    -- | The message that's returned from the API for the request to retrieve information about all the versions of the message template.
    message :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TemplateVersionsResponse' with the minimum fields required to make a request.
--
-- * 'requestId' - The unique identifier for the request to retrieve information about all the versions of the message template.
-- * 'nextToken' - The string to use in a subsequent request to get the next page of results in a paginated response. This value is null if there are no additional pages.
-- * 'item' - An array of responses, one for each version of the message template.
-- * 'message' - The message that's returned from the API for the request to retrieve information about all the versions of the message template.
mkTemplateVersionsResponse ::
  TemplateVersionsResponse
mkTemplateVersionsResponse =
  TemplateVersionsResponse'
    { requestId = Lude.Nothing,
      nextToken = Lude.Nothing,
      item = Lude.mempty,
      message = Lude.Nothing
    }

-- | The unique identifier for the request to retrieve information about all the versions of the message template.
--
-- /Note:/ Consider using 'requestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tvRequestId :: Lens.Lens' TemplateVersionsResponse (Lude.Maybe Lude.Text)
tvRequestId = Lens.lens (requestId :: TemplateVersionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {requestId = a} :: TemplateVersionsResponse)
{-# DEPRECATED tvRequestId "Use generic-lens or generic-optics with 'requestId' instead." #-}

-- | The string to use in a subsequent request to get the next page of results in a paginated response. This value is null if there are no additional pages.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tvNextToken :: Lens.Lens' TemplateVersionsResponse (Lude.Maybe Lude.Text)
tvNextToken = Lens.lens (nextToken :: TemplateVersionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: TemplateVersionsResponse)
{-# DEPRECATED tvNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | An array of responses, one for each version of the message template.
--
-- /Note:/ Consider using 'item' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tvItem :: Lens.Lens' TemplateVersionsResponse [TemplateVersionResponse]
tvItem = Lens.lens (item :: TemplateVersionsResponse -> [TemplateVersionResponse]) (\s a -> s {item = a} :: TemplateVersionsResponse)
{-# DEPRECATED tvItem "Use generic-lens or generic-optics with 'item' instead." #-}

-- | The message that's returned from the API for the request to retrieve information about all the versions of the message template.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tvMessage :: Lens.Lens' TemplateVersionsResponse (Lude.Maybe Lude.Text)
tvMessage = Lens.lens (message :: TemplateVersionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: TemplateVersionsResponse)
{-# DEPRECATED tvMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromJSON TemplateVersionsResponse where
  parseJSON =
    Lude.withObject
      "TemplateVersionsResponse"
      ( \x ->
          TemplateVersionsResponse'
            Lude.<$> (x Lude..:? "RequestID")
            Lude.<*> (x Lude..:? "NextToken")
            Lude.<*> (x Lude..:? "Item" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Message")
      )
