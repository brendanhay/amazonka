{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.TemplatesResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.TemplatesResponse
  ( TemplatesResponse (..),

    -- * Smart constructor
    mkTemplatesResponse,

    -- * Lenses
    tNextToken,
    tItem,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.TemplateResponse
import qualified Network.AWS.Prelude as Lude

-- | Provides information about all the message templates that are associated with your Amazon Pinpoint account.
--
-- /See:/ 'mkTemplatesResponse' smart constructor.
data TemplatesResponse = TemplatesResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    item :: [TemplateResponse]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TemplatesResponse' with the minimum fields required to make a request.
--
-- * 'item' - An array of responses, one for each message template that's associated with your Amazon Pinpoint account and meets any filter criteria that you specified in the request.
-- * 'nextToken' - The string to use in a subsequent request to get the next page of results in a paginated response. This value is null if there are no additional pages.
mkTemplatesResponse ::
  TemplatesResponse
mkTemplatesResponse =
  TemplatesResponse' {nextToken = Lude.Nothing, item = Lude.mempty}

-- | The string to use in a subsequent request to get the next page of results in a paginated response. This value is null if there are no additional pages.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tNextToken :: Lens.Lens' TemplatesResponse (Lude.Maybe Lude.Text)
tNextToken = Lens.lens (nextToken :: TemplatesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: TemplatesResponse)
{-# DEPRECATED tNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | An array of responses, one for each message template that's associated with your Amazon Pinpoint account and meets any filter criteria that you specified in the request.
--
-- /Note:/ Consider using 'item' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tItem :: Lens.Lens' TemplatesResponse [TemplateResponse]
tItem = Lens.lens (item :: TemplatesResponse -> [TemplateResponse]) (\s a -> s {item = a} :: TemplatesResponse)
{-# DEPRECATED tItem "Use generic-lens or generic-optics with 'item' instead." #-}

instance Lude.FromJSON TemplatesResponse where
  parseJSON =
    Lude.withObject
      "TemplatesResponse"
      ( \x ->
          TemplatesResponse'
            Lude.<$> (x Lude..:? "NextToken")
            Lude.<*> (x Lude..:? "Item" Lude..!= Lude.mempty)
      )
