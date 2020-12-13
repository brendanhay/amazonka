{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.ListPhoneNumbersOptedOut
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of phone numbers that are opted out, meaning you cannot send SMS messages to them.
--
-- The results for @ListPhoneNumbersOptedOut@ are paginated, and each page returns up to 100 phone numbers. If additional phone numbers are available after the first page of results, then a @NextToken@ string will be returned. To receive the next page, you call @ListPhoneNumbersOptedOut@ again using the @NextToken@ string received from the previous call. When there are no more records to return, @NextToken@ will be null.
--
-- This operation returns paginated results.
module Network.AWS.SNS.ListPhoneNumbersOptedOut
  ( -- * Creating a request
    ListPhoneNumbersOptedOut (..),
    mkListPhoneNumbersOptedOut,

    -- ** Request lenses
    lpnooNextToken,

    -- * Destructuring the response
    ListPhoneNumbersOptedOutResponse (..),
    mkListPhoneNumbersOptedOutResponse,

    -- ** Response lenses
    lpnoorsPhoneNumbers,
    lpnoorsNextToken,
    lpnoorsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SNS.Types

-- | The input for the @ListPhoneNumbersOptedOut@ action.
--
-- /See:/ 'mkListPhoneNumbersOptedOut' smart constructor.
newtype ListPhoneNumbersOptedOut = ListPhoneNumbersOptedOut'
  { -- | A @NextToken@ string is used when you call the @ListPhoneNumbersOptedOut@ action to retrieve additional records that are available after the first page of results.
    nextToken :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListPhoneNumbersOptedOut' with the minimum fields required to make a request.
--
-- * 'nextToken' - A @NextToken@ string is used when you call the @ListPhoneNumbersOptedOut@ action to retrieve additional records that are available after the first page of results.
mkListPhoneNumbersOptedOut ::
  ListPhoneNumbersOptedOut
mkListPhoneNumbersOptedOut =
  ListPhoneNumbersOptedOut' {nextToken = Lude.Nothing}

-- | A @NextToken@ string is used when you call the @ListPhoneNumbersOptedOut@ action to retrieve additional records that are available after the first page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpnooNextToken :: Lens.Lens' ListPhoneNumbersOptedOut (Lude.Maybe Lude.Text)
lpnooNextToken = Lens.lens (nextToken :: ListPhoneNumbersOptedOut -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListPhoneNumbersOptedOut)
{-# DEPRECATED lpnooNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Page.AWSPager ListPhoneNumbersOptedOut where
  page rq rs
    | Page.stop (rs Lens.^. lpnoorsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lpnoorsPhoneNumbers) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lpnooNextToken Lens..~ rs Lens.^. lpnoorsNextToken

instance Lude.AWSRequest ListPhoneNumbersOptedOut where
  type Rs ListPhoneNumbersOptedOut = ListPhoneNumbersOptedOutResponse
  request = Req.postQuery snsService
  response =
    Res.receiveXMLWrapper
      "ListPhoneNumbersOptedOutResult"
      ( \s h x ->
          ListPhoneNumbersOptedOutResponse'
            Lude.<$> ( x Lude..@? "phoneNumbers" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (x Lude..@? "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListPhoneNumbersOptedOut where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListPhoneNumbersOptedOut where
  toPath = Lude.const "/"

instance Lude.ToQuery ListPhoneNumbersOptedOut where
  toQuery ListPhoneNumbersOptedOut' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ListPhoneNumbersOptedOut" :: Lude.ByteString),
        "Version" Lude.=: ("2010-03-31" :: Lude.ByteString),
        "nextToken" Lude.=: nextToken
      ]

-- | The response from the @ListPhoneNumbersOptedOut@ action.
--
-- /See:/ 'mkListPhoneNumbersOptedOutResponse' smart constructor.
data ListPhoneNumbersOptedOutResponse = ListPhoneNumbersOptedOutResponse'
  { -- | A list of phone numbers that are opted out of receiving SMS messages. The list is paginated, and each page can contain up to 100 phone numbers.
    phoneNumbers :: Lude.Maybe [Lude.Text],
    -- | A @NextToken@ string is returned when you call the @ListPhoneNumbersOptedOut@ action if additional records are available after the first page of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListPhoneNumbersOptedOutResponse' with the minimum fields required to make a request.
--
-- * 'phoneNumbers' - A list of phone numbers that are opted out of receiving SMS messages. The list is paginated, and each page can contain up to 100 phone numbers.
-- * 'nextToken' - A @NextToken@ string is returned when you call the @ListPhoneNumbersOptedOut@ action if additional records are available after the first page of results.
-- * 'responseStatus' - The response status code.
mkListPhoneNumbersOptedOutResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListPhoneNumbersOptedOutResponse
mkListPhoneNumbersOptedOutResponse pResponseStatus_ =
  ListPhoneNumbersOptedOutResponse'
    { phoneNumbers = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of phone numbers that are opted out of receiving SMS messages. The list is paginated, and each page can contain up to 100 phone numbers.
--
-- /Note:/ Consider using 'phoneNumbers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpnoorsPhoneNumbers :: Lens.Lens' ListPhoneNumbersOptedOutResponse (Lude.Maybe [Lude.Text])
lpnoorsPhoneNumbers = Lens.lens (phoneNumbers :: ListPhoneNumbersOptedOutResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {phoneNumbers = a} :: ListPhoneNumbersOptedOutResponse)
{-# DEPRECATED lpnoorsPhoneNumbers "Use generic-lens or generic-optics with 'phoneNumbers' instead." #-}

-- | A @NextToken@ string is returned when you call the @ListPhoneNumbersOptedOut@ action if additional records are available after the first page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpnoorsNextToken :: Lens.Lens' ListPhoneNumbersOptedOutResponse (Lude.Maybe Lude.Text)
lpnoorsNextToken = Lens.lens (nextToken :: ListPhoneNumbersOptedOutResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListPhoneNumbersOptedOutResponse)
{-# DEPRECATED lpnoorsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpnoorsResponseStatus :: Lens.Lens' ListPhoneNumbersOptedOutResponse Lude.Int
lpnoorsResponseStatus = Lens.lens (responseStatus :: ListPhoneNumbersOptedOutResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListPhoneNumbersOptedOutResponse)
{-# DEPRECATED lpnoorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
