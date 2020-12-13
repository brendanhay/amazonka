{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.ListPhoneNumbers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information about the phone numbers for the specified Amazon Connect instance.
--
-- For more information about phone numbers, see <https://docs.aws.amazon.com/connect/latest/adminguide/contact-center-phone-number.html Set Up Phone Numbers for Your Contact Center> in the /Amazon Connect Administrator Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.Connect.ListPhoneNumbers
  ( -- * Creating a request
    ListPhoneNumbers (..),
    mkListPhoneNumbers,

    -- ** Request lenses
    lpnInstanceId,
    lpnPhoneNumberTypes,
    lpnPhoneNumberCountryCodes,
    lpnNextToken,
    lpnMaxResults,

    -- * Destructuring the response
    ListPhoneNumbersResponse (..),
    mkListPhoneNumbersResponse,

    -- ** Response lenses
    lpnrsPhoneNumberSummaryList,
    lpnrsNextToken,
    lpnrsResponseStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListPhoneNumbers' smart constructor.
data ListPhoneNumbers = ListPhoneNumbers'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Lude.Text,
    -- | The type of phone number.
    phoneNumberTypes :: Lude.Maybe [PhoneNumberType],
    -- | The ISO country code.
    phoneNumberCountryCodes :: Lude.Maybe [PhoneNumberCountryCode],
    -- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximimum number of results to return per page.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListPhoneNumbers' with the minimum fields required to make a request.
--
-- * 'instanceId' - The identifier of the Amazon Connect instance.
-- * 'phoneNumberTypes' - The type of phone number.
-- * 'phoneNumberCountryCodes' - The ISO country code.
-- * 'nextToken' - The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
-- * 'maxResults' - The maximimum number of results to return per page.
mkListPhoneNumbers ::
  -- | 'instanceId'
  Lude.Text ->
  ListPhoneNumbers
mkListPhoneNumbers pInstanceId_ =
  ListPhoneNumbers'
    { instanceId = pInstanceId_,
      phoneNumberTypes = Lude.Nothing,
      phoneNumberCountryCodes = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpnInstanceId :: Lens.Lens' ListPhoneNumbers Lude.Text
lpnInstanceId = Lens.lens (instanceId :: ListPhoneNumbers -> Lude.Text) (\s a -> s {instanceId = a} :: ListPhoneNumbers)
{-# DEPRECATED lpnInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The type of phone number.
--
-- /Note:/ Consider using 'phoneNumberTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpnPhoneNumberTypes :: Lens.Lens' ListPhoneNumbers (Lude.Maybe [PhoneNumberType])
lpnPhoneNumberTypes = Lens.lens (phoneNumberTypes :: ListPhoneNumbers -> Lude.Maybe [PhoneNumberType]) (\s a -> s {phoneNumberTypes = a} :: ListPhoneNumbers)
{-# DEPRECATED lpnPhoneNumberTypes "Use generic-lens or generic-optics with 'phoneNumberTypes' instead." #-}

-- | The ISO country code.
--
-- /Note:/ Consider using 'phoneNumberCountryCodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpnPhoneNumberCountryCodes :: Lens.Lens' ListPhoneNumbers (Lude.Maybe [PhoneNumberCountryCode])
lpnPhoneNumberCountryCodes = Lens.lens (phoneNumberCountryCodes :: ListPhoneNumbers -> Lude.Maybe [PhoneNumberCountryCode]) (\s a -> s {phoneNumberCountryCodes = a} :: ListPhoneNumbers)
{-# DEPRECATED lpnPhoneNumberCountryCodes "Use generic-lens or generic-optics with 'phoneNumberCountryCodes' instead." #-}

-- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpnNextToken :: Lens.Lens' ListPhoneNumbers (Lude.Maybe Lude.Text)
lpnNextToken = Lens.lens (nextToken :: ListPhoneNumbers -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListPhoneNumbers)
{-# DEPRECATED lpnNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximimum number of results to return per page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpnMaxResults :: Lens.Lens' ListPhoneNumbers (Lude.Maybe Lude.Natural)
lpnMaxResults = Lens.lens (maxResults :: ListPhoneNumbers -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListPhoneNumbers)
{-# DEPRECATED lpnMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListPhoneNumbers where
  page rq rs
    | Page.stop (rs Lens.^. lpnrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lpnrsPhoneNumberSummaryList) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lpnNextToken Lens..~ rs Lens.^. lpnrsNextToken

instance Lude.AWSRequest ListPhoneNumbers where
  type Rs ListPhoneNumbers = ListPhoneNumbersResponse
  request = Req.get connectService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListPhoneNumbersResponse'
            Lude.<$> (x Lude..?> "PhoneNumberSummaryList" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListPhoneNumbers where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListPhoneNumbers where
  toPath ListPhoneNumbers' {..} =
    Lude.mconcat ["/phone-numbers-summary/", Lude.toBS instanceId]

instance Lude.ToQuery ListPhoneNumbers where
  toQuery ListPhoneNumbers' {..} =
    Lude.mconcat
      [ "phoneNumberTypes"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> phoneNumberTypes),
        "phoneNumberCountryCodes"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "member" Lude.<$> phoneNumberCountryCodes),
        "nextToken" Lude.=: nextToken,
        "maxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkListPhoneNumbersResponse' smart constructor.
data ListPhoneNumbersResponse = ListPhoneNumbersResponse'
  { -- | Information about the phone numbers.
    phoneNumberSummaryList :: Lude.Maybe [PhoneNumberSummary],
    -- | If there are additional results, this is the token for the next set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListPhoneNumbersResponse' with the minimum fields required to make a request.
--
-- * 'phoneNumberSummaryList' - Information about the phone numbers.
-- * 'nextToken' - If there are additional results, this is the token for the next set of results.
-- * 'responseStatus' - The response status code.
mkListPhoneNumbersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListPhoneNumbersResponse
mkListPhoneNumbersResponse pResponseStatus_ =
  ListPhoneNumbersResponse'
    { phoneNumberSummaryList = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the phone numbers.
--
-- /Note:/ Consider using 'phoneNumberSummaryList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpnrsPhoneNumberSummaryList :: Lens.Lens' ListPhoneNumbersResponse (Lude.Maybe [PhoneNumberSummary])
lpnrsPhoneNumberSummaryList = Lens.lens (phoneNumberSummaryList :: ListPhoneNumbersResponse -> Lude.Maybe [PhoneNumberSummary]) (\s a -> s {phoneNumberSummaryList = a} :: ListPhoneNumbersResponse)
{-# DEPRECATED lpnrsPhoneNumberSummaryList "Use generic-lens or generic-optics with 'phoneNumberSummaryList' instead." #-}

-- | If there are additional results, this is the token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpnrsNextToken :: Lens.Lens' ListPhoneNumbersResponse (Lude.Maybe Lude.Text)
lpnrsNextToken = Lens.lens (nextToken :: ListPhoneNumbersResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListPhoneNumbersResponse)
{-# DEPRECATED lpnrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpnrsResponseStatus :: Lens.Lens' ListPhoneNumbersResponse Lude.Int
lpnrsResponseStatus = Lens.lens (responseStatus :: ListPhoneNumbersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListPhoneNumbersResponse)
{-# DEPRECATED lpnrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
