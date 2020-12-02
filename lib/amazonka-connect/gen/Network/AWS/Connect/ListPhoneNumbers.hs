{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
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
--
-- For more information about phone numbers, see <https://docs.aws.amazon.com/connect/latest/adminguide/contact-center-phone-number.html Set Up Phone Numbers for Your Contact Center> in the /Amazon Connect Administrator Guide/ .
--
--
-- This operation returns paginated results.
module Network.AWS.Connect.ListPhoneNumbers
  ( -- * Creating a Request
    listPhoneNumbers,
    ListPhoneNumbers,

    -- * Request Lenses
    lpnPhoneNumberTypes,
    lpnPhoneNumberCountryCodes,
    lpnNextToken,
    lpnMaxResults,
    lpnInstanceId,

    -- * Destructuring the Response
    listPhoneNumbersResponse,
    ListPhoneNumbersResponse,

    -- * Response Lenses
    lpnrsPhoneNumberSummaryList,
    lpnrsNextToken,
    lpnrsResponseStatus,
  )
where

import Network.AWS.Connect.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listPhoneNumbers' smart constructor.
data ListPhoneNumbers = ListPhoneNumbers'
  { _lpnPhoneNumberTypes ::
      !(Maybe [PhoneNumberType]),
    _lpnPhoneNumberCountryCodes ::
      !(Maybe [PhoneNumberCountryCode]),
    _lpnNextToken :: !(Maybe Text),
    _lpnMaxResults :: !(Maybe Nat),
    _lpnInstanceId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListPhoneNumbers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpnPhoneNumberTypes' - The type of phone number.
--
-- * 'lpnPhoneNumberCountryCodes' - The ISO country code.
--
-- * 'lpnNextToken' - The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
--
-- * 'lpnMaxResults' - The maximimum number of results to return per page.
--
-- * 'lpnInstanceId' - The identifier of the Amazon Connect instance.
listPhoneNumbers ::
  -- | 'lpnInstanceId'
  Text ->
  ListPhoneNumbers
listPhoneNumbers pInstanceId_ =
  ListPhoneNumbers'
    { _lpnPhoneNumberTypes = Nothing,
      _lpnPhoneNumberCountryCodes = Nothing,
      _lpnNextToken = Nothing,
      _lpnMaxResults = Nothing,
      _lpnInstanceId = pInstanceId_
    }

-- | The type of phone number.
lpnPhoneNumberTypes :: Lens' ListPhoneNumbers [PhoneNumberType]
lpnPhoneNumberTypes = lens _lpnPhoneNumberTypes (\s a -> s {_lpnPhoneNumberTypes = a}) . _Default . _Coerce

-- | The ISO country code.
lpnPhoneNumberCountryCodes :: Lens' ListPhoneNumbers [PhoneNumberCountryCode]
lpnPhoneNumberCountryCodes = lens _lpnPhoneNumberCountryCodes (\s a -> s {_lpnPhoneNumberCountryCodes = a}) . _Default . _Coerce

-- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
lpnNextToken :: Lens' ListPhoneNumbers (Maybe Text)
lpnNextToken = lens _lpnNextToken (\s a -> s {_lpnNextToken = a})

-- | The maximimum number of results to return per page.
lpnMaxResults :: Lens' ListPhoneNumbers (Maybe Natural)
lpnMaxResults = lens _lpnMaxResults (\s a -> s {_lpnMaxResults = a}) . mapping _Nat

-- | The identifier of the Amazon Connect instance.
lpnInstanceId :: Lens' ListPhoneNumbers Text
lpnInstanceId = lens _lpnInstanceId (\s a -> s {_lpnInstanceId = a})

instance AWSPager ListPhoneNumbers where
  page rq rs
    | stop (rs ^. lpnrsNextToken) = Nothing
    | stop (rs ^. lpnrsPhoneNumberSummaryList) = Nothing
    | otherwise = Just $ rq & lpnNextToken .~ rs ^. lpnrsNextToken

instance AWSRequest ListPhoneNumbers where
  type Rs ListPhoneNumbers = ListPhoneNumbersResponse
  request = get connect
  response =
    receiveJSON
      ( \s h x ->
          ListPhoneNumbersResponse'
            <$> (x .?> "PhoneNumberSummaryList" .!@ mempty)
            <*> (x .?> "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable ListPhoneNumbers

instance NFData ListPhoneNumbers

instance ToHeaders ListPhoneNumbers where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath ListPhoneNumbers where
  toPath ListPhoneNumbers' {..} =
    mconcat ["/phone-numbers-summary/", toBS _lpnInstanceId]

instance ToQuery ListPhoneNumbers where
  toQuery ListPhoneNumbers' {..} =
    mconcat
      [ "phoneNumberTypes"
          =: toQuery (toQueryList "member" <$> _lpnPhoneNumberTypes),
        "phoneNumberCountryCodes"
          =: toQuery (toQueryList "member" <$> _lpnPhoneNumberCountryCodes),
        "nextToken" =: _lpnNextToken,
        "maxResults" =: _lpnMaxResults
      ]

-- | /See:/ 'listPhoneNumbersResponse' smart constructor.
data ListPhoneNumbersResponse = ListPhoneNumbersResponse'
  { _lpnrsPhoneNumberSummaryList ::
      !(Maybe [PhoneNumberSummary]),
    _lpnrsNextToken :: !(Maybe Text),
    _lpnrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListPhoneNumbersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpnrsPhoneNumberSummaryList' - Information about the phone numbers.
--
-- * 'lpnrsNextToken' - If there are additional results, this is the token for the next set of results.
--
-- * 'lpnrsResponseStatus' - -- | The response status code.
listPhoneNumbersResponse ::
  -- | 'lpnrsResponseStatus'
  Int ->
  ListPhoneNumbersResponse
listPhoneNumbersResponse pResponseStatus_ =
  ListPhoneNumbersResponse'
    { _lpnrsPhoneNumberSummaryList = Nothing,
      _lpnrsNextToken = Nothing,
      _lpnrsResponseStatus = pResponseStatus_
    }

-- | Information about the phone numbers.
lpnrsPhoneNumberSummaryList :: Lens' ListPhoneNumbersResponse [PhoneNumberSummary]
lpnrsPhoneNumberSummaryList = lens _lpnrsPhoneNumberSummaryList (\s a -> s {_lpnrsPhoneNumberSummaryList = a}) . _Default . _Coerce

-- | If there are additional results, this is the token for the next set of results.
lpnrsNextToken :: Lens' ListPhoneNumbersResponse (Maybe Text)
lpnrsNextToken = lens _lpnrsNextToken (\s a -> s {_lpnrsNextToken = a})

-- | -- | The response status code.
lpnrsResponseStatus :: Lens' ListPhoneNumbersResponse Int
lpnrsResponseStatus = lens _lpnrsResponseStatus (\s a -> s {_lpnrsResponseStatus = a})

instance NFData ListPhoneNumbersResponse
