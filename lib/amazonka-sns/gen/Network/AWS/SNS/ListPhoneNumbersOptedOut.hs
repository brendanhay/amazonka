{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.ListPhoneNumbersOptedOut
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of phone numbers that are opted out, meaning you cannot send SMS messages to them.
--
--
-- The results for @ListPhoneNumbersOptedOut@ are paginated, and each page returns up to 100 phone numbers. If additional phone numbers are available after the first page of results, then a @NextToken@ string will be returned. To receive the next page, you call @ListPhoneNumbersOptedOut@ again using the @NextToken@ string received from the previous call. When there are no more records to return, @NextToken@ will be null.
--
module Network.AWS.SNS.ListPhoneNumbersOptedOut
    (
    -- * Creating a Request
      listPhoneNumbersOptedOut
    , ListPhoneNumbersOptedOut
    -- * Request Lenses
    , lpnooNextToken

    -- * Destructuring the Response
    , listPhoneNumbersOptedOutResponse
    , ListPhoneNumbersOptedOutResponse
    -- * Response Lenses
    , lpnoorsPhoneNumbers
    , lpnoorsNextToken
    , lpnoorsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SNS.Types
import Network.AWS.SNS.Types.Product

-- | The input for the @ListPhoneNumbersOptedOut@ action.
--
--
--
-- /See:/ 'listPhoneNumbersOptedOut' smart constructor.
newtype ListPhoneNumbersOptedOut = ListPhoneNumbersOptedOut'
  { _lpnooNextToken :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListPhoneNumbersOptedOut' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpnooNextToken' - A @NextToken@ string is used when you call the @ListPhoneNumbersOptedOut@ action to retrieve additional records that are available after the first page of results.
listPhoneNumbersOptedOut
    :: ListPhoneNumbersOptedOut
listPhoneNumbersOptedOut = ListPhoneNumbersOptedOut' {_lpnooNextToken = Nothing}


-- | A @NextToken@ string is used when you call the @ListPhoneNumbersOptedOut@ action to retrieve additional records that are available after the first page of results.
lpnooNextToken :: Lens' ListPhoneNumbersOptedOut (Maybe Text)
lpnooNextToken = lens _lpnooNextToken (\ s a -> s{_lpnooNextToken = a})

instance AWSRequest ListPhoneNumbersOptedOut where
        type Rs ListPhoneNumbersOptedOut =
             ListPhoneNumbersOptedOutResponse
        request = postQuery sns
        response
          = receiveXMLWrapper "ListPhoneNumbersOptedOutResult"
              (\ s h x ->
                 ListPhoneNumbersOptedOutResponse' <$>
                   (x .@? "phoneNumbers" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (x .@? "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListPhoneNumbersOptedOut where

instance NFData ListPhoneNumbersOptedOut where

instance ToHeaders ListPhoneNumbersOptedOut where
        toHeaders = const mempty

instance ToPath ListPhoneNumbersOptedOut where
        toPath = const "/"

instance ToQuery ListPhoneNumbersOptedOut where
        toQuery ListPhoneNumbersOptedOut'{..}
          = mconcat
              ["Action" =:
                 ("ListPhoneNumbersOptedOut" :: ByteString),
               "Version" =: ("2010-03-31" :: ByteString),
               "nextToken" =: _lpnooNextToken]

-- | The response from the @ListPhoneNumbersOptedOut@ action.
--
--
--
-- /See:/ 'listPhoneNumbersOptedOutResponse' smart constructor.
data ListPhoneNumbersOptedOutResponse = ListPhoneNumbersOptedOutResponse'
  { _lpnoorsPhoneNumbers   :: !(Maybe [Text])
  , _lpnoorsNextToken      :: !(Maybe Text)
  , _lpnoorsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListPhoneNumbersOptedOutResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpnoorsPhoneNumbers' - A list of phone numbers that are opted out of receiving SMS messages. The list is paginated, and each page can contain up to 100 phone numbers.
--
-- * 'lpnoorsNextToken' - A @NextToken@ string is returned when you call the @ListPhoneNumbersOptedOut@ action if additional records are available after the first page of results.
--
-- * 'lpnoorsResponseStatus' - -- | The response status code.
listPhoneNumbersOptedOutResponse
    :: Int -- ^ 'lpnoorsResponseStatus'
    -> ListPhoneNumbersOptedOutResponse
listPhoneNumbersOptedOutResponse pResponseStatus_ =
  ListPhoneNumbersOptedOutResponse'
    { _lpnoorsPhoneNumbers = Nothing
    , _lpnoorsNextToken = Nothing
    , _lpnoorsResponseStatus = pResponseStatus_
    }


-- | A list of phone numbers that are opted out of receiving SMS messages. The list is paginated, and each page can contain up to 100 phone numbers.
lpnoorsPhoneNumbers :: Lens' ListPhoneNumbersOptedOutResponse [Text]
lpnoorsPhoneNumbers = lens _lpnoorsPhoneNumbers (\ s a -> s{_lpnoorsPhoneNumbers = a}) . _Default . _Coerce

-- | A @NextToken@ string is returned when you call the @ListPhoneNumbersOptedOut@ action if additional records are available after the first page of results.
lpnoorsNextToken :: Lens' ListPhoneNumbersOptedOutResponse (Maybe Text)
lpnoorsNextToken = lens _lpnoorsNextToken (\ s a -> s{_lpnoorsNextToken = a})

-- | -- | The response status code.
lpnoorsResponseStatus :: Lens' ListPhoneNumbersOptedOutResponse Int
lpnoorsResponseStatus = lens _lpnoorsResponseStatus (\ s a -> s{_lpnoorsResponseStatus = a})

instance NFData ListPhoneNumbersOptedOutResponse
         where
