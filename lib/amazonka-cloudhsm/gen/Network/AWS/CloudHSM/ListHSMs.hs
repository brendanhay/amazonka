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
-- Module      : Network.AWS.CloudHSM.ListHSMs
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is documentation for __AWS CloudHSM Classic__ . For more information, see <http://aws.amazon.com/cloudhsm/faqs-classic/ AWS CloudHSM Classic FAQs> , the <http://docs.aws.amazon.com/cloudhsm/classic/userguide/ AWS CloudHSM Classic User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/classic/APIReference/ AWS CloudHSM Classic API Reference> .
--
--
-- __For information about the current version of AWS CloudHSM__ , see <http://aws.amazon.com/cloudhsm/ AWS CloudHSM> , the <http://docs.aws.amazon.com/cloudhsm/latest/userguide/ AWS CloudHSM User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/latest/APIReference/ AWS CloudHSM API Reference> .
--
-- Retrieves the identifiers of all of the HSMs provisioned for the current customer.
--
-- This operation supports pagination with the use of the @NextToken@ member. If more results are available, the @NextToken@ member of the response contains a token that you pass in the next call to @ListHsms@ to retrieve the next set of items.
--
module Network.AWS.CloudHSM.ListHSMs
    (
    -- * Creating a Request
      listHSMs
    , ListHSMs
    -- * Request Lenses
    , lhsmNextToken

    -- * Destructuring the Response
    , listHSMsResponse
    , ListHSMsResponse
    -- * Response Lenses
    , lhsmrsNextToken
    , lhsmrsHSMList
    , lhsmrsResponseStatus
    ) where

import Network.AWS.CloudHSM.Types
import Network.AWS.CloudHSM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listHSMs' smart constructor.
newtype ListHSMs = ListHSMs'
  { _lhsmNextToken :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListHSMs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lhsmNextToken' - The @NextToken@ value from a previous call to @ListHsms@ . Pass null if this is the first call.
listHSMs
    :: ListHSMs
listHSMs = ListHSMs' {_lhsmNextToken = Nothing}


-- | The @NextToken@ value from a previous call to @ListHsms@ . Pass null if this is the first call.
lhsmNextToken :: Lens' ListHSMs (Maybe Text)
lhsmNextToken = lens _lhsmNextToken (\ s a -> s{_lhsmNextToken = a})

instance AWSRequest ListHSMs where
        type Rs ListHSMs = ListHSMsResponse
        request = postJSON cloudHSM
        response
          = receiveJSON
              (\ s h x ->
                 ListHSMsResponse' <$>
                   (x .?> "NextToken") <*> (x .?> "HsmList" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListHSMs where

instance NFData ListHSMs where

instance ToHeaders ListHSMs where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CloudHsmFrontendService.ListHsms" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListHSMs where
        toJSON ListHSMs'{..}
          = object
              (catMaybes [("NextToken" .=) <$> _lhsmNextToken])

instance ToPath ListHSMs where
        toPath = const "/"

instance ToQuery ListHSMs where
        toQuery = const mempty

-- | Contains the output of the @ListHsms@ operation.
--
--
--
-- /See:/ 'listHSMsResponse' smart constructor.
data ListHSMsResponse = ListHSMsResponse'
  { _lhsmrsNextToken      :: !(Maybe Text)
  , _lhsmrsHSMList        :: !(Maybe [Text])
  , _lhsmrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListHSMsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lhsmrsNextToken' - If not null, more results are available. Pass this value to @ListHsms@ to retrieve the next set of items.
--
-- * 'lhsmrsHSMList' - The list of ARNs that identify the HSMs.
--
-- * 'lhsmrsResponseStatus' - -- | The response status code.
listHSMsResponse
    :: Int -- ^ 'lhsmrsResponseStatus'
    -> ListHSMsResponse
listHSMsResponse pResponseStatus_ =
  ListHSMsResponse'
    { _lhsmrsNextToken = Nothing
    , _lhsmrsHSMList = Nothing
    , _lhsmrsResponseStatus = pResponseStatus_
    }


-- | If not null, more results are available. Pass this value to @ListHsms@ to retrieve the next set of items.
lhsmrsNextToken :: Lens' ListHSMsResponse (Maybe Text)
lhsmrsNextToken = lens _lhsmrsNextToken (\ s a -> s{_lhsmrsNextToken = a})

-- | The list of ARNs that identify the HSMs.
lhsmrsHSMList :: Lens' ListHSMsResponse [Text]
lhsmrsHSMList = lens _lhsmrsHSMList (\ s a -> s{_lhsmrsHSMList = a}) . _Default . _Coerce

-- | -- | The response status code.
lhsmrsResponseStatus :: Lens' ListHSMsResponse Int
lhsmrsResponseStatus = lens _lhsmrsResponseStatus (\ s a -> s{_lhsmrsResponseStatus = a})

instance NFData ListHSMsResponse where
