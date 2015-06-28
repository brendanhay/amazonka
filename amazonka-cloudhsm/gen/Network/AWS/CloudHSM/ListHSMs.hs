{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CloudHSM.ListHSMs
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Retrieves the identifiers of all of the HSMs provisioned for the current
-- customer.
--
-- This operation supports pagination with the use of the /NextToken/
-- member. If more results are available, the /NextToken/ member of the
-- response contains a token that you pass in the next call to ListHsms to
-- retrieve the next set of items.
--
-- <http://docs.aws.amazon.com/cloudhsm/latest/dg/API_ListHSMs.html>
module Network.AWS.CloudHSM.ListHSMs
    (
    -- * Request
      ListHSMs
    -- ** Request constructor
    , listHSMs
    -- ** Request lenses
    , lNextToken

    -- * Response
    , ListHSMsResponse
    -- ** Response constructor
    , listHSMsResponse
    -- ** Response lenses
    , lisNextToken
    , lisHSMList
    , lisStatus
    ) where

import           Network.AWS.CloudHSM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listHSMs' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lNextToken'
newtype ListHSMs = ListHSMs'
    { _lNextToken :: Maybe Text
    } deriving (Eq,Read,Show)

-- | 'ListHSMs' smart constructor.
listHSMs :: ListHSMs
listHSMs =
    ListHSMs'
    { _lNextToken = Nothing
    }

-- | The /NextToken/ value from a previous call to ListHsms. Pass null if
-- this is the first call.
lNextToken :: Lens' ListHSMs (Maybe Text)
lNextToken = lens _lNextToken (\ s a -> s{_lNextToken = a});

instance AWSRequest ListHSMs where
        type Sv ListHSMs = CloudHSM
        type Rs ListHSMs = ListHSMsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ListHSMsResponse' <$>
                   (x .?> "NextToken") <*> (x .?> "HsmList" .!@ mempty)
                     <*> (pure s))

instance ToHeaders ListHSMs where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CloudHsmFrontendService.ListHSMs" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListHSMs where
        toJSON ListHSMs'{..}
          = object ["NextToken" .= _lNextToken]

instance ToPath ListHSMs where
        toPath = const "/"

instance ToQuery ListHSMs where
        toQuery = const mempty

-- | Contains the output of the ListHsms action.
--
-- /See:/ 'listHSMsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lisNextToken'
--
-- * 'lisHSMList'
--
-- * 'lisStatus'
data ListHSMsResponse = ListHSMsResponse'
    { _lisNextToken :: !(Maybe Text)
    , _lisHSMList   :: !(Maybe [Text])
    , _lisStatus    :: !Status
    } deriving (Eq,Show)

-- | 'ListHSMsResponse' smart constructor.
listHSMsResponse :: Status -> ListHSMsResponse
listHSMsResponse pStatus =
    ListHSMsResponse'
    { _lisNextToken = Nothing
    , _lisHSMList = Nothing
    , _lisStatus = pStatus
    }

-- | If not null, more results are available. Pass this value to ListHsms to
-- retrieve the next set of items.
lisNextToken :: Lens' ListHSMsResponse (Maybe Text)
lisNextToken = lens _lisNextToken (\ s a -> s{_lisNextToken = a});

-- | The list of ARNs that identify the HSMs.
lisHSMList :: Lens' ListHSMsResponse [Text]
lisHSMList = lens _lisHSMList (\ s a -> s{_lisHSMList = a}) . _Default;

-- | FIXME: Undocumented member.
lisStatus :: Lens' ListHSMsResponse Status
lisStatus = lens _lisStatus (\ s a -> s{_lisStatus = a});
