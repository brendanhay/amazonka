{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudHSM.ListHapgs
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

-- | Lists the high-availability partition groups for the account.
--
-- This operation supports pagination with the use of the /NextToken/
-- member. If more results are available, the /NextToken/ member of the
-- response contains a token that you pass in the next call to ListHapgs to
-- retrieve the next set of items.
--
-- <http://docs.aws.amazon.com/cloudhsm/latest/dg/API_ListHapgs.html>
module Network.AWS.CloudHSM.ListHapgs
    (
    -- * Request
      ListHapgs
    -- ** Request constructor
    , listHapgs
    -- ** Request lenses
    , lhNextToken

    -- * Response
    , ListHapgsResponse
    -- ** Response constructor
    , listHapgsResponse
    -- ** Response lenses
    , lhrNextToken
    , lhrHapgList
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.CloudHSM.Types

-- | /See:/ 'listHapgs' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lhNextToken'
newtype ListHapgs = ListHapgs'{_lhNextToken :: Maybe Text} deriving (Eq, Read, Show)

-- | 'ListHapgs' smart constructor.
listHapgs :: ListHapgs
listHapgs = ListHapgs'{_lhNextToken = Nothing};

-- | The /NextToken/ value from a previous call to ListHapgs. Pass null if
-- this is the first call.
lhNextToken :: Lens' ListHapgs (Maybe Text)
lhNextToken = lens _lhNextToken (\ s a -> s{_lhNextToken = a});

instance AWSRequest ListHapgs where
        type Sv ListHapgs = CloudHSM
        type Rs ListHapgs = ListHapgsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ListHapgsResponse' <$>
                   x .?> "NextToken" <*> x .?> "HapgList" .!@ mempty)

instance ToHeaders ListHapgs where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CloudHsmFrontendService.ListHapgs" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListHapgs where
        toJSON ListHapgs'{..}
          = object ["NextToken" .= _lhNextToken]

instance ToPath ListHapgs where
        toPath = const "/"

instance ToQuery ListHapgs where
        toQuery = const mempty

-- | /See:/ 'listHapgsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lhrNextToken'
--
-- * 'lhrHapgList'
data ListHapgsResponse = ListHapgsResponse'{_lhrNextToken :: Maybe Text, _lhrHapgList :: [Text]} deriving (Eq, Read, Show)

-- | 'ListHapgsResponse' smart constructor.
listHapgsResponse :: ListHapgsResponse
listHapgsResponse = ListHapgsResponse'{_lhrNextToken = Nothing, _lhrHapgList = mempty};

-- | If not null, more results are available. Pass this value to ListHapgs to
-- retrieve the next set of items.
lhrNextToken :: Lens' ListHapgsResponse (Maybe Text)
lhrNextToken = lens _lhrNextToken (\ s a -> s{_lhrNextToken = a});

-- | The list of high-availability partition groups.
lhrHapgList :: Lens' ListHapgsResponse [Text]
lhrHapgList = lens _lhrHapgList (\ s a -> s{_lhrHapgList = a});
