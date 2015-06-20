{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CodeDeploy.ListApplications
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

-- | Lists the applications registered with the applicable IAM user or AWS
-- account.
--
-- <http://docs.aws.amazon.com/codedeploy/latest/APIReference/API_ListApplications.html>
module Network.AWS.CodeDeploy.ListApplications
    (
    -- * Request
      ListApplications
    -- ** Request constructor
    , listApplications
    -- ** Request lenses
    , laNextToken

    -- * Response
    , ListApplicationsResponse
    -- ** Response constructor
    , listApplicationsResponse
    -- ** Response lenses
    , larNextToken
    , larApplications
    ) where

import Network.AWS.CodeDeploy.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listApplications' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'laNextToken'
newtype ListApplications = ListApplications'{_laNextToken :: Maybe Text} deriving (Eq, Read, Show)

-- | 'ListApplications' smart constructor.
listApplications :: ListApplications
listApplications = ListApplications'{_laNextToken = Nothing};

-- | An identifier that was returned from the previous list applications
-- call, which can be used to return the next set of applications in the
-- list.
laNextToken :: Lens' ListApplications (Maybe Text)
laNextToken = lens _laNextToken (\ s a -> s{_laNextToken = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest ListApplications where
        type Sv ListApplications = CodeDeploy
        type Rs ListApplications = ListApplicationsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ListApplicationsResponse' <$>
                   (x .?> "nextToken") <*>
                     (x .?> "applications" .!@ mempty))

instance ToHeaders ListApplications where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeDeploy_20141006.ListApplications" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListApplications where
        toJSON ListApplications'{..}
          = object ["nextToken" .= _laNextToken]

instance ToPath ListApplications where
        toPath = const "/"

instance ToQuery ListApplications where
        toQuery = const mempty

-- | /See:/ 'listApplicationsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'larNextToken'
--
-- * 'larApplications'
data ListApplicationsResponse = ListApplicationsResponse'{_larNextToken :: Maybe Text, _larApplications :: Maybe [Text]} deriving (Eq, Read, Show)

-- | 'ListApplicationsResponse' smart constructor.
listApplicationsResponse :: ListApplicationsResponse
listApplicationsResponse = ListApplicationsResponse'{_larNextToken = Nothing, _larApplications = Nothing};

-- | If the amount of information that is returned is significantly large, an
-- identifier will also be returned, which can be used in a subsequent list
-- applications call to return the next set of applications in the list.
larNextToken :: Lens' ListApplicationsResponse (Maybe Text)
larNextToken = lens _larNextToken (\ s a -> s{_larNextToken = a});

-- | A list of application names.
larApplications :: Lens' ListApplicationsResponse [Text]
larApplications = lens _larApplications (\ s a -> s{_larApplications = a}) . _Default;
