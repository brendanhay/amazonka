{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.ElasticBeanstalk.ListAvailableSolutionStacks
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

-- | Returns a list of the available solution stack names.
--
-- <http://docs.aws.amazon.com/elasticbeanstalk/latest/api/API_ListAvailableSolutionStacks.html>
module Network.AWS.ElasticBeanstalk.ListAvailableSolutionStacks
    (
    -- * Request
      ListAvailableSolutionStacks
    -- ** Request constructor
    , listAvailableSolutionStacks

    -- * Response
    , ListAvailableSolutionStacksResponse
    -- ** Response constructor
    , listAvailableSolutionStacksResponse
    -- ** Response lenses
    , lassrSolutionStacks
    , lassrSolutionStackDetails
    ) where

import Network.AWS.ElasticBeanstalk.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listAvailableSolutionStacks' smart constructor.
data ListAvailableSolutionStacks = ListAvailableSolutionStacks' deriving (Eq, Read, Show)

-- | 'ListAvailableSolutionStacks' smart constructor.
listAvailableSolutionStacks :: ListAvailableSolutionStacks
listAvailableSolutionStacks = ListAvailableSolutionStacks';

instance AWSRequest ListAvailableSolutionStacks where
        type Sv ListAvailableSolutionStacks =
             ElasticBeanstalk
        type Rs ListAvailableSolutionStacks =
             ListAvailableSolutionStacksResponse
        request = post
        response
          = receiveXMLWrapper
              "ListAvailableSolutionStacksResult"
              (\ s h x ->
                 ListAvailableSolutionStacksResponse' <$>
                   (x .@? "SolutionStacks" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*>
                     (x .@? "SolutionStackDetails" .!@ mempty >>=
                        may (parseXMLList "member")))

instance ToHeaders ListAvailableSolutionStacks where
        toHeaders = const mempty

instance ToPath ListAvailableSolutionStacks where
        toPath = const "/"

instance ToQuery ListAvailableSolutionStacks where
        toQuery
          = const
              (mconcat
                 ["Action" =:
                    ("ListAvailableSolutionStacks" :: ByteString),
                  "Version" =: ("2010-12-01" :: ByteString)])

-- | /See:/ 'listAvailableSolutionStacksResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lassrSolutionStacks'
--
-- * 'lassrSolutionStackDetails'
data ListAvailableSolutionStacksResponse = ListAvailableSolutionStacksResponse'{_lassrSolutionStacks :: Maybe [Text], _lassrSolutionStackDetails :: Maybe [SolutionStackDescription]} deriving (Eq, Read, Show)

-- | 'ListAvailableSolutionStacksResponse' smart constructor.
listAvailableSolutionStacksResponse :: ListAvailableSolutionStacksResponse
listAvailableSolutionStacksResponse = ListAvailableSolutionStacksResponse'{_lassrSolutionStacks = Nothing, _lassrSolutionStackDetails = Nothing};

-- | A list of available solution stacks.
lassrSolutionStacks :: Lens' ListAvailableSolutionStacksResponse [Text]
lassrSolutionStacks = lens _lassrSolutionStacks (\ s a -> s{_lassrSolutionStacks = a}) . _Default;

-- | A list of available solution stacks and their SolutionStackDescription.
lassrSolutionStackDetails :: Lens' ListAvailableSolutionStacksResponse [SolutionStackDescription]
lassrSolutionStackDetails = lens _lassrSolutionStackDetails (\ s a -> s{_lassrSolutionStackDetails = a}) . _Default;
