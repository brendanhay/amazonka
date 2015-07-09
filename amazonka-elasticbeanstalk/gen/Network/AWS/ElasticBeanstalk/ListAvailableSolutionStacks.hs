{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.ListAvailableSolutionStacks
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the available solution stack names.
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
    , lassrStatus
    ) where

import           Network.AWS.ElasticBeanstalk.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listAvailableSolutionStacks' smart constructor.
data ListAvailableSolutionStacks =
    ListAvailableSolutionStacks'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListAvailableSolutionStacks' smart constructor.
listAvailableSolutionStacks :: ListAvailableSolutionStacks
listAvailableSolutionStacks = ListAvailableSolutionStacks'

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
                        may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

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

-- | A list of available AWS Elastic Beanstalk solution stacks.
--
-- /See:/ 'listAvailableSolutionStacksResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lassrSolutionStacks'
--
-- * 'lassrSolutionStackDetails'
--
-- * 'lassrStatus'
data ListAvailableSolutionStacksResponse = ListAvailableSolutionStacksResponse'
    { _lassrSolutionStacks       :: !(Maybe [Text])
    , _lassrSolutionStackDetails :: !(Maybe [SolutionStackDescription])
    , _lassrStatus               :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListAvailableSolutionStacksResponse' smart constructor.
listAvailableSolutionStacksResponse :: Int -> ListAvailableSolutionStacksResponse
listAvailableSolutionStacksResponse pStatus =
    ListAvailableSolutionStacksResponse'
    { _lassrSolutionStacks = Nothing
    , _lassrSolutionStackDetails = Nothing
    , _lassrStatus = pStatus
    }

-- | A list of available solution stacks.
lassrSolutionStacks :: Lens' ListAvailableSolutionStacksResponse [Text]
lassrSolutionStacks = lens _lassrSolutionStacks (\ s a -> s{_lassrSolutionStacks = a}) . _Default;

-- | A list of available solution stacks and their SolutionStackDescription.
lassrSolutionStackDetails :: Lens' ListAvailableSolutionStacksResponse [SolutionStackDescription]
lassrSolutionStackDetails = lens _lassrSolutionStackDetails (\ s a -> s{_lassrSolutionStackDetails = a}) . _Default;

-- | FIXME: Undocumented member.
lassrStatus :: Lens' ListAvailableSolutionStacksResponse Int
lassrStatus = lens _lassrStatus (\ s a -> s{_lassrStatus = a});
