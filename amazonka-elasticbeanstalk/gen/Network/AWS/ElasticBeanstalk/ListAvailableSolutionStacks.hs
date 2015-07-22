{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    , lassrsSolutionStacks
    , lassrsSolutionStackDetails
    , lassrsStatus
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
-- * 'lassrsSolutionStacks'
--
-- * 'lassrsSolutionStackDetails'
--
-- * 'lassrsStatus'
data ListAvailableSolutionStacksResponse = ListAvailableSolutionStacksResponse'
    { _lassrsSolutionStacks       :: !(Maybe [Text])
    , _lassrsSolutionStackDetails :: !(Maybe [SolutionStackDescription])
    , _lassrsStatus               :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListAvailableSolutionStacksResponse' smart constructor.
listAvailableSolutionStacksResponse :: Int -> ListAvailableSolutionStacksResponse
listAvailableSolutionStacksResponse pStatus =
    ListAvailableSolutionStacksResponse'
    { _lassrsSolutionStacks = Nothing
    , _lassrsSolutionStackDetails = Nothing
    , _lassrsStatus = pStatus
    }

-- | A list of available solution stacks.
lassrsSolutionStacks :: Lens' ListAvailableSolutionStacksResponse [Text]
lassrsSolutionStacks = lens _lassrsSolutionStacks (\ s a -> s{_lassrsSolutionStacks = a}) . _Default;

-- | A list of available solution stacks and their SolutionStackDescription.
lassrsSolutionStackDetails :: Lens' ListAvailableSolutionStacksResponse [SolutionStackDescription]
lassrsSolutionStackDetails = lens _lassrsSolutionStackDetails (\ s a -> s{_lassrsSolutionStackDetails = a}) . _Default;

-- | FIXME: Undocumented member.
lassrsStatus :: Lens' ListAvailableSolutionStacksResponse Int
lassrsStatus = lens _lassrsStatus (\ s a -> s{_lassrsStatus = a});
