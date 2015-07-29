{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.ListProjects
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Gets information about projects.
--
-- <http://docs.aws.amazon.com/devicefarm/latest/APIReference/API_ListProjects.html>
module Network.AWS.DeviceFarm.ListProjects
    (
    -- * Request
      ListProjects
    -- ** Request constructor
    , listProjects
    -- ** Request lenses
    , lpArn
    , lpNextToken

    -- * Response
    , ListProjectsResponse
    -- ** Response constructor
    , listProjectsResponse
    -- ** Response lenses
    , lprsNextToken
    , lprsProjects
    , lprsStatus
    ) where

import           Network.AWS.DeviceFarm.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents a request to the list projects operation.
--
-- /See:/ 'listProjects' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lpArn'
--
-- * 'lpNextToken'
data ListProjects = ListProjects'
    { _lpArn       :: !(Maybe Text)
    , _lpNextToken :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListProjects' smart constructor.
listProjects :: ListProjects
listProjects =
    ListProjects'
    { _lpArn = Nothing
    , _lpNextToken = Nothing
    }

-- | The projects\' ARNs.
lpArn :: Lens' ListProjects (Maybe Text)
lpArn = lens _lpArn (\ s a -> s{_lpArn = a});

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
lpNextToken :: Lens' ListProjects (Maybe Text)
lpNextToken = lens _lpNextToken (\ s a -> s{_lpNextToken = a});

instance AWSRequest ListProjects where
        type Sv ListProjects = DeviceFarm
        type Rs ListProjects = ListProjectsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ListProjectsResponse' <$>
                   (x .?> "nextToken") <*> (x .?> "projects" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance ToHeaders ListProjects where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DeviceFarm_20150623.ListProjects" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListProjects where
        toJSON ListProjects'{..}
          = object
              ["arn" .= _lpArn, "nextToken" .= _lpNextToken]

instance ToPath ListProjects where
        toPath = const mempty

instance ToQuery ListProjects where
        toQuery = const mempty

-- | Represents the result of a list projects request.
--
-- /See:/ 'listProjectsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lprsNextToken'
--
-- * 'lprsProjects'
--
-- * 'lprsStatus'
data ListProjectsResponse = ListProjectsResponse'
    { _lprsNextToken :: !(Maybe Text)
    , _lprsProjects  :: !(Maybe [Project])
    , _lprsStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListProjectsResponse' smart constructor.
listProjectsResponse :: Int -> ListProjectsResponse
listProjectsResponse pStatus_ =
    ListProjectsResponse'
    { _lprsNextToken = Nothing
    , _lprsProjects = Nothing
    , _lprsStatus = pStatus_
    }

-- | If the number of items that are returned is significantly large, this is
-- an identifier that is also returned, which can be used in a subsequent
-- call to this operation to return the next set of items in the list.
lprsNextToken :: Lens' ListProjectsResponse (Maybe Text)
lprsNextToken = lens _lprsNextToken (\ s a -> s{_lprsNextToken = a});

-- | Information about the projects.
lprsProjects :: Lens' ListProjectsResponse [Project]
lprsProjects = lens _lprsProjects (\ s a -> s{_lprsProjects = a}) . _Default . _Coerce;

-- | FIXME: Undocumented member.
lprsStatus :: Lens' ListProjectsResponse Int
lprsStatus = lens _lprsStatus (\ s a -> s{_lprsStatus = a});
