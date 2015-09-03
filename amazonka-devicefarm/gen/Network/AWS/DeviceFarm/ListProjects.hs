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
-- Module      : Network.AWS.DeviceFarm.ListProjects
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about projects.
--
-- /See:/ <http://docs.aws.amazon.com/devicefarm/latest/APIReference/API_ListProjects.html AWS API Reference> for ListProjects.
module Network.AWS.DeviceFarm.ListProjects
    (
    -- * Creating a Request
      listProjects
    , ListProjects
    -- * Request Lenses
    , lpArn
    , lpNextToken

    -- * Destructuring the Response
    , listProjectsResponse
    , ListProjectsResponse
    -- * Response Lenses
    , lprsNextToken
    , lprsProjects
    , lprsResponseStatus
    ) where

import           Network.AWS.DeviceFarm.Types
import           Network.AWS.DeviceFarm.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents a request to the list projects operation.
--
-- /See:/ 'listProjects' smart constructor.
data ListProjects = ListProjects'
    { _lpArn       :: !(Maybe Text)
    , _lpNextToken :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListProjects' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpArn'
--
-- * 'lpNextToken'
listProjects
    :: ListProjects
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
        type Rs ListProjects = ListProjectsResponse
        request = postJSON deviceFarm
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
              (catMaybes
                 [("arn" .=) <$> _lpArn,
                  ("nextToken" .=) <$> _lpNextToken])

instance ToPath ListProjects where
        toPath = const "/"

instance ToQuery ListProjects where
        toQuery = const mempty

-- | Represents the result of a list projects request.
--
-- /See:/ 'listProjectsResponse' smart constructor.
data ListProjectsResponse = ListProjectsResponse'
    { _lprsNextToken      :: !(Maybe Text)
    , _lprsProjects       :: !(Maybe [Project])
    , _lprsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListProjectsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lprsNextToken'
--
-- * 'lprsProjects'
--
-- * 'lprsResponseStatus'
listProjectsResponse
    :: Int -- ^ 'lprsResponseStatus'
    -> ListProjectsResponse
listProjectsResponse pResponseStatus_ =
    ListProjectsResponse'
    { _lprsNextToken = Nothing
    , _lprsProjects = Nothing
    , _lprsResponseStatus = pResponseStatus_
    }

-- | If the number of items that are returned is significantly large, this is
-- an identifier that is also returned, which can be used in a subsequent
-- call to this operation to return the next set of items in the list.
lprsNextToken :: Lens' ListProjectsResponse (Maybe Text)
lprsNextToken = lens _lprsNextToken (\ s a -> s{_lprsNextToken = a});

-- | Information about the projects.
lprsProjects :: Lens' ListProjectsResponse [Project]
lprsProjects = lens _lprsProjects (\ s a -> s{_lprsProjects = a}) . _Default . _Coerce;

-- | The response status code.
lprsResponseStatus :: Lens' ListProjectsResponse Int
lprsResponseStatus = lens _lprsResponseStatus (\ s a -> s{_lprsResponseStatus = a});
