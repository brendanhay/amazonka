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
-- Module      : Network.AWS.CloudTrail.DescribeTrails
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves settings for the trail associated with the current region for
-- your account.
--
-- /See:/ <http://docs.aws.amazon.com/awscloudtrail/latest/APIReference/API_DescribeTrails.html AWS API Reference> for DescribeTrails.
module Network.AWS.CloudTrail.DescribeTrails
    (
    -- * Creating a Request
      describeTrails
    , DescribeTrails
    -- * Request Lenses
    , dtTrailNameList

    -- * Destructuring the Response
    , describeTrailsResponse
    , DescribeTrailsResponse
    -- * Response Lenses
    , dtrsTrailList
    , dtrsStatus
    ) where

import           Network.AWS.CloudTrail.Types
import           Network.AWS.CloudTrail.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Returns information about the trail.
--
-- /See:/ 'describeTrails' smart constructor.
newtype DescribeTrails = DescribeTrails'
    { _dtTrailNameList :: Maybe [Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeTrails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtTrailNameList'
describeTrails
    :: DescribeTrails
describeTrails =
    DescribeTrails'
    { _dtTrailNameList = Nothing
    }

-- | The trail returned.
dtTrailNameList :: Lens' DescribeTrails [Text]
dtTrailNameList = lens _dtTrailNameList (\ s a -> s{_dtTrailNameList = a}) . _Default . _Coerce;

instance AWSRequest DescribeTrails where
        type Rs DescribeTrails = DescribeTrailsResponse
        request = postJSON cloudTrail
        response
          = receiveJSON
              (\ s h x ->
                 DescribeTrailsResponse' <$>
                   (x .?> "trailList" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance ToHeaders DescribeTrails where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.DescribeTrails"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeTrails where
        toJSON DescribeTrails'{..}
          = object
              (catMaybes
                 [("trailNameList" .=) <$> _dtTrailNameList])

instance ToPath DescribeTrails where
        toPath = const "/"

instance ToQuery DescribeTrails where
        toQuery = const mempty

-- | Returns the objects or data listed below if successful. Otherwise,
-- returns an error.
--
-- /See:/ 'describeTrailsResponse' smart constructor.
data DescribeTrailsResponse = DescribeTrailsResponse'
    { _dtrsTrailList :: !(Maybe [Trail])
    , _dtrsStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeTrailsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtrsTrailList'
--
-- * 'dtrsStatus'
describeTrailsResponse
    :: Int -- ^ 'dtrsStatus'
    -> DescribeTrailsResponse
describeTrailsResponse pStatus_ =
    DescribeTrailsResponse'
    { _dtrsTrailList = Nothing
    , _dtrsStatus = pStatus_
    }

-- | The list of trails.
dtrsTrailList :: Lens' DescribeTrailsResponse [Trail]
dtrsTrailList = lens _dtrsTrailList (\ s a -> s{_dtrsTrailList = a}) . _Default . _Coerce;

-- | The response status code.
dtrsStatus :: Lens' DescribeTrailsResponse Int
dtrsStatus = lens _dtrsStatus (\ s a -> s{_dtrsStatus = a});
