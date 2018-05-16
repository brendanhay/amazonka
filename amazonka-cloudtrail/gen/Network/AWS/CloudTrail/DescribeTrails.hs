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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves settings for the trail associated with the current region for your account.
--
--
module Network.AWS.CloudTrail.DescribeTrails
    (
    -- * Creating a Request
      describeTrails
    , DescribeTrails
    -- * Request Lenses
    , dtIncludeShadowTrails
    , dtTrailNameList

    -- * Destructuring the Response
    , describeTrailsResponse
    , DescribeTrailsResponse
    -- * Response Lenses
    , dtrsTrailList
    , dtrsResponseStatus
    ) where

import Network.AWS.CloudTrail.Types
import Network.AWS.CloudTrail.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Returns information about the trail.
--
--
--
-- /See:/ 'describeTrails' smart constructor.
data DescribeTrails = DescribeTrails'
  { _dtIncludeShadowTrails :: !(Maybe Bool)
  , _dtTrailNameList       :: !(Maybe [Text])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeTrails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtIncludeShadowTrails' - Specifies whether to include shadow trails in the response. A shadow trail is the replication in a region of a trail that was created in a different region. The default is true.
--
-- * 'dtTrailNameList' - Specifies a list of trail names, trail ARNs, or both, of the trails to describe. The format of a trail ARN is: @arn:aws:cloudtrail:us-east-1:123456789012:trail/MyTrail@  If an empty list is specified, information for the trail in the current region is returned.     * If an empty list is specified and @IncludeShadowTrails@ is false, then information for all trails in the current region is returned.     * If an empty list is specified and IncludeShadowTrails is null or true, then information for all trails in the current region and any associated shadow trails in other regions is returned.
describeTrails
    :: DescribeTrails
describeTrails =
  DescribeTrails' {_dtIncludeShadowTrails = Nothing, _dtTrailNameList = Nothing}


-- | Specifies whether to include shadow trails in the response. A shadow trail is the replication in a region of a trail that was created in a different region. The default is true.
dtIncludeShadowTrails :: Lens' DescribeTrails (Maybe Bool)
dtIncludeShadowTrails = lens _dtIncludeShadowTrails (\ s a -> s{_dtIncludeShadowTrails = a})

-- | Specifies a list of trail names, trail ARNs, or both, of the trails to describe. The format of a trail ARN is: @arn:aws:cloudtrail:us-east-1:123456789012:trail/MyTrail@  If an empty list is specified, information for the trail in the current region is returned.     * If an empty list is specified and @IncludeShadowTrails@ is false, then information for all trails in the current region is returned.     * If an empty list is specified and IncludeShadowTrails is null or true, then information for all trails in the current region and any associated shadow trails in other regions is returned.
dtTrailNameList :: Lens' DescribeTrails [Text]
dtTrailNameList = lens _dtTrailNameList (\ s a -> s{_dtTrailNameList = a}) . _Default . _Coerce

instance AWSRequest DescribeTrails where
        type Rs DescribeTrails = DescribeTrailsResponse
        request = postJSON cloudTrail
        response
          = receiveJSON
              (\ s h x ->
                 DescribeTrailsResponse' <$>
                   (x .?> "trailList" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable DescribeTrails where

instance NFData DescribeTrails where

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
                 [("includeShadowTrails" .=) <$>
                    _dtIncludeShadowTrails,
                  ("trailNameList" .=) <$> _dtTrailNameList])

instance ToPath DescribeTrails where
        toPath = const "/"

instance ToQuery DescribeTrails where
        toQuery = const mempty

-- | Returns the objects or data listed below if successful. Otherwise, returns an error.
--
--
--
-- /See:/ 'describeTrailsResponse' smart constructor.
data DescribeTrailsResponse = DescribeTrailsResponse'
  { _dtrsTrailList      :: !(Maybe [Trail])
  , _dtrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeTrailsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtrsTrailList' - The list of trail objects.
--
-- * 'dtrsResponseStatus' - -- | The response status code.
describeTrailsResponse
    :: Int -- ^ 'dtrsResponseStatus'
    -> DescribeTrailsResponse
describeTrailsResponse pResponseStatus_ =
  DescribeTrailsResponse'
    {_dtrsTrailList = Nothing, _dtrsResponseStatus = pResponseStatus_}


-- | The list of trail objects.
dtrsTrailList :: Lens' DescribeTrailsResponse [Trail]
dtrsTrailList = lens _dtrsTrailList (\ s a -> s{_dtrsTrailList = a}) . _Default . _Coerce

-- | -- | The response status code.
dtrsResponseStatus :: Lens' DescribeTrailsResponse Int
dtrsResponseStatus = lens _dtrsResponseStatus (\ s a -> s{_dtrsResponseStatus = a})

instance NFData DescribeTrailsResponse where
