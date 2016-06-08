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
-- Module      : Network.AWS.EC2.DescribeIdFormat
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the ID format settings for your resources on a per-region basis, for example, to view which resource types are enabled for longer IDs. This request only returns information about resource types whose ID formats can be modified; it does not return information about other resource types.
--
-- The following resource types support longer IDs: 'instance' | 'reservation' | 'snapshot' | 'volume'.
--
-- These settings apply to the IAM user who makes the request; they do not apply to the entire AWS account. By default, an IAM user defaults to the same settings as the root user, unless they explicitly override the settings by running the < ModifyIdFormat> command. Resources created with longer IDs are visible to all IAM users, regardless of these settings and provided that they have permission to use the relevant 'Describe' command for the resource type.
module Network.AWS.EC2.DescribeIdFormat
    (
    -- * Creating a Request
      describeIdFormat
    , DescribeIdFormat
    -- * Request Lenses
    , difResource

    -- * Destructuring the Response
    , describeIdFormatResponse
    , DescribeIdFormatResponse
    -- * Response Lenses
    , difrsStatuses
    , difrsResponseStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the parameters for DescribeIdFormat.
--
-- /See:/ 'describeIdFormat' smart constructor.
newtype DescribeIdFormat = DescribeIdFormat'
    { _difResource :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeIdFormat' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'difResource'
describeIdFormat
    :: DescribeIdFormat
describeIdFormat =
    DescribeIdFormat'
    { _difResource = Nothing
    }

-- | The type of resource.
difResource :: Lens' DescribeIdFormat (Maybe Text)
difResource = lens _difResource (\ s a -> s{_difResource = a});

instance AWSRequest DescribeIdFormat where
        type Rs DescribeIdFormat = DescribeIdFormatResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DescribeIdFormatResponse' <$>
                   (x .@? "statusSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance Hashable DescribeIdFormat

instance NFData DescribeIdFormat

instance ToHeaders DescribeIdFormat where
        toHeaders = const mempty

instance ToPath DescribeIdFormat where
        toPath = const "/"

instance ToQuery DescribeIdFormat where
        toQuery DescribeIdFormat'{..}
          = mconcat
              ["Action" =: ("DescribeIdFormat" :: ByteString),
               "Version" =: ("2015-10-01" :: ByteString),
               "Resource" =: _difResource]

-- | Contains the output of DescribeIdFormat.
--
-- /See:/ 'describeIdFormatResponse' smart constructor.
data DescribeIdFormatResponse = DescribeIdFormatResponse'
    { _difrsStatuses       :: !(Maybe [IdFormat])
    , _difrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeIdFormatResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'difrsStatuses'
--
-- * 'difrsResponseStatus'
describeIdFormatResponse
    :: Int -- ^ 'difrsResponseStatus'
    -> DescribeIdFormatResponse
describeIdFormatResponse pResponseStatus_ =
    DescribeIdFormatResponse'
    { _difrsStatuses = Nothing
    , _difrsResponseStatus = pResponseStatus_
    }

-- | Information about the ID format for the resource.
difrsStatuses :: Lens' DescribeIdFormatResponse [IdFormat]
difrsStatuses = lens _difrsStatuses (\ s a -> s{_difrsStatuses = a}) . _Default . _Coerce;

-- | The response status code.
difrsResponseStatus :: Lens' DescribeIdFormatResponse Int
difrsResponseStatus = lens _difrsResponseStatus (\ s a -> s{_difrsResponseStatus = a});

instance NFData DescribeIdFormatResponse
