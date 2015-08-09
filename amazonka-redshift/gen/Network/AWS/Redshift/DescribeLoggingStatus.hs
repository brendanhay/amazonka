{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DescribeLoggingStatus
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes whether information, such as queries and connection attempts,
-- is being logged for the specified Amazon Redshift cluster.
--
-- /See:/ <http://docs.aws.amazon.com/redshift/latest/APIReference/API_DescribeLoggingStatus.html AWS API Reference> for DescribeLoggingStatus.
module Network.AWS.Redshift.DescribeLoggingStatus
    (
    -- * Creating a Request
      DescribeLoggingStatus
    , describeLoggingStatus
    -- * Request Lenses
    , dlsClusterIdentifier

    -- * Destructuring the Response
    , LoggingStatus
    , loggingStatus
    -- * Response Lenses
    , lsLastSuccessfulDeliveryTime
    , lsLastFailureTime
    , lsS3KeyPrefix
    , lsBucketName
    , lsLoggingEnabled
    , lsLastFailureMessage
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Redshift.Types
import           Network.AWS.Redshift.Types.Product
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'describeLoggingStatus' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlsClusterIdentifier'
newtype DescribeLoggingStatus = DescribeLoggingStatus'
    { _dlsClusterIdentifier :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeLoggingStatus' smart constructor.
describeLoggingStatus :: Text -> DescribeLoggingStatus
describeLoggingStatus pClusterIdentifier_ =
    DescribeLoggingStatus'
    { _dlsClusterIdentifier = pClusterIdentifier_
    }

-- | The identifier of the cluster to get the logging status from.
--
-- Example: @examplecluster@
dlsClusterIdentifier :: Lens' DescribeLoggingStatus Text
dlsClusterIdentifier = lens _dlsClusterIdentifier (\ s a -> s{_dlsClusterIdentifier = a});

instance AWSRequest DescribeLoggingStatus where
        type Sv DescribeLoggingStatus = Redshift
        type Rs DescribeLoggingStatus = LoggingStatus
        request = postQuery
        response
          = receiveXMLWrapper "DescribeLoggingStatusResult"
              (\ s h x -> parseXML x)

instance ToHeaders DescribeLoggingStatus where
        toHeaders = const mempty

instance ToPath DescribeLoggingStatus where
        toPath = const "/"

instance ToQuery DescribeLoggingStatus where
        toQuery DescribeLoggingStatus'{..}
          = mconcat
              ["Action" =: ("DescribeLoggingStatus" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "ClusterIdentifier" =: _dlsClusterIdentifier]
