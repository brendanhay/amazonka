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
-- Module      : Network.AWS.Redshift.DescribeLoggingStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes whether information, such as queries and connection attempts, is being logged for the specified Amazon Redshift cluster.
--
--
module Network.AWS.Redshift.DescribeLoggingStatus
    (
    -- * Creating a Request
      describeLoggingStatus
    , DescribeLoggingStatus
    -- * Request Lenses
    , dlsClusterIdentifier

    -- * Destructuring the Response
    , loggingStatus
    , LoggingStatus
    -- * Response Lenses
    , lsLastFailureTime
    , lsLastSuccessfulDeliveryTime
    , lsS3KeyPrefix
    , lsBucketName
    , lsLoggingEnabled
    , lsLastFailureMessage
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Types
import Network.AWS.Redshift.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'describeLoggingStatus' smart constructor.
newtype DescribeLoggingStatus = DescribeLoggingStatus'
  { _dlsClusterIdentifier :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeLoggingStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlsClusterIdentifier' - The identifier of the cluster from which to get the logging status. Example: @examplecluster@
describeLoggingStatus
    :: Text -- ^ 'dlsClusterIdentifier'
    -> DescribeLoggingStatus
describeLoggingStatus pClusterIdentifier_ =
  DescribeLoggingStatus' {_dlsClusterIdentifier = pClusterIdentifier_}


-- | The identifier of the cluster from which to get the logging status. Example: @examplecluster@
dlsClusterIdentifier :: Lens' DescribeLoggingStatus Text
dlsClusterIdentifier = lens _dlsClusterIdentifier (\ s a -> s{_dlsClusterIdentifier = a})

instance AWSRequest DescribeLoggingStatus where
        type Rs DescribeLoggingStatus = LoggingStatus
        request = postQuery redshift
        response
          = receiveXMLWrapper "DescribeLoggingStatusResult"
              (\ s h x -> parseXML x)

instance Hashable DescribeLoggingStatus where

instance NFData DescribeLoggingStatus where

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
