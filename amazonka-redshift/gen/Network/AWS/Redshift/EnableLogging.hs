{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.Redshift.EnableLogging
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

-- | Starts logging information, such as queries and connection attempts, for
-- the specified Amazon Redshift cluster.
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_EnableLogging.html>
module Network.AWS.Redshift.EnableLogging
    (
    -- * Request
      EnableLogging
    -- ** Request constructor
    , enableLogging
    -- ** Request lenses
    , elS3KeyPrefix
    , elClusterIdentifier
    , elBucketName

    -- * Response
    , LoggingStatus
    -- ** Response constructor
    , loggingStatus
    -- ** Response lenses
    , lsLastSuccessfulDeliveryTime
    , lsLastFailureTime
    , lsS3KeyPrefix
    , lsBucketName
    , lsLoggingEnabled
    , lsLastFailureMessage
    ) where

import Network.AWS.Prelude
import Network.AWS.Redshift.Types
import Network.AWS.Request
import Network.AWS.Response

-- |
--
-- /See:/ 'enableLogging' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'elS3KeyPrefix'
--
-- * 'elClusterIdentifier'
--
-- * 'elBucketName'
data EnableLogging = EnableLogging'{_elS3KeyPrefix :: Maybe Text, _elClusterIdentifier :: Text, _elBucketName :: Text} deriving (Eq, Read, Show)

-- | 'EnableLogging' smart constructor.
enableLogging :: Text -> Text -> EnableLogging
enableLogging pClusterIdentifier pBucketName = EnableLogging'{_elS3KeyPrefix = Nothing, _elClusterIdentifier = pClusterIdentifier, _elBucketName = pBucketName};

-- | The prefix applied to the log file names.
--
-- Constraints:
--
-- -   Cannot exceed 512 characters
-- -   Cannot contain spaces( ), double quotes (\"), single quotes (\'), a
--     backslash (\\), or control characters. The hexadecimal codes for
--     invalid characters are:
--     -   x00 to x20
--     -   x22
--     -   x27
--     -   x5c
--     -   x7f or larger
elS3KeyPrefix :: Lens' EnableLogging (Maybe Text)
elS3KeyPrefix = lens _elS3KeyPrefix (\ s a -> s{_elS3KeyPrefix = a});

-- | The identifier of the cluster on which logging is to be started.
--
-- Example: @examplecluster@
elClusterIdentifier :: Lens' EnableLogging Text
elClusterIdentifier = lens _elClusterIdentifier (\ s a -> s{_elClusterIdentifier = a});

-- | The name of an existing S3 bucket where the log files are to be stored.
--
-- Constraints:
--
-- -   Must be in the same region as the cluster
-- -   The cluster must have read bucket and put object permissions
elBucketName :: Lens' EnableLogging Text
elBucketName = lens _elBucketName (\ s a -> s{_elBucketName = a});

instance AWSRequest EnableLogging where
        type Sv EnableLogging = Redshift
        type Rs EnableLogging = LoggingStatus
        request = post
        response
          = receiveXMLWrapper "EnableLoggingResult"
              (\ s h x -> parseXML x)

instance ToHeaders EnableLogging where
        toHeaders = const mempty

instance ToPath EnableLogging where
        toPath = const "/"

instance ToQuery EnableLogging where
        toQuery EnableLogging'{..}
          = mconcat
              ["Action" =: ("EnableLogging" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "S3KeyPrefix" =: _elS3KeyPrefix,
               "ClusterIdentifier" =: _elClusterIdentifier,
               "BucketName" =: _elBucketName]
