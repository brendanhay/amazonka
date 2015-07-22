{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.EnableLogging
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Starts logging information, such as queries and connection attempts, for
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
    , elrqS3KeyPrefix
    , elrqClusterIdentifier
    , elrqBucketName

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

import           Network.AWS.Prelude
import           Network.AWS.Redshift.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'enableLogging' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'elrqS3KeyPrefix'
--
-- * 'elrqClusterIdentifier'
--
-- * 'elrqBucketName'
data EnableLogging = EnableLogging'
    { _elrqS3KeyPrefix       :: !(Maybe Text)
    , _elrqClusterIdentifier :: !Text
    , _elrqBucketName        :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'EnableLogging' smart constructor.
enableLogging :: Text -> Text -> EnableLogging
enableLogging pClusterIdentifier pBucketName =
    EnableLogging'
    { _elrqS3KeyPrefix = Nothing
    , _elrqClusterIdentifier = pClusterIdentifier
    , _elrqBucketName = pBucketName
    }

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
elrqS3KeyPrefix :: Lens' EnableLogging (Maybe Text)
elrqS3KeyPrefix = lens _elrqS3KeyPrefix (\ s a -> s{_elrqS3KeyPrefix = a});

-- | The identifier of the cluster on which logging is to be started.
--
-- Example: @examplecluster@
elrqClusterIdentifier :: Lens' EnableLogging Text
elrqClusterIdentifier = lens _elrqClusterIdentifier (\ s a -> s{_elrqClusterIdentifier = a});

-- | The name of an existing S3 bucket where the log files are to be stored.
--
-- Constraints:
--
-- -   Must be in the same region as the cluster
-- -   The cluster must have read bucket and put object permissions
elrqBucketName :: Lens' EnableLogging Text
elrqBucketName = lens _elrqBucketName (\ s a -> s{_elrqBucketName = a});

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
               "S3KeyPrefix" =: _elrqS3KeyPrefix,
               "ClusterIdentifier" =: _elrqClusterIdentifier,
               "BucketName" =: _elrqBucketName]
