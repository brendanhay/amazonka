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
-- Module      : Network.AWS.Redshift.EnableLogging
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts logging information, such as queries and connection attempts, for the specified Amazon Redshift cluster.
--
--
module Network.AWS.Redshift.EnableLogging
    (
    -- * Creating a Request
      enableLogging
    , EnableLogging
    -- * Request Lenses
    , elS3KeyPrefix
    , elClusterIdentifier
    , elBucketName

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
-- /See:/ 'enableLogging' smart constructor.
data EnableLogging = EnableLogging'
  { _elS3KeyPrefix       :: !(Maybe Text)
  , _elClusterIdentifier :: !Text
  , _elBucketName        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EnableLogging' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'elS3KeyPrefix' - The prefix applied to the log file names. Constraints:     * Cannot exceed 512 characters     * Cannot contain spaces( ), double quotes ("), single quotes ('), a backslash (\), or control characters. The hexadecimal codes for invalid characters are:      * x00 to x20     * x22     * x27     * x5c     * x7f or larger
--
-- * 'elClusterIdentifier' - The identifier of the cluster on which logging is to be started. Example: @examplecluster@
--
-- * 'elBucketName' - The name of an existing S3 bucket where the log files are to be stored. Constraints:     * Must be in the same region as the cluster     * The cluster must have read bucket and put object permissions
enableLogging
    :: Text -- ^ 'elClusterIdentifier'
    -> Text -- ^ 'elBucketName'
    -> EnableLogging
enableLogging pClusterIdentifier_ pBucketName_ =
  EnableLogging'
    { _elS3KeyPrefix = Nothing
    , _elClusterIdentifier = pClusterIdentifier_
    , _elBucketName = pBucketName_
    }


-- | The prefix applied to the log file names. Constraints:     * Cannot exceed 512 characters     * Cannot contain spaces( ), double quotes ("), single quotes ('), a backslash (\), or control characters. The hexadecimal codes for invalid characters are:      * x00 to x20     * x22     * x27     * x5c     * x7f or larger
elS3KeyPrefix :: Lens' EnableLogging (Maybe Text)
elS3KeyPrefix = lens _elS3KeyPrefix (\ s a -> s{_elS3KeyPrefix = a})

-- | The identifier of the cluster on which logging is to be started. Example: @examplecluster@
elClusterIdentifier :: Lens' EnableLogging Text
elClusterIdentifier = lens _elClusterIdentifier (\ s a -> s{_elClusterIdentifier = a})

-- | The name of an existing S3 bucket where the log files are to be stored. Constraints:     * Must be in the same region as the cluster     * The cluster must have read bucket and put object permissions
elBucketName :: Lens' EnableLogging Text
elBucketName = lens _elBucketName (\ s a -> s{_elBucketName = a})

instance AWSRequest EnableLogging where
        type Rs EnableLogging = LoggingStatus
        request = postQuery redshift
        response
          = receiveXMLWrapper "EnableLoggingResult"
              (\ s h x -> parseXML x)

instance Hashable EnableLogging where

instance NFData EnableLogging where

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
