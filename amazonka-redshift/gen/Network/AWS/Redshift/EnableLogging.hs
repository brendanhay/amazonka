{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.EnableLogging
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Starts logging information, such as queries and connection attempts, for the
-- specified Amazon Redshift cluster.
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_EnableLogging.html>
module Network.AWS.Redshift.EnableLogging
    (
    -- * Request
      EnableLogging
    -- ** Request constructor
    , enableLogging
    -- ** Request lenses
    , elBucketName
    , elClusterIdentifier
    , elS3KeyPrefix

    -- * Response
    , EnableLoggingResponse
    -- ** Response constructor
    , enableLoggingResponse
    -- ** Response lenses
    , elrBucketName
    , elrLastFailureMessage
    , elrLastFailureTime
    , elrLastSuccessfulDeliveryTime
    , elrLoggingEnabled
    , elrS3KeyPrefix
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types
import qualified GHC.Exts

data EnableLogging = EnableLogging
    { _elBucketName        :: Text
    , _elClusterIdentifier :: Text
    , _elS3KeyPrefix       :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'EnableLogging' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'elBucketName' @::@ 'Text'
--
-- * 'elClusterIdentifier' @::@ 'Text'
--
-- * 'elS3KeyPrefix' @::@ 'Maybe' 'Text'
--
enableLogging :: Text -- ^ 'elClusterIdentifier'
              -> Text -- ^ 'elBucketName'
              -> EnableLogging
enableLogging p1 p2 = EnableLogging
    { _elClusterIdentifier = p1
    , _elBucketName        = p2
    , _elS3KeyPrefix       = Nothing
    }

-- | The name of an existing S3 bucket where the log files are to be stored.
--
-- Constraints:
--
-- Must be in the same region as the cluster The cluster must have read bucket
-- and put object permissions
elBucketName :: Lens' EnableLogging Text
elBucketName = lens _elBucketName (\s a -> s { _elBucketName = a })

-- | The identifier of the cluster on which logging is to be started.
--
-- Example: 'examplecluster'
--
elClusterIdentifier :: Lens' EnableLogging Text
elClusterIdentifier =
    lens _elClusterIdentifier (\s a -> s { _elClusterIdentifier = a })

-- | The prefix applied to the log file names.
--
-- Constraints:
--
-- Cannot exceed 512 characters Cannot contain spaces( ), double quotes ("),
-- single quotes ('), a backslash (\), or control characters. The hexadecimal
-- codes for invalid characters are:  x00 to x20 x22 x27 x5c x7f or larger
elS3KeyPrefix :: Lens' EnableLogging (Maybe Text)
elS3KeyPrefix = lens _elS3KeyPrefix (\s a -> s { _elS3KeyPrefix = a })

data EnableLoggingResponse = EnableLoggingResponse
    { _elrBucketName                 :: Maybe Text
    , _elrLastFailureMessage         :: Maybe Text
    , _elrLastFailureTime            :: Maybe RFC822
    , _elrLastSuccessfulDeliveryTime :: Maybe RFC822
    , _elrLoggingEnabled             :: Maybe Bool
    , _elrS3KeyPrefix                :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'EnableLoggingResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'elrBucketName' @::@ 'Maybe' 'Text'
--
-- * 'elrLastFailureMessage' @::@ 'Maybe' 'Text'
--
-- * 'elrLastFailureTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'elrLastSuccessfulDeliveryTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'elrLoggingEnabled' @::@ 'Maybe' 'Bool'
--
-- * 'elrS3KeyPrefix' @::@ 'Maybe' 'Text'
--
enableLoggingResponse :: EnableLoggingResponse
enableLoggingResponse = EnableLoggingResponse
    { _elrLoggingEnabled             = Nothing
    , _elrBucketName                 = Nothing
    , _elrS3KeyPrefix                = Nothing
    , _elrLastSuccessfulDeliveryTime = Nothing
    , _elrLastFailureTime            = Nothing
    , _elrLastFailureMessage         = Nothing
    }

-- | The name of the S3 bucket where the log files are stored.
--
elrBucketName :: Lens' EnableLoggingResponse (Maybe Text)
elrBucketName = lens _elrBucketName (\s a -> s { _elrBucketName = a })

-- | The message indicating that logs failed to be delivered.
--
elrLastFailureMessage :: Lens' EnableLoggingResponse (Maybe Text)
elrLastFailureMessage =
    lens _elrLastFailureMessage (\s a -> s { _elrLastFailureMessage = a })

-- | The last time when logs failed to be delivered.
--
elrLastFailureTime :: Lens' EnableLoggingResponse (Maybe UTCTime)
elrLastFailureTime =
    lens _elrLastFailureTime (\s a -> s { _elrLastFailureTime = a })
        . mapping _Time

-- | The last time when logs were delivered.
--
elrLastSuccessfulDeliveryTime :: Lens' EnableLoggingResponse (Maybe UTCTime)
elrLastSuccessfulDeliveryTime =
    lens _elrLastSuccessfulDeliveryTime
        (\s a -> s { _elrLastSuccessfulDeliveryTime = a })
            . mapping _Time

-- | 'true' if logging is on, 'false' if logging is off.
--
elrLoggingEnabled :: Lens' EnableLoggingResponse (Maybe Bool)
elrLoggingEnabled =
    lens _elrLoggingEnabled (\s a -> s { _elrLoggingEnabled = a })

-- | The prefix applied to the log file names.
--
elrS3KeyPrefix :: Lens' EnableLoggingResponse (Maybe Text)
elrS3KeyPrefix = lens _elrS3KeyPrefix (\s a -> s { _elrS3KeyPrefix = a })

instance ToPath EnableLogging where
    toPath = const "/"

instance ToQuery EnableLogging where
    toQuery EnableLogging{..} = mconcat
        [ "BucketName"        =? _elBucketName
        , "ClusterIdentifier" =? _elClusterIdentifier
        , "S3KeyPrefix"       =? _elS3KeyPrefix
        ]

instance ToHeaders EnableLogging

instance AWSRequest EnableLogging where
    type Sv EnableLogging = Redshift
    type Rs EnableLogging = EnableLoggingResponse

    request  = post "EnableLogging"
    response = xmlResponse

instance FromXML EnableLoggingResponse where
    parseXML = withElement "EnableLoggingResult" $ \x -> EnableLoggingResponse
        <$> x .@? "BucketName"
        <*> x .@? "LastFailureMessage"
        <*> x .@? "LastFailureTime"
        <*> x .@? "LastSuccessfulDeliveryTime"
        <*> x .@? "LoggingEnabled"
        <*> x .@? "S3KeyPrefix"
