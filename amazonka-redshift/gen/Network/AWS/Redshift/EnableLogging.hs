{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.Redshift.EnableLogging
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Starts logging information, such as queries and connection attempts, for
-- the specified Amazon Redshift cluster.
module Network.AWS.Redshift.EnableLogging
    (
    -- * Request
      EnableLoggingMessage
    -- ** Request constructor
    , enableLoggingMessage
    -- ** Request lenses
    , elmBucketName
    , elmClusterIdentifier
    , elmS3KeyPrefix

    -- * Response
    , LoggingStatus
    -- ** Response constructor
    , loggingStatus
    -- ** Response lenses
    , lsBucketName
    , lsLastFailureMessage
    , lsLastFailureTime
    , lsLastSuccessfulDeliveryTime
    , lsLoggingEnabled
    , lsS3KeyPrefix
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types

data EnableLoggingMessage = EnableLoggingMessage
    { _elmBucketName        :: Text
    , _elmClusterIdentifier :: Text
    , _elmS3KeyPrefix       :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'EnableLoggingMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'elmBucketName' @::@ 'Text'
--
-- * 'elmClusterIdentifier' @::@ 'Text'
--
-- * 'elmS3KeyPrefix' @::@ 'Maybe' 'Text'
--
enableLoggingMessage :: Text -- ^ 'elmClusterIdentifier'
                     -> Text -- ^ 'elmBucketName'
                     -> EnableLoggingMessage
enableLoggingMessage p1 p2 = EnableLoggingMessage
    { _elmClusterIdentifier = p1
    , _elmBucketName        = p2
    , _elmS3KeyPrefix       = Nothing
    }

-- | The name of an existing S3 bucket where the log files are to be stored.
-- Constraints: Must be in the same region as the cluster The cluster must
-- have read bucket and put object permissions.
elmBucketName :: Lens' EnableLoggingMessage Text
elmBucketName = lens _elmBucketName (\s a -> s { _elmBucketName = a })

-- | The identifier of the cluster on which logging is to be started. Example:
-- examplecluster.
elmClusterIdentifier :: Lens' EnableLoggingMessage Text
elmClusterIdentifier =
    lens _elmClusterIdentifier (\s a -> s { _elmClusterIdentifier = a })

-- | The prefix applied to the log file names. Constraints: Cannot exceed 512
-- characters Cannot contain spaces( ), double quotes ("), single quotes
-- ('), a backslash (\), or control characters. The hexadecimal codes for
-- invalid characters are: x00 to x20 x22 x27 x5c x7f or larger.
elmS3KeyPrefix :: Lens' EnableLoggingMessage (Maybe Text)
elmS3KeyPrefix = lens _elmS3KeyPrefix (\s a -> s { _elmS3KeyPrefix = a })

instance ToPath EnableLoggingMessage where
    toPath = const "/"

instance ToQuery EnableLoggingMessage

instance AWSRequest EnableLoggingMessage where
    type Sv EnableLoggingMessage = Redshift
    type Rs EnableLoggingMessage = LoggingStatus

    request  = post "EnableLogging"
    response = const . xmlResponse $ const decodeCursor
