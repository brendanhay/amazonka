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

-- Module      : Network.AWS.Redshift.DescribeLoggingStatus
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes whether information, such as queries and connection attempts, is
-- being logged for the specified Amazon Redshift cluster.
module Network.AWS.Redshift.DescribeLoggingStatus
    (
    -- * Request
      DescribeLoggingStatusMessage
    -- ** Request constructor
    , describeLoggingStatusMessage
    -- ** Request lenses
    , dlsmClusterIdentifier

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

newtype DescribeLoggingStatusMessage = DescribeLoggingStatusMessage
    { _dlsmClusterIdentifier :: Text
    } (Eq, Ord, Show, Generic, Monoid)

-- | 'DescribeLoggingStatusMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlsmClusterIdentifier' @::@ 'Text'
--
describeLoggingStatusMessage :: Text -- ^ 'dlsmClusterIdentifier'
                             -> DescribeLoggingStatusMessage
describeLoggingStatusMessage p1 = DescribeLoggingStatusMessage
    { _dlsmClusterIdentifier = p1
    }

-- | The identifier of the cluster to get the logging status from. Example:
-- examplecluster.
dlsmClusterIdentifier :: Lens' DescribeLoggingStatusMessage Text
dlsmClusterIdentifier =
    lens _dlsmClusterIdentifier (\s a -> s { _dlsmClusterIdentifier = a })
instance ToQuery DescribeLoggingStatusMessage

instance ToPath DescribeLoggingStatusMessage where
    toPath = const "/"

instance AWSRequest DescribeLoggingStatusMessage where
    type Sv DescribeLoggingStatusMessage = Redshift
    type Rs DescribeLoggingStatusMessage = LoggingStatus

    request  = post "DescribeLoggingStatus"
    response = xmlResponse $ const decodeCursor
