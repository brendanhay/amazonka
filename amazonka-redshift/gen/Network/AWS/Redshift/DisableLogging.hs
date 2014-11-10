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

-- Module      : Network.AWS.Redshift.DisableLogging
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Stops logging information, such as queries and connection attempts, for the
-- specified Amazon Redshift cluster.
module Network.AWS.Redshift.DisableLogging
    (
    -- * Request
      DisableLoggingMessage
    -- ** Request constructor
    , disableLogging
    -- ** Request lenses
    , dlmClusterIdentifier

    -- * Response
    , LoggingStatus
    -- ** Response constructor
    , disableLoggingResponse
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

newtype DisableLoggingMessage = DisableLoggingMessage
    { _dlmClusterIdentifier :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'DisableLoggingMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlmClusterIdentifier' @::@ 'Text'
--
disableLogging :: Text -- ^ 'dlmClusterIdentifier'
               -> DisableLoggingMessage
disableLogging p1 = DisableLoggingMessage
    { _dlmClusterIdentifier = p1
    }

-- | The identifier of the cluster on which logging is to be stopped. Example:
-- examplecluster.
dlmClusterIdentifier :: Lens' DisableLoggingMessage Text
dlmClusterIdentifier =
    lens _dlmClusterIdentifier (\s a -> s { _dlmClusterIdentifier = a })

instance ToPath DisableLoggingMessage where
    toPath = const "/"

instance ToQuery DisableLoggingMessage

instance AWSRequest DisableLoggingMessage where
    type Sv DisableLoggingMessage = Redshift
    type Rs DisableLoggingMessage = LoggingStatus

    request  = post "DisableLogging"
    response = xmlResponse $ const decodeCursor
