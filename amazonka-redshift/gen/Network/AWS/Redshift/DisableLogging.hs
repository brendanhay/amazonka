{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DisableLogging
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Stops logging information, such as queries and connection attempts, for
-- the specified Amazon Redshift cluster.
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_DisableLogging.html>
module Network.AWS.Redshift.DisableLogging
    (
    -- * Request
      DisableLogging
    -- ** Request constructor
    , disableLogging
    -- ** Request lenses
    , dlClusterIdentifier

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
-- /See:/ 'disableLogging' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlClusterIdentifier'
newtype DisableLogging = DisableLogging'
    { _dlClusterIdentifier :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DisableLogging' smart constructor.
disableLogging :: Text -> DisableLogging
disableLogging pClusterIdentifier =
    DisableLogging'
    { _dlClusterIdentifier = pClusterIdentifier
    }

-- | The identifier of the cluster on which logging is to be stopped.
--
-- Example: @examplecluster@
dlClusterIdentifier :: Lens' DisableLogging Text
dlClusterIdentifier = lens _dlClusterIdentifier (\ s a -> s{_dlClusterIdentifier = a});

instance AWSRequest DisableLogging where
        type Sv DisableLogging = Redshift
        type Rs DisableLogging = LoggingStatus
        request = post
        response
          = receiveXMLWrapper "DisableLoggingResult"
              (\ s h x -> parseXML x)

instance ToHeaders DisableLogging where
        toHeaders = const mempty

instance ToPath DisableLogging where
        toPath = const "/"

instance ToQuery DisableLogging where
        toQuery DisableLogging'{..}
          = mconcat
              ["Action" =: ("DisableLogging" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "ClusterIdentifier" =: _dlClusterIdentifier]
