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
-- Module      : Network.AWS.Redshift.DisableLogging
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops logging information, such as queries and connection attempts, for the specified Amazon Redshift cluster.
--
--
module Network.AWS.Redshift.DisableLogging
    (
    -- * Creating a Request
      disableLogging
    , DisableLogging
    -- * Request Lenses
    , dlClusterIdentifier

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
-- /See:/ 'disableLogging' smart constructor.
newtype DisableLogging = DisableLogging'
  { _dlClusterIdentifier :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisableLogging' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlClusterIdentifier' - The identifier of the cluster on which logging is to be stopped. Example: @examplecluster@
disableLogging
    :: Text -- ^ 'dlClusterIdentifier'
    -> DisableLogging
disableLogging pClusterIdentifier_ =
  DisableLogging' {_dlClusterIdentifier = pClusterIdentifier_}


-- | The identifier of the cluster on which logging is to be stopped. Example: @examplecluster@
dlClusterIdentifier :: Lens' DisableLogging Text
dlClusterIdentifier = lens _dlClusterIdentifier (\ s a -> s{_dlClusterIdentifier = a})

instance AWSRequest DisableLogging where
        type Rs DisableLogging = LoggingStatus
        request = postQuery redshift
        response
          = receiveXMLWrapper "DisableLoggingResult"
              (\ s h x -> parseXML x)

instance Hashable DisableLogging where

instance NFData DisableLogging where

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
