{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DirectConnect.DeleteConnection
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the connection. Deleting a connection only stops the AWS Direct
-- Connect port hour and data transfer charges. You need to cancel separately
-- with the providers any services or charges for cross-connects or network
-- circuits that connect you to the AWS Direct Connect location.
--
-- <http://docs.aws.amazon.com/directconnect/latest/APIReference/API_DeleteConnection.html>
module Network.AWS.DirectConnect.DeleteConnection
    (
    -- * Request
      DeleteConnection
    -- ** Request constructor
    , deleteConnection
    -- ** Request lenses
    , dcConnectionId

    -- * Response
    , DeleteConnectionResponse
    -- ** Response constructor
    , deleteConnectionResponse
    -- ** Response lenses
    , dcrBandwidth
    , dcrConnectionId
    , dcrConnectionName
    , dcrConnectionState
    , dcrLocation
    , dcrOwnerAccount
    , dcrPartnerName
    , dcrRegion
    , dcrVlan
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.DirectConnect.Types
import qualified GHC.Exts

newtype DeleteConnection = DeleteConnection
    { _dcConnectionId :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'DeleteConnection' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcConnectionId' @::@ 'Text'
--
deleteConnection :: Text -- ^ 'dcConnectionId'
                 -> DeleteConnection
deleteConnection p1 = DeleteConnection
    { _dcConnectionId = p1
    }

dcConnectionId :: Lens' DeleteConnection Text
dcConnectionId = lens _dcConnectionId (\s a -> s { _dcConnectionId = a })

data DeleteConnectionResponse = DeleteConnectionResponse
    { _dcrBandwidth       :: Maybe Text
    , _dcrConnectionId    :: Maybe Text
    , _dcrConnectionName  :: Maybe Text
    , _dcrConnectionState :: Maybe Text
    , _dcrLocation        :: Maybe Text
    , _dcrOwnerAccount    :: Maybe Text
    , _dcrPartnerName     :: Maybe Text
    , _dcrRegion          :: Maybe Text
    , _dcrVlan            :: Maybe Int
    } deriving (Eq, Ord, Show, Generic)

-- | 'DeleteConnectionResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcrBandwidth' @::@ 'Maybe' 'Text'
--
-- * 'dcrConnectionId' @::@ 'Maybe' 'Text'
--
-- * 'dcrConnectionName' @::@ 'Maybe' 'Text'
--
-- * 'dcrConnectionState' @::@ 'Maybe' 'Text'
--
-- * 'dcrLocation' @::@ 'Maybe' 'Text'
--
-- * 'dcrOwnerAccount' @::@ 'Maybe' 'Text'
--
-- * 'dcrPartnerName' @::@ 'Maybe' 'Text'
--
-- * 'dcrRegion' @::@ 'Maybe' 'Text'
--
-- * 'dcrVlan' @::@ 'Maybe' 'Int'
--
deleteConnectionResponse :: DeleteConnectionResponse
deleteConnectionResponse = DeleteConnectionResponse
    { _dcrOwnerAccount    = Nothing
    , _dcrConnectionId    = Nothing
    , _dcrConnectionName  = Nothing
    , _dcrConnectionState = Nothing
    , _dcrRegion          = Nothing
    , _dcrLocation        = Nothing
    , _dcrBandwidth       = Nothing
    , _dcrVlan            = Nothing
    , _dcrPartnerName     = Nothing
    }

-- | Bandwidth of the connection. Example: 1Gbps (for regular connections), or
-- 500Mbps (for hosted connections) Default: None.
dcrBandwidth :: Lens' DeleteConnectionResponse (Maybe Text)
dcrBandwidth = lens _dcrBandwidth (\s a -> s { _dcrBandwidth = a })

dcrConnectionId :: Lens' DeleteConnectionResponse (Maybe Text)
dcrConnectionId = lens _dcrConnectionId (\s a -> s { _dcrConnectionId = a })

dcrConnectionName :: Lens' DeleteConnectionResponse (Maybe Text)
dcrConnectionName =
    lens _dcrConnectionName (\s a -> s { _dcrConnectionName = a })

dcrConnectionState :: Lens' DeleteConnectionResponse (Maybe Text)
dcrConnectionState =
    lens _dcrConnectionState (\s a -> s { _dcrConnectionState = a })

dcrLocation :: Lens' DeleteConnectionResponse (Maybe Text)
dcrLocation = lens _dcrLocation (\s a -> s { _dcrLocation = a })

dcrOwnerAccount :: Lens' DeleteConnectionResponse (Maybe Text)
dcrOwnerAccount = lens _dcrOwnerAccount (\s a -> s { _dcrOwnerAccount = a })

dcrPartnerName :: Lens' DeleteConnectionResponse (Maybe Text)
dcrPartnerName = lens _dcrPartnerName (\s a -> s { _dcrPartnerName = a })

dcrRegion :: Lens' DeleteConnectionResponse (Maybe Text)
dcrRegion = lens _dcrRegion (\s a -> s { _dcrRegion = a })

dcrVlan :: Lens' DeleteConnectionResponse (Maybe Int)
dcrVlan = lens _dcrVlan (\s a -> s { _dcrVlan = a })

instance ToPath DeleteConnection where
    toPath = const "/"

instance ToQuery DeleteConnection where
    toQuery = const mempty

instance ToHeaders DeleteConnection
instance ToJSON DeleteConnection where
    toJSON = genericToJSON jsonOptions

instance AWSRequest DeleteConnection where
    type Sv DeleteConnection = DirectConnect
    type Rs DeleteConnection = DeleteConnectionResponse

    request  = post "DeleteConnection"
    response = jsonResponse

instance FromJSON DeleteConnectionResponse where
    parseJSON = genericParseJSON jsonOptions
