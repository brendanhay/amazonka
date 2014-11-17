{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.DeleteChapCredentials
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation deletes Challenge-Handshake Authentication Protocol (CHAP)
-- credentials for a specified iSCSI target and initiator pair.
module Network.AWS.StorageGateway.DeleteChapCredentials
    (
    -- * Request
      DeleteChapCredentials
    -- ** Request constructor
    , deleteChapCredentials
    -- ** Request lenses
    , dcc1InitiatorName
    , dcc1TargetARN

    -- * Response
    , DeleteChapCredentialsResponse
    -- ** Response constructor
    , deleteChapCredentialsResponse
    -- ** Response lenses
    , dccrInitiatorName
    , dccrTargetARN
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.StorageGateway.Types
import qualified GHC.Exts

data DeleteChapCredentials = DeleteChapCredentials
    { _dcc1InitiatorName :: Text
    , _dcc1TargetARN     :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DeleteChapCredentials' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcc1InitiatorName' @::@ 'Text'
--
-- * 'dcc1TargetARN' @::@ 'Text'
--
deleteChapCredentials :: Text -- ^ 'dcc1TargetARN'
                      -> Text -- ^ 'dcc1InitiatorName'
                      -> DeleteChapCredentials
deleteChapCredentials p1 p2 = DeleteChapCredentials
    { _dcc1TargetARN     = p1
    , _dcc1InitiatorName = p2
    }

-- | The iSCSI initiator that connects to the target.
dcc1InitiatorName :: Lens' DeleteChapCredentials Text
dcc1InitiatorName =
    lens _dcc1InitiatorName (\s a -> s { _dcc1InitiatorName = a })

-- | The Amazon Resource Name (ARN) of the iSCSI volume target. Use the
-- DescribeStorediSCSIVolumes operation to return to retrieve the TargetARN
-- for specified VolumeARN.
dcc1TargetARN :: Lens' DeleteChapCredentials Text
dcc1TargetARN = lens _dcc1TargetARN (\s a -> s { _dcc1TargetARN = a })

data DeleteChapCredentialsResponse = DeleteChapCredentialsResponse
    { _dccrInitiatorName :: Maybe Text
    , _dccrTargetARN     :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DeleteChapCredentialsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dccrInitiatorName' @::@ 'Maybe' 'Text'
--
-- * 'dccrTargetARN' @::@ 'Maybe' 'Text'
--
deleteChapCredentialsResponse :: DeleteChapCredentialsResponse
deleteChapCredentialsResponse = DeleteChapCredentialsResponse
    { _dccrTargetARN     = Nothing
    , _dccrInitiatorName = Nothing
    }

-- | The iSCSI initiator that connects to the target.
dccrInitiatorName :: Lens' DeleteChapCredentialsResponse (Maybe Text)
dccrInitiatorName =
    lens _dccrInitiatorName (\s a -> s { _dccrInitiatorName = a })

-- | The Amazon Resource Name (ARN) of the target.
dccrTargetARN :: Lens' DeleteChapCredentialsResponse (Maybe Text)
dccrTargetARN = lens _dccrTargetARN (\s a -> s { _dccrTargetARN = a })

instance AWSRequest DeleteChapCredentials where
    type Sv DeleteChapCredentials = StorageGateway
    type Rs DeleteChapCredentials = DeleteChapCredentialsResponse

    request  = post
    response = jsonResponse

instance FromJSON DeleteChapCredentialsResponse where
    parseJSON = genericParseJSON jsonOptions

instance ToPath DeleteChapCredentials where
    toPath = const "/"

instance ToHeaders DeleteChapCredentials

instance ToQuery DeleteChapCredentials where
    toQuery = const mempty

instance ToJSON DeleteChapCredentials where
    toJSON = genericToJSON jsonOptions
