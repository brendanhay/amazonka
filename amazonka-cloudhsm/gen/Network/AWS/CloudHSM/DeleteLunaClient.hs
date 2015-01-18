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

-- Module      : Network.AWS.CloudHSM.DeleteLunaClient
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Deletes a client.
--
-- <http://docs.aws.amazon.com/cloudhsm/latest/dg/API_DeleteLunaClient.html>
module Network.AWS.CloudHSM.DeleteLunaClient
    (
    -- * Request
      DeleteLunaClient
    -- ** Request constructor
    , deleteLunaClient
    -- ** Request lenses
    , dlc1ClientArn

    -- * Response
    , DeleteLunaClientResponse
    -- ** Response constructor
    , deleteLunaClientResponse
    -- ** Response lenses
    , dlcrStatus
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.CloudHSM.Types
import qualified GHC.Exts

newtype DeleteLunaClient = DeleteLunaClient
    { _dlc1ClientArn :: Text
    } deriving (Eq, Ord, Show, Monoid, IsString)

-- | 'DeleteLunaClient' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlc1ClientArn' @::@ 'Text'
--
deleteLunaClient :: Text -- ^ 'dlc1ClientArn'
                 -> DeleteLunaClient
deleteLunaClient p1 = DeleteLunaClient
    { _dlc1ClientArn = p1
    }

-- | The ARN of the client to delete.
dlc1ClientArn :: Lens' DeleteLunaClient Text
dlc1ClientArn = lens _dlc1ClientArn (\s a -> s { _dlc1ClientArn = a })

newtype DeleteLunaClientResponse = DeleteLunaClientResponse
    { _dlcrStatus :: Text
    } deriving (Eq, Ord, Show, Monoid, IsString)

-- | 'DeleteLunaClientResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlcrStatus' @::@ 'Text'
--
deleteLunaClientResponse :: Text -- ^ 'dlcrStatus'
                         -> DeleteLunaClientResponse
deleteLunaClientResponse p1 = DeleteLunaClientResponse
    { _dlcrStatus = p1
    }

-- | The status of the action.
dlcrStatus :: Lens' DeleteLunaClientResponse Text
dlcrStatus = lens _dlcrStatus (\s a -> s { _dlcrStatus = a })

instance ToPath DeleteLunaClient where
    toPath = const "/"

instance ToQuery DeleteLunaClient where
    toQuery = const mempty

instance ToHeaders DeleteLunaClient

instance ToJSON DeleteLunaClient where
    toJSON DeleteLunaClient{..} = object
        [ "ClientArn" .= _dlc1ClientArn
        ]

instance AWSRequest DeleteLunaClient where
    type Sv DeleteLunaClient = CloudHSM
    type Rs DeleteLunaClient = DeleteLunaClientResponse

    request  = post "DeleteLunaClient"
    response = jsonResponse

instance FromJSON DeleteLunaClientResponse where
    parseJSON = withObject "DeleteLunaClientResponse" $ \o -> DeleteLunaClientResponse
        <$> o .:  "Status"
