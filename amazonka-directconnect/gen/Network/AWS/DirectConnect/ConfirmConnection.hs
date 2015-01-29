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

-- Module      : Network.AWS.DirectConnect.ConfirmConnection
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Confirm the creation of a hosted connection on an interconnect.
--
-- Upon creation, the hosted connection is initially in the 'Ordering' state,
-- and will remain in this state until the owner calls ConfirmConnection to
-- confirm creation of the hosted connection.
--
-- <http://docs.aws.amazon.com/directconnect/latest/APIReference/API_ConfirmConnection.html>
module Network.AWS.DirectConnect.ConfirmConnection
    (
    -- * Request
      ConfirmConnection
    -- ** Request constructor
    , confirmConnection
    -- ** Request lenses
    , ccConnectionId

    -- * Response
    , ConfirmConnectionResponse
    -- ** Response constructor
    , confirmConnectionResponse
    -- ** Response lenses
    , ccr1ConnectionState
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.DirectConnect.Types
import qualified GHC.Exts

newtype ConfirmConnection = ConfirmConnection
    { _ccConnectionId :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

-- | 'ConfirmConnection' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccConnectionId' @::@ 'Text'
--
confirmConnection :: Text -- ^ 'ccConnectionId'
                  -> ConfirmConnection
confirmConnection p1 = ConfirmConnection
    { _ccConnectionId = p1
    }

ccConnectionId :: Lens' ConfirmConnection Text
ccConnectionId = lens _ccConnectionId (\s a -> s { _ccConnectionId = a })

newtype ConfirmConnectionResponse = ConfirmConnectionResponse
    { _ccr1ConnectionState :: Maybe ConnectionState
    } deriving (Eq, Read, Show)

-- | 'ConfirmConnectionResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccr1ConnectionState' @::@ 'Maybe' 'ConnectionState'
--
confirmConnectionResponse :: ConfirmConnectionResponse
confirmConnectionResponse = ConfirmConnectionResponse
    { _ccr1ConnectionState = Nothing
    }

ccr1ConnectionState :: Lens' ConfirmConnectionResponse (Maybe ConnectionState)
ccr1ConnectionState =
    lens _ccr1ConnectionState (\s a -> s { _ccr1ConnectionState = a })

instance ToPath ConfirmConnection where
    toPath = const "/"

instance ToQuery ConfirmConnection where
    toQuery = const mempty

instance ToHeaders ConfirmConnection

instance ToJSON ConfirmConnection where
    toJSON ConfirmConnection{..} = object
        [ "connectionId" .= _ccConnectionId
        ]

instance AWSRequest ConfirmConnection where
    type Sv ConfirmConnection = DirectConnect
    type Rs ConfirmConnection = ConfirmConnectionResponse

    request  = post "ConfirmConnection"
    response = jsonResponse

instance FromJSON ConfirmConnectionResponse where
    parseJSON = withObject "ConfirmConnectionResponse" $ \o -> ConfirmConnectionResponse
        <$> o .:? "connectionState"
