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

-- Module      : Network.AWS.StorageGateway.DeleteTape
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified virtual tape.
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_DeleteTape.html>
module Network.AWS.StorageGateway.DeleteTape
    (
    -- * Request
      DeleteTape
    -- ** Request constructor
    , deleteTape
    -- ** Request lenses
    , dt1GatewayARN
    , dt1TapeARN

    -- * Response
    , DeleteTapeResponse
    -- ** Response constructor
    , deleteTapeResponse
    -- ** Response lenses
    , dtrTapeARN
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.StorageGateway.Types
import qualified GHC.Exts

data DeleteTape = DeleteTape
    { _dt1GatewayARN :: Text
    , _dt1TapeARN    :: Text
    } deriving (Eq, Ord, Show)

-- | 'DeleteTape' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dt1GatewayARN' @::@ 'Text'
--
-- * 'dt1TapeARN' @::@ 'Text'
--
deleteTape :: Text -- ^ 'dt1GatewayARN'
           -> Text -- ^ 'dt1TapeARN'
           -> DeleteTape
deleteTape p1 p2 = DeleteTape
    { _dt1GatewayARN = p1
    , _dt1TapeARN    = p2
    }

-- | The unique Amazon Resource Name (ARN) of the gateway that the virtual tape to
-- delete is associated with. Use the 'ListGateways' operation to return a list of
-- gateways for your account and region.
dt1GatewayARN :: Lens' DeleteTape Text
dt1GatewayARN = lens _dt1GatewayARN (\s a -> s { _dt1GatewayARN = a })

-- | The Amazon Resource Name (ARN) of the virtual tape to delete.
dt1TapeARN :: Lens' DeleteTape Text
dt1TapeARN = lens _dt1TapeARN (\s a -> s { _dt1TapeARN = a })

newtype DeleteTapeResponse = DeleteTapeResponse
    { _dtrTapeARN :: Maybe Text
    } deriving (Eq, Ord, Show, Monoid)

-- | 'DeleteTapeResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtrTapeARN' @::@ 'Maybe' 'Text'
--
deleteTapeResponse :: DeleteTapeResponse
deleteTapeResponse = DeleteTapeResponse
    { _dtrTapeARN = Nothing
    }

-- | The Amazon Resource Name (ARN) of the deleted virtual tape.
dtrTapeARN :: Lens' DeleteTapeResponse (Maybe Text)
dtrTapeARN = lens _dtrTapeARN (\s a -> s { _dtrTapeARN = a })

instance ToPath DeleteTape where
    toPath = const "/"

instance ToQuery DeleteTape where
    toQuery = const mempty

instance ToHeaders DeleteTape

instance ToJSON DeleteTape where
    toJSON DeleteTape{..} = object
        [ "GatewayARN" .= _dt1GatewayARN
        , "TapeARN"    .= _dt1TapeARN
        ]

instance AWSRequest DeleteTape where
    type Sv DeleteTape = StorageGateway
    type Rs DeleteTape = DeleteTapeResponse

    request  = post "DeleteTape"
    response = jsonResponse

instance FromJSON DeleteTapeResponse where
    parseJSON = withObject "DeleteTapeResponse" $ \o -> DeleteTapeResponse
        <$> o .:? "TapeARN"
