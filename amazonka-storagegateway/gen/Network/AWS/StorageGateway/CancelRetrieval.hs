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

-- Module      : Network.AWS.StorageGateway.CancelRetrieval
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Cancels retrieval of a virtual tape from the virtual tape shelf (VTS) to a
-- gateway after the retrieval process is initiated. The virtual tape is
-- returned to the VTS.
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_CancelRetrieval.html>
module Network.AWS.StorageGateway.CancelRetrieval
    (
    -- * Request
      CancelRetrieval
    -- ** Request constructor
    , cancelRetrieval
    -- ** Request lenses
    , crGatewayARN
    , crTapeARN

    -- * Response
    , CancelRetrievalResponse
    -- ** Response constructor
    , cancelRetrievalResponse
    -- ** Response lenses
    , crrTapeARN
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.StorageGateway.Types
import qualified GHC.Exts

data CancelRetrieval = CancelRetrieval
    { _crGatewayARN :: Text
    , _crTapeARN    :: Text
    } deriving (Eq, Ord, Show)

-- | 'CancelRetrieval' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'crGatewayARN' @::@ 'Text'
--
-- * 'crTapeARN' @::@ 'Text'
--
cancelRetrieval :: Text -- ^ 'crGatewayARN'
                -> Text -- ^ 'crTapeARN'
                -> CancelRetrieval
cancelRetrieval p1 p2 = CancelRetrieval
    { _crGatewayARN = p1
    , _crTapeARN    = p2
    }

crGatewayARN :: Lens' CancelRetrieval Text
crGatewayARN = lens _crGatewayARN (\s a -> s { _crGatewayARN = a })

-- | The Amazon Resource Name (ARN) of the virtual tape you want to cancel
-- retrieval for.
crTapeARN :: Lens' CancelRetrieval Text
crTapeARN = lens _crTapeARN (\s a -> s { _crTapeARN = a })

newtype CancelRetrievalResponse = CancelRetrievalResponse
    { _crrTapeARN :: Maybe Text
    } deriving (Eq, Ord, Show, Monoid)

-- | 'CancelRetrievalResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'crrTapeARN' @::@ 'Maybe' 'Text'
--
cancelRetrievalResponse :: CancelRetrievalResponse
cancelRetrievalResponse = CancelRetrievalResponse
    { _crrTapeARN = Nothing
    }

-- | The Amazon Resource Name (ARN) of the virtual tape for which retrieval was
-- canceled.
crrTapeARN :: Lens' CancelRetrievalResponse (Maybe Text)
crrTapeARN = lens _crrTapeARN (\s a -> s { _crrTapeARN = a })

instance ToPath CancelRetrieval where
    toPath = const "/"

instance ToQuery CancelRetrieval where
    toQuery = const mempty

instance ToHeaders CancelRetrieval

instance ToJSON CancelRetrieval where
    toJSON CancelRetrieval{..} = object
        [ "GatewayARN" .= _crGatewayARN
        , "TapeARN"    .= _crTapeARN
        ]

instance AWSRequest CancelRetrieval where
    type Sv CancelRetrieval = StorageGateway
    type Rs CancelRetrieval = CancelRetrievalResponse

    request  = post "CancelRetrieval"
    response = jsonResponse

instance FromJSON CancelRetrievalResponse where
    parseJSON = withObject "CancelRetrievalResponse" $ \o -> CancelRetrievalResponse
        <$> o .:? "TapeARN"
