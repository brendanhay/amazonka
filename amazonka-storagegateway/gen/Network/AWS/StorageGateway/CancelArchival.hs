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

-- Module      : Network.AWS.StorageGateway.CancelArchival
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

-- | Cancels archiving of a virtual tape to the virtual tape shelf (VTS) after the
-- archiving process is initiated.
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_CancelArchival.html>
module Network.AWS.StorageGateway.CancelArchival
    (
    -- * Request
      CancelArchival
    -- ** Request constructor
    , cancelArchival
    -- ** Request lenses
    , caGatewayARN
    , caTapeARN

    -- * Response
    , CancelArchivalResponse
    -- ** Response constructor
    , cancelArchivalResponse
    -- ** Response lenses
    , carTapeARN
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.StorageGateway.Types
import qualified GHC.Exts

data CancelArchival = CancelArchival
    { _caGatewayARN :: Text
    , _caTapeARN    :: Text
    } deriving (Eq, Ord, Show)

-- | 'CancelArchival' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'caGatewayARN' @::@ 'Text'
--
-- * 'caTapeARN' @::@ 'Text'
--
cancelArchival :: Text -- ^ 'caGatewayARN'
               -> Text -- ^ 'caTapeARN'
               -> CancelArchival
cancelArchival p1 p2 = CancelArchival
    { _caGatewayARN = p1
    , _caTapeARN    = p2
    }

caGatewayARN :: Lens' CancelArchival Text
caGatewayARN = lens _caGatewayARN (\s a -> s { _caGatewayARN = a })

-- | The Amazon Resource Name (ARN) of the virtual tape you want to cancel
-- archiving for.
caTapeARN :: Lens' CancelArchival Text
caTapeARN = lens _caTapeARN (\s a -> s { _caTapeARN = a })

newtype CancelArchivalResponse = CancelArchivalResponse
    { _carTapeARN :: Maybe Text
    } deriving (Eq, Ord, Show, Monoid)

-- | 'CancelArchivalResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'carTapeARN' @::@ 'Maybe' 'Text'
--
cancelArchivalResponse :: CancelArchivalResponse
cancelArchivalResponse = CancelArchivalResponse
    { _carTapeARN = Nothing
    }

-- | The Amazon Resource Name (ARN) of the virtual tape for which archiving was
-- canceled.
carTapeARN :: Lens' CancelArchivalResponse (Maybe Text)
carTapeARN = lens _carTapeARN (\s a -> s { _carTapeARN = a })

instance ToPath CancelArchival where
    toPath = const "/"

instance ToQuery CancelArchival where
    toQuery = const mempty

instance ToHeaders CancelArchival

instance ToJSON CancelArchival where
    toJSON CancelArchival{..} = object
        [ "GatewayARN" .= _caGatewayARN
        , "TapeARN"    .= _caTapeARN
        ]

instance AWSRequest CancelArchival where
    type Sv CancelArchival = StorageGateway
    type Rs CancelArchival = CancelArchivalResponse

    request  = post "CancelArchival"
    response = jsonResponse

instance FromJSON CancelArchivalResponse where
    parseJSON = withObject "CancelArchivalResponse" $ \o -> CancelArchivalResponse
        <$> o .:? "TapeARN"
