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

-- Module      : Network.AWS.StorageGateway.CancelArchival
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Cancels archiving of a virtual tape to the virtual tape shelf (VTS) after
-- the archiving process is initiated.
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

import Data.Aeson
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.StorageGateway.Types

data CancelArchival = CancelArchival
    { _caGatewayARN :: Text
    , _caTapeARN    :: Text
    } deriving (Eq, Ord, Show, Generic)

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

instance ToPath CancelArchival where
    toPath = const "/"

instance ToQuery CancelArchival where
    toQuery = const mempty

instance ToHeaders CancelArchival

instance ToBody CancelArchival where
    toBody = toBody . encode . _caGatewayARN

newtype CancelArchivalResponse = CancelArchivalResponse
    { _carTapeARN :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

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

-- | The Amazon Resource Name (ARN) of the virtual tape for which archiving
-- was canceled.
carTapeARN :: Lens' CancelArchivalResponse (Maybe Text)
carTapeARN = lens _carTapeARN (\s a -> s { _carTapeARN = a })

-- FromJSON

instance AWSRequest CancelArchival where
    type Sv CancelArchival = StorageGateway
    type Rs CancelArchival = CancelArchivalResponse

    request  = post'
    response = jsonResponse $ \h o -> CancelArchivalResponse
        <$> o .: "TapeARN"
