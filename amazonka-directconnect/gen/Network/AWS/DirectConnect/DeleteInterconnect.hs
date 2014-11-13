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

-- Module      : Network.AWS.DirectConnect.DeleteInterconnect
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified interconnect.
module Network.AWS.DirectConnect.DeleteInterconnect
    (
    -- * Request
      DeleteInterconnect
    -- ** Request constructor
    , deleteInterconnect
    -- ** Request lenses
    , di1InterconnectId

    -- * Response
    , DeleteInterconnectResponse
    -- ** Response constructor
    , deleteInterconnectResponse
    -- ** Response lenses
    , dirInterconnectState
    ) where

import Data.Aeson
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.DirectConnect.Types

newtype DeleteInterconnect = DeleteInterconnect
    { _di1InterconnectId :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'DeleteInterconnect' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'di1InterconnectId' @::@ 'Text'
--
deleteInterconnect :: Text -- ^ 'di1InterconnectId'
                   -> DeleteInterconnect
deleteInterconnect p1 = DeleteInterconnect
    { _di1InterconnectId = p1
    }

di1InterconnectId :: Lens' DeleteInterconnect Text
di1InterconnectId =
    lens _di1InterconnectId (\s a -> s { _di1InterconnectId = a })

instance ToPath DeleteInterconnect where
    toPath = const "/"

instance ToQuery DeleteInterconnect where
    toQuery = const mempty

instance ToHeaders DeleteInterconnect

instance ToBody DeleteInterconnect where
    toBody = toBody . encode . _di1InterconnectId

newtype DeleteInterconnectResponse = DeleteInterconnectResponse
    { _dirInterconnectState :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'DeleteInterconnectResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dirInterconnectState' @::@ 'Maybe' 'Text'
--
deleteInterconnectResponse :: DeleteInterconnectResponse
deleteInterconnectResponse = DeleteInterconnectResponse
    { _dirInterconnectState = Nothing
    }

dirInterconnectState :: Lens' DeleteInterconnectResponse (Maybe Text)
dirInterconnectState =
    lens _dirInterconnectState (\s a -> s { _dirInterconnectState = a })

-- FromJSON

instance AWSRequest DeleteInterconnect where
    type Sv DeleteInterconnect = DirectConnect
    type Rs DeleteInterconnect = DeleteInterconnectResponse

    request  = post'
    response = jsonResponse $ \h o -> DeleteInterconnectResponse
        <$> o .: "interconnectState"
