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

-- Module      : Network.AWS.DirectConnect.DeleteInterconnect
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

-- | Deletes the specified interconnect.
--
-- <http://docs.aws.amazon.com/directconnect/latest/APIReference/API_DeleteInterconnect.html>
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

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.DirectConnect.Types
import qualified GHC.Exts

newtype DeleteInterconnect = DeleteInterconnect
    { _di1InterconnectId :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

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

newtype DeleteInterconnectResponse = DeleteInterconnectResponse
    { _dirInterconnectState :: Maybe InterconnectState
    } deriving (Eq, Read, Show)

-- | 'DeleteInterconnectResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dirInterconnectState' @::@ 'Maybe' 'InterconnectState'
--
deleteInterconnectResponse :: DeleteInterconnectResponse
deleteInterconnectResponse = DeleteInterconnectResponse
    { _dirInterconnectState = Nothing
    }

dirInterconnectState :: Lens' DeleteInterconnectResponse (Maybe InterconnectState)
dirInterconnectState =
    lens _dirInterconnectState (\s a -> s { _dirInterconnectState = a })

instance ToPath DeleteInterconnect where
    toPath = const "/"

instance ToQuery DeleteInterconnect where
    toQuery = const mempty

instance ToHeaders DeleteInterconnect

instance ToJSON DeleteInterconnect where
    toJSON DeleteInterconnect{..} = object
        [ "interconnectId" .= _di1InterconnectId
        ]

instance AWSRequest DeleteInterconnect where
    type Sv DeleteInterconnect = DirectConnect
    type Rs DeleteInterconnect = DeleteInterconnectResponse

    request  = post "DeleteInterconnect"
    response = jsonResponse

instance FromJSON DeleteInterconnectResponse where
    parseJSON = withObject "DeleteInterconnectResponse" $ \o -> DeleteInterconnectResponse
        <$> o .:? "interconnectState"
