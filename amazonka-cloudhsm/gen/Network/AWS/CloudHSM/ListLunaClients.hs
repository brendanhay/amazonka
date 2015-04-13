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

-- Module      : Network.AWS.CloudHSM.ListLunaClients
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

-- | Lists all of the clients.
--
-- This operation supports pagination with the use of the /NextToken/ member. If
-- more results are available, the /NextToken/ member of the response contains a
-- token that you pass in the next call to 'ListLunaClients' to retrieve the next
-- set of items.
--
-- <http://docs.aws.amazon.com/cloudhsm/latest/dg/API_ListLunaClients.html>
module Network.AWS.CloudHSM.ListLunaClients
    (
    -- * Request
      ListLunaClients
    -- ** Request constructor
    , listLunaClients
    -- ** Request lenses
    , llcNextToken

    -- * Response
    , ListLunaClientsResponse
    -- ** Response constructor
    , listLunaClientsResponse
    -- ** Response lenses
    , llcrClientList
    , llcrNextToken
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.CloudHSM.Types
import qualified GHC.Exts

newtype ListLunaClients = ListLunaClients
    { _llcNextToken :: Maybe Text
    } deriving (Eq, Ord, Read, Show, Monoid)

-- | 'ListLunaClients' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'llcNextToken' @::@ 'Maybe' 'Text'
--
listLunaClients :: ListLunaClients
listLunaClients = ListLunaClients
    { _llcNextToken = Nothing
    }

-- | The /NextToken/ value from a previous call to 'ListLunaClients'. Pass null if
-- this is the first call.
llcNextToken :: Lens' ListLunaClients (Maybe Text)
llcNextToken = lens _llcNextToken (\s a -> s { _llcNextToken = a })

data ListLunaClientsResponse = ListLunaClientsResponse
    { _llcrClientList :: List "ClientList" Text
    , _llcrNextToken  :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'ListLunaClientsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'llcrClientList' @::@ ['Text']
--
-- * 'llcrNextToken' @::@ 'Maybe' 'Text'
--
listLunaClientsResponse :: ListLunaClientsResponse
listLunaClientsResponse = ListLunaClientsResponse
    { _llcrClientList = mempty
    , _llcrNextToken  = Nothing
    }

-- | The list of clients.
llcrClientList :: Lens' ListLunaClientsResponse [Text]
llcrClientList = lens _llcrClientList (\s a -> s { _llcrClientList = a }) . _List

-- | If not null, more results are available. Pass this to 'ListLunaClients' to
-- retrieve the next set of items.
llcrNextToken :: Lens' ListLunaClientsResponse (Maybe Text)
llcrNextToken = lens _llcrNextToken (\s a -> s { _llcrNextToken = a })

instance ToPath ListLunaClients where
    toPath = const "/"

instance ToQuery ListLunaClients where
    toQuery = const mempty

instance ToHeaders ListLunaClients

instance ToJSON ListLunaClients where
    toJSON ListLunaClients{..} = object
        [ "NextToken" .= _llcNextToken
        ]

instance AWSRequest ListLunaClients where
    type Sv ListLunaClients = CloudHSM
    type Rs ListLunaClients = ListLunaClientsResponse

    request  = post "ListLunaClients"
    response = jsonResponse

instance FromJSON ListLunaClientsResponse where
    parseJSON = withObject "ListLunaClientsResponse" $ \o -> ListLunaClientsResponse
        <$> o .:? "ClientList" .!= mempty
        <*> o .:? "NextToken"
