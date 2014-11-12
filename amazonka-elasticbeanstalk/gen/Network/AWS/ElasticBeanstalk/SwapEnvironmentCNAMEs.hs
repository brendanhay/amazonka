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

-- Module      : Network.AWS.ElasticBeanstalk.SwapEnvironmentCNAMEs
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Swaps the CNAMEs of two environments.
module Network.AWS.ElasticBeanstalk.SwapEnvironmentCNAMEs
    (
    -- * Request
      SwapEnvironmentCNAMEsMessage
    -- ** Request constructor
    , swapEnvironmentCNAMEsMessage
    -- ** Request lenses
    , secnamemDestinationEnvironmentId
    , secnamemDestinationEnvironmentName
    , secnamemSourceEnvironmentId
    , secnamemSourceEnvironmentName

    -- * Response
    , SwapEnvironmentCNAMEsResponse
    -- ** Response constructor
    , swapEnvironmentCNAMEsResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.Types

data SwapEnvironmentCNAMEsMessage = SwapEnvironmentCNAMEsMessage
    { _secnamemDestinationEnvironmentId   :: Maybe Text
    , _secnamemDestinationEnvironmentName :: Maybe Text
    , _secnamemSourceEnvironmentId        :: Maybe Text
    , _secnamemSourceEnvironmentName      :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'SwapEnvironmentCNAMEsMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'secnamemDestinationEnvironmentId' @::@ 'Maybe' 'Text'
--
-- * 'secnamemDestinationEnvironmentName' @::@ 'Maybe' 'Text'
--
-- * 'secnamemSourceEnvironmentId' @::@ 'Maybe' 'Text'
--
-- * 'secnamemSourceEnvironmentName' @::@ 'Maybe' 'Text'
--
swapEnvironmentCNAMEsMessage :: SwapEnvironmentCNAMEsMessage
swapEnvironmentCNAMEsMessage = SwapEnvironmentCNAMEsMessage
    { _secnamemSourceEnvironmentId        = Nothing
    , _secnamemSourceEnvironmentName      = Nothing
    , _secnamemDestinationEnvironmentId   = Nothing
    , _secnamemDestinationEnvironmentName = Nothing
    }

-- | The ID of the destination environment. Condition: You must specify at
-- least the DestinationEnvironmentID or the DestinationEnvironmentName. You
-- may also specify both. You must specify the SourceEnvironmentId with the
-- DestinationEnvironmentId.
secnamemDestinationEnvironmentId :: Lens' SwapEnvironmentCNAMEsMessage (Maybe Text)
secnamemDestinationEnvironmentId =
    lens _secnamemDestinationEnvironmentId
        (\s a -> s { _secnamemDestinationEnvironmentId = a })

-- | The name of the destination environment. Condition: You must specify at
-- least the DestinationEnvironmentID or the DestinationEnvironmentName. You
-- may also specify both. You must specify the SourceEnvironmentName with
-- the DestinationEnvironmentName.
secnamemDestinationEnvironmentName :: Lens' SwapEnvironmentCNAMEsMessage (Maybe Text)
secnamemDestinationEnvironmentName =
    lens _secnamemDestinationEnvironmentName
        (\s a -> s { _secnamemDestinationEnvironmentName = a })

-- | The ID of the source environment. Condition: You must specify at least
-- the SourceEnvironmentID or the SourceEnvironmentName. You may also
-- specify both. If you specify the SourceEnvironmentId, you must specify
-- the DestinationEnvironmentId.
secnamemSourceEnvironmentId :: Lens' SwapEnvironmentCNAMEsMessage (Maybe Text)
secnamemSourceEnvironmentId =
    lens _secnamemSourceEnvironmentId
        (\s a -> s { _secnamemSourceEnvironmentId = a })

-- | The name of the source environment. Condition: You must specify at least
-- the SourceEnvironmentID or the SourceEnvironmentName. You may also
-- specify both. If you specify the SourceEnvironmentName, you must specify
-- the DestinationEnvironmentName.
secnamemSourceEnvironmentName :: Lens' SwapEnvironmentCNAMEsMessage (Maybe Text)
secnamemSourceEnvironmentName =
    lens _secnamemSourceEnvironmentName
        (\s a -> s { _secnamemSourceEnvironmentName = a })

instance ToQuery SwapEnvironmentCNAMEsMessage

instance ToPath SwapEnvironmentCNAMEsMessage where
    toPath = const "/"

data SwapEnvironmentCNAMEsResponse = SwapEnvironmentCNAMEsResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'SwapEnvironmentCNAMEsResponse' constructor.
swapEnvironmentCNAMEsResponse :: SwapEnvironmentCNAMEsResponse
swapEnvironmentCNAMEsResponse = SwapEnvironmentCNAMEsResponse

instance FromXML SwapEnvironmentCNAMEsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "SwapEnvironmentCNAMEsResponse"

instance AWSRequest SwapEnvironmentCNAMEsMessage where
    type Sv SwapEnvironmentCNAMEsMessage = ElasticBeanstalk
    type Rs SwapEnvironmentCNAMEsMessage = SwapEnvironmentCNAMEsResponse

    request  = post "SwapEnvironmentCNAMEs"
    response = nullaryResponse SwapEnvironmentCNAMEsResponse
