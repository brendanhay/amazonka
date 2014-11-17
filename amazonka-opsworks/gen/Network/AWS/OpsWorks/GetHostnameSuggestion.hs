{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.GetHostnameSuggestion
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Gets a generated host name for the specified layer, based on the current
-- host name theme. Required Permissions: To use this action, an IAM user must
-- have a Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see Managing User Permissions.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_GetHostnameSuggestion.html>
module Network.AWS.OpsWorks.GetHostnameSuggestion
    (
    -- * Request
      GetHostnameSuggestion
    -- ** Request constructor
    , getHostnameSuggestion
    -- ** Request lenses
    , ghsLayerId

    -- * Response
    , GetHostnameSuggestionResponse
    -- ** Response constructor
    , getHostnameSuggestionResponse
    -- ** Response lenses
    , ghsrHostname
    , ghsrLayerId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.OpsWorks.Types
import qualified GHC.Exts

newtype GetHostnameSuggestion = GetHostnameSuggestion
    { _ghsLayerId :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'GetHostnameSuggestion' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ghsLayerId' @::@ 'Text'
--
getHostnameSuggestion :: Text -- ^ 'ghsLayerId'
                      -> GetHostnameSuggestion
getHostnameSuggestion p1 = GetHostnameSuggestion
    { _ghsLayerId = p1
    }

-- | The layer ID.
ghsLayerId :: Lens' GetHostnameSuggestion Text
ghsLayerId = lens _ghsLayerId (\s a -> s { _ghsLayerId = a })

data GetHostnameSuggestionResponse = GetHostnameSuggestionResponse
    { _ghsrHostname :: Maybe Text
    , _ghsrLayerId  :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'GetHostnameSuggestionResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ghsrHostname' @::@ 'Maybe' 'Text'
--
-- * 'ghsrLayerId' @::@ 'Maybe' 'Text'
--
getHostnameSuggestionResponse :: GetHostnameSuggestionResponse
getHostnameSuggestionResponse = GetHostnameSuggestionResponse
    { _ghsrLayerId  = Nothing
    , _ghsrHostname = Nothing
    }

-- | The generated host name.
ghsrHostname :: Lens' GetHostnameSuggestionResponse (Maybe Text)
ghsrHostname = lens _ghsrHostname (\s a -> s { _ghsrHostname = a })

-- | The layer ID.
ghsrLayerId :: Lens' GetHostnameSuggestionResponse (Maybe Text)
ghsrLayerId = lens _ghsrLayerId (\s a -> s { _ghsrLayerId = a })

instance ToPath GetHostnameSuggestion where
    toPath = const "/"

instance ToQuery GetHostnameSuggestion where
    toQuery = const mempty

instance ToHeaders GetHostnameSuggestion
instance ToJSON GetHostnameSuggestion where
    toJSON = genericToJSON jsonOptions

instance AWSRequest GetHostnameSuggestion where
    type Sv GetHostnameSuggestion = OpsWorks
    type Rs GetHostnameSuggestion = GetHostnameSuggestionResponse

    request  = post
    response = jsonResponse

instance FromJSON GetHostnameSuggestionResponse where
    parseJSON = genericParseJSON jsonOptions
