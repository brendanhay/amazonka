{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.V2013_02_18.GetHostnameSuggestion
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
module Network.AWS.OpsWorks.V2013_02_18.GetHostnameSuggestion
    (
    -- * Request
      GetHostnameSuggestion
    -- ** Request constructor
    , mkGetHostnameSuggestion
    -- ** Request lenses
    , ghsLayerId

    -- * Response
    , GetHostnameSuggestionResponse
    -- ** Response lenses
    , ghsrsLayerId
    , ghsrsHostname
    ) where

import           Network.AWS.OpsWorks.V2013_02_18.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

newtype GetHostnameSuggestion = GetHostnameSuggestion
    { _ghsLayerId :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetHostnameSuggestion' request.
mkGetHostnameSuggestion :: Text -- ^ 'ghsLayerId'
                        -> GetHostnameSuggestion
mkGetHostnameSuggestion p1 = GetHostnameSuggestion
    { _ghsLayerId = p1
    }

-- | The layer ID.
ghsLayerId :: Lens' GetHostnameSuggestion Text
ghsLayerId = lens _ghsLayerId (\s a -> s { _ghsLayerId = a })

instance ToPath GetHostnameSuggestion

instance ToQuery GetHostnameSuggestion

instance ToHeaders GetHostnameSuggestion

instance ToJSON GetHostnameSuggestion

-- | Contains the response to a GetHostnameSuggestion request.
data GetHostnameSuggestionResponse = GetHostnameSuggestionResponse
    { _ghsrsLayerId :: Maybe Text
    , _ghsrsHostname :: Maybe Text
    } deriving (Show, Generic)

-- | The layer ID.
ghsrsLayerId :: Lens' GetHostnameSuggestionResponse (Maybe Text)
ghsrsLayerId = lens _ghsrsLayerId (\s a -> s { _ghsrsLayerId = a })

-- | The generated host name.
ghsrsHostname :: Lens' GetHostnameSuggestionResponse (Maybe Text)
ghsrsHostname = lens _ghsrsHostname (\s a -> s { _ghsrsHostname = a })

instance FromJSON GetHostnameSuggestionResponse

instance AWSRequest GetHostnameSuggestion where
    type Sv GetHostnameSuggestion = OpsWorks
    type Rs GetHostnameSuggestion = GetHostnameSuggestionResponse

    request = get
    response _ = jsonResponse
