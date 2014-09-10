{-# LANGUAGE DeriveGeneric               #-}
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
module Network.AWS.OpsWorks.GetHostnameSuggestion
    (
    -- * Request
      GetHostnameSuggestion
    -- ** Request constructor
    , mkGetHostnameSuggestion
    -- ** Request lenses
    , ghsLayerId

    -- * Response
    , GetHostnameSuggestionResponse
    -- ** Response constructor
    , mkGetHostnameSuggestionResponse
    -- ** Response lenses
    , ghsrLayerId
    , ghsrHostname
    ) where

import Network.AWS.OpsWorks.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

newtype GetHostnameSuggestion = GetHostnameSuggestion
    { _ghsLayerId :: !Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetHostnameSuggestion' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @LayerId ::@ @Text@
--
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
    { _ghsrLayerId :: !(Maybe Text)
    , _ghsrHostname :: !(Maybe Text)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetHostnameSuggestionResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @LayerId ::@ @Maybe Text@
--
-- * @Hostname ::@ @Maybe Text@
--
mkGetHostnameSuggestionResponse :: GetHostnameSuggestionResponse
mkGetHostnameSuggestionResponse = GetHostnameSuggestionResponse
    { _ghsrLayerId = Nothing
    , _ghsrHostname = Nothing
    }

-- | The layer ID.
ghsrLayerId :: Lens' GetHostnameSuggestionResponse (Maybe Text)
ghsrLayerId = lens _ghsrLayerId (\s a -> s { _ghsrLayerId = a })

-- | The generated host name.
ghsrHostname :: Lens' GetHostnameSuggestionResponse (Maybe Text)
ghsrHostname = lens _ghsrHostname (\s a -> s { _ghsrHostname = a })

instance FromJSON GetHostnameSuggestionResponse

instance AWSRequest GetHostnameSuggestion where
    type Sv GetHostnameSuggestion = OpsWorks
    type Rs GetHostnameSuggestion = GetHostnameSuggestionResponse

    request = get
    response _ = jsonResponse
