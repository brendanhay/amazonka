{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

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
module Network.AWS.OpsWorks.V2013_02_18.GetHostnameSuggestion where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.OpsWorks.V2013_02_18.Types
import Network.AWS.Prelude

data GetHostnameSuggestion = GetHostnameSuggestion
    { _ghsrLayerId :: Text
      -- ^ The layer ID.
    } deriving (Show, Generic)

makeLenses ''GetHostnameSuggestion

instance ToPath GetHostnameSuggestion

instance ToQuery GetHostnameSuggestion

instance ToHeaders GetHostnameSuggestion

instance ToJSON GetHostnameSuggestion

data GetHostnameSuggestionResponse = GetHostnameSuggestionResponse
    { _ghssHostname :: Maybe Text
      -- ^ The generated host name.
    , _ghssLayerId :: Maybe Text
      -- ^ The layer ID.
    } deriving (Show, Generic)

makeLenses ''GetHostnameSuggestionResponse

instance FromJSON GetHostnameSuggestionResponse

instance AWSRequest GetHostnameSuggestion where
    type Sv GetHostnameSuggestion = OpsWorks
    type Rs GetHostnameSuggestion = GetHostnameSuggestionResponse

    request = get
    response _ = jsonResponse
