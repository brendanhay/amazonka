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
    , getHostnameSuggestion
    -- ** Request lenses
    , ghsrLayerId

    -- * Response
    , GetHostnameSuggestionResponse
    -- ** Response lenses
    , ghssLayerId
    , ghssHostname
    ) where

import           Network.AWS.OpsWorks.V2013_02_18.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'GetHostnameSuggestion' request.
getHostnameSuggestion :: Text -- ^ 'ghsrLayerId'
                      -> GetHostnameSuggestion
getHostnameSuggestion p1 = GetHostnameSuggestion
    { _ghsrLayerId = p1
    }

data GetHostnameSuggestion = GetHostnameSuggestion
    { _ghsrLayerId :: Text
      -- ^ The layer ID.
    } deriving (Show, Generic)

-- | The layer ID.
ghsrLayerId
    :: Functor f
    => (Text
    -> f (Text))
    -> GetHostnameSuggestion
    -> f GetHostnameSuggestion
ghsrLayerId f x =
    (\y -> x { _ghsrLayerId = y })
       <$> f (_ghsrLayerId x)
{-# INLINE ghsrLayerId #-}

instance ToPath GetHostnameSuggestion

instance ToQuery GetHostnameSuggestion

instance ToHeaders GetHostnameSuggestion

instance ToJSON GetHostnameSuggestion

data GetHostnameSuggestionResponse = GetHostnameSuggestionResponse
    { _ghssLayerId :: Maybe Text
      -- ^ The layer ID.
    , _ghssHostname :: Maybe Text
      -- ^ The generated host name.
    } deriving (Show, Generic)

-- | The layer ID.
ghssLayerId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> GetHostnameSuggestionResponse
    -> f GetHostnameSuggestionResponse
ghssLayerId f x =
    (\y -> x { _ghssLayerId = y })
       <$> f (_ghssLayerId x)
{-# INLINE ghssLayerId #-}

-- | The generated host name.
ghssHostname
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> GetHostnameSuggestionResponse
    -> f GetHostnameSuggestionResponse
ghssHostname f x =
    (\y -> x { _ghssHostname = y })
       <$> f (_ghssHostname x)
{-# INLINE ghssHostname #-}

instance FromJSON GetHostnameSuggestionResponse

instance AWSRequest GetHostnameSuggestion where
    type Sv GetHostnameSuggestion = OpsWorks
    type Rs GetHostnameSuggestion = GetHostnameSuggestionResponse

    request = get
    response _ = jsonResponse
