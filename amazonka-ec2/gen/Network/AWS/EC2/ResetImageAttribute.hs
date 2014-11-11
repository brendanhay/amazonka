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

-- Module      : Network.AWS.EC2.ResetImageAttribute
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Resets an attribute of an AMI to its default value.
module Network.AWS.EC2.ResetImageAttribute
    (
    -- * Request
      ResetImageAttribute
    -- ** Request constructor
    , resetImageAttribute
    -- ** Request lenses
    , riaAttribute
    , riaDryRun
    , riaImageId

    -- * Response
    , ResetImageAttributeResponse
    -- ** Response constructor
    , resetImageAttributeResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types

data ResetImageAttribute = ResetImageAttribute
    { _riaAttribute :: Text
    , _riaDryRun    :: Maybe Bool
    , _riaImageId   :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ResetImageAttribute' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'riaAttribute' @::@ 'Text'
--
-- * 'riaDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'riaImageId' @::@ 'Text'
--
resetImageAttribute :: Text -- ^ 'riaImageId'
                    -> Text -- ^ 'riaAttribute'
                    -> ResetImageAttribute
resetImageAttribute p1 p2 = ResetImageAttribute
    { _riaImageId   = p1
    , _riaAttribute = p2
    , _riaDryRun    = Nothing
    }

-- | The attribute to reset (currently you can only reset the launch
-- permission attribute).
riaAttribute :: Lens' ResetImageAttribute Text
riaAttribute = lens _riaAttribute (\s a -> s { _riaAttribute = a })

riaDryRun :: Lens' ResetImageAttribute (Maybe Bool)
riaDryRun = lens _riaDryRun (\s a -> s { _riaDryRun = a })

-- | The ID of the AMI.
riaImageId :: Lens' ResetImageAttribute Text
riaImageId = lens _riaImageId (\s a -> s { _riaImageId = a })
instance ToQuery ResetImageAttribute

instance ToPath ResetImageAttribute where
    toPath = const "/"

data ResetImageAttributeResponse = ResetImageAttributeResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'ResetImageAttributeResponse' constructor.
resetImageAttributeResponse :: ResetImageAttributeResponse
resetImageAttributeResponse = ResetImageAttributeResponse
instance FromXML ResetImageAttributeResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ResetImageAttributeResponse"

instance AWSRequest ResetImageAttribute where
    type Sv ResetImageAttribute = EC2
    type Rs ResetImageAttribute = ResetImageAttributeResponse

    request  = post "ResetImageAttribute"
    response = nullaryResponse ResetImageAttributeResponse
