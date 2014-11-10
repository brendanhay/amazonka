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

-- Module      : Network.AWS.SNS.SetTopicAttributes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Allows a topic owner to set an attribute of the topic to a new value.
module Network.AWS.SNS.SetTopicAttributes
    (
    -- * Request
      SetTopicAttributesInput
    -- ** Request constructor
    , setTopicAttributes
    -- ** Request lenses
    , staiAttributeName
    , staiAttributeValue
    , staiTopicArn

    -- * Response
    , SetTopicAttributesResponse
    -- ** Response constructor
    , setTopicAttributesResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SNS.Types

data SetTopicAttributesInput = SetTopicAttributesInput
    { _staiAttributeName  :: Text
    , _staiAttributeValue :: Maybe Text
    , _staiTopicArn       :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'SetTopicAttributesInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'staiAttributeName' @::@ 'Text'
--
-- * 'staiAttributeValue' @::@ 'Maybe' 'Text'
--
-- * 'staiTopicArn' @::@ 'Text'
--
setTopicAttributes :: Text -- ^ 'staiTopicArn'
                   -> Text -- ^ 'staiAttributeName'
                   -> SetTopicAttributesInput
setTopicAttributes p1 p2 = SetTopicAttributesInput
    { _staiTopicArn       = p1
    , _staiAttributeName  = p2
    , _staiAttributeValue = Nothing
    }

-- | The name of the attribute you want to set. Only a subset of the topic's
-- attributes are mutable. Valid values: Policy | DisplayName |
-- DeliveryPolicy.
staiAttributeName :: Lens' SetTopicAttributesInput Text
staiAttributeName =
    lens _staiAttributeName (\s a -> s { _staiAttributeName = a })

-- | The new value for the attribute.
staiAttributeValue :: Lens' SetTopicAttributesInput (Maybe Text)
staiAttributeValue =
    lens _staiAttributeValue (\s a -> s { _staiAttributeValue = a })

-- | The ARN of the topic to modify.
staiTopicArn :: Lens' SetTopicAttributesInput Text
staiTopicArn = lens _staiTopicArn (\s a -> s { _staiTopicArn = a })

instance ToPath SetTopicAttributesInput where
    toPath = const "/"

instance ToQuery SetTopicAttributesInput

data SetTopicAttributesResponse = SetTopicAttributesResponse

-- | 'SetTopicAttributesResponse' constructor.
setTopicAttributesResponse :: SetTopicAttributesResponse
setTopicAttributesResponse = SetTopicAttributesResponse

instance AWSRequest SetTopicAttributesInput where
    type Sv SetTopicAttributesInput = SNS
    type Rs SetTopicAttributesInput = SetTopicAttributesResponse

    request  = post "SetTopicAttributes"
    response = const (nullaryResponse SetTopicAttributesResponse)
