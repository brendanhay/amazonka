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
--
-- <http://docs.aws.amazon.com/sns/latest/api/API_SetTopicAttributes.html>
module Network.AWS.SNS.SetTopicAttributes
    (
    -- * Request
      SetTopicAttributes
    -- ** Request constructor
    , setTopicAttributes
    -- ** Request lenses
    , staAttributeName
    , staAttributeValue
    , staTopicArn

    -- * Response
    , SetTopicAttributesResponse
    -- ** Response constructor
    , setTopicAttributesResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SNS.Types
import qualified GHC.Exts

data SetTopicAttributes = SetTopicAttributes
    { _staAttributeName  :: Text
    , _staAttributeValue :: Maybe Text
    , _staTopicArn       :: Text
    } deriving (Eq, Ord, Show)

-- | 'SetTopicAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'staAttributeName' @::@ 'Text'
--
-- * 'staAttributeValue' @::@ 'Maybe' 'Text'
--
-- * 'staTopicArn' @::@ 'Text'
--
setTopicAttributes :: Text -- ^ 'staTopicArn'
                   -> Text -- ^ 'staAttributeName'
                   -> SetTopicAttributes
setTopicAttributes p1 p2 = SetTopicAttributes
    { _staTopicArn       = p1
    , _staAttributeName  = p2
    , _staAttributeValue = Nothing
    }

-- | The name of the attribute you want to set. Only a subset of the topic's
-- attributes are mutable.
--
-- Valid values: 'Policy' | 'DisplayName' | 'DeliveryPolicy'
staAttributeName :: Lens' SetTopicAttributes Text
staAttributeName = lens _staAttributeName (\s a -> s { _staAttributeName = a })

-- | The new value for the attribute.
staAttributeValue :: Lens' SetTopicAttributes (Maybe Text)
staAttributeValue =
    lens _staAttributeValue (\s a -> s { _staAttributeValue = a })

-- | The ARN of the topic to modify.
staTopicArn :: Lens' SetTopicAttributes Text
staTopicArn = lens _staTopicArn (\s a -> s { _staTopicArn = a })

data SetTopicAttributesResponse = SetTopicAttributesResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'SetTopicAttributesResponse' constructor.
setTopicAttributesResponse :: SetTopicAttributesResponse
setTopicAttributesResponse = SetTopicAttributesResponse

instance ToPath SetTopicAttributes where
    toPath = const "/"

instance ToQuery SetTopicAttributes where
    toQuery SetTopicAttributes{..} = mconcat
        [ "AttributeName"  =? _staAttributeName
        , "AttributeValue" =? _staAttributeValue
        , "TopicArn"       =? _staTopicArn
        ]

instance ToHeaders SetTopicAttributes

instance AWSRequest SetTopicAttributes where
    type Sv SetTopicAttributes = SNS
    type Rs SetTopicAttributes = SetTopicAttributesResponse

    request  = post "SetTopicAttributes"
    response = nullResponse SetTopicAttributesResponse
