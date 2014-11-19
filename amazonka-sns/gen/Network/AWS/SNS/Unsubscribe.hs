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

-- Module      : Network.AWS.SNS.Unsubscribe
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a subscription. If the subscription requires authentication for
-- deletion, only the owner of the subscription or the topic's owner can
-- unsubscribe, and an AWS signature is required. If the Unsubscribe call does
-- not require authentication and the requester is not the subscription owner,
-- a final cancellation message is delivered to the endpoint, so that the
-- endpoint owner can easily resubscribe to the topic if the Unsubscribe
-- request was unintended.
--
-- <http://docs.aws.amazon.com/sns/latest/api/API_Unsubscribe.html>
module Network.AWS.SNS.Unsubscribe
    (
    -- * Request
      Unsubscribe
    -- ** Request constructor
    , unsubscribe
    -- ** Request lenses
    , uSubscriptionArn

    -- * Response
    , UnsubscribeResponse
    -- ** Response constructor
    , unsubscribeResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SNS.Types
import qualified GHC.Exts

newtype Unsubscribe = Unsubscribe
    { _uSubscriptionArn :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'Unsubscribe' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uSubscriptionArn' @::@ 'Text'
--
unsubscribe :: Text -- ^ 'uSubscriptionArn'
            -> Unsubscribe
unsubscribe p1 = Unsubscribe
    { _uSubscriptionArn = p1
    }

-- | The ARN of the subscription to be deleted.
uSubscriptionArn :: Lens' Unsubscribe Text
uSubscriptionArn = lens _uSubscriptionArn (\s a -> s { _uSubscriptionArn = a })

data UnsubscribeResponse = UnsubscribeResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'UnsubscribeResponse' constructor.
unsubscribeResponse :: UnsubscribeResponse
unsubscribeResponse = UnsubscribeResponse

instance ToPath Unsubscribe where
    toPath = const "/"

instance ToQuery Unsubscribe where
    toQuery Unsubscribe{..} = mconcat
        [ "SubscriptionArn" =? _uSubscriptionArn
        ]

instance ToHeaders Unsubscribe

instance AWSRequest Unsubscribe where
    type Sv Unsubscribe = SNS
    type Rs Unsubscribe = UnsubscribeResponse

    request  = post "Unsubscribe"
    response = nullResponse UnsubscribeResponse
