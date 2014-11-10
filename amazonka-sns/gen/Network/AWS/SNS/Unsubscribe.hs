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
module Network.AWS.SNS.Unsubscribe
    (
    -- * Request
      UnsubscribeInput
    -- ** Request constructor
    , unsubscribe
    -- ** Request lenses
    , uiSubscriptionArn

    -- * Response
    , UnsubscribeResponse
    -- ** Response constructor
    , unsubscribeResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SNS.Types

newtype UnsubscribeInput = UnsubscribeInput
    { _uiSubscriptionArn :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'UnsubscribeInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uiSubscriptionArn' @::@ 'Text'
--
unsubscribe :: Text -- ^ 'uiSubscriptionArn'
            -> UnsubscribeInput
unsubscribe p1 = UnsubscribeInput
    { _uiSubscriptionArn = p1
    }

-- | The ARN of the subscription to be deleted.
uiSubscriptionArn :: Lens' UnsubscribeInput Text
uiSubscriptionArn =
    lens _uiSubscriptionArn (\s a -> s { _uiSubscriptionArn = a })

instance ToPath UnsubscribeInput where
    toPath = const "/"

instance ToQuery UnsubscribeInput

data UnsubscribeResponse = UnsubscribeResponse

-- | 'UnsubscribeResponse' constructor.
unsubscribeResponse :: UnsubscribeResponse
unsubscribeResponse = UnsubscribeResponse

instance AWSRequest UnsubscribeInput where
    type Sv UnsubscribeInput = SNS
    type Rs UnsubscribeInput = UnsubscribeResponse

    request  = post "Unsubscribe"
    response = const (nullaryResponse UnsubscribeResponse)
