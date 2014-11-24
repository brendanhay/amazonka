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

-- Module      : Network.AWS.SES.SetIdentityFeedbackForwardingEnabled
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Given an identity (email address or domain), enables or disables whether
-- Amazon SES forwards bounce and complaint notifications as email. Feedback
-- forwarding can only be disabled when Amazon Simple Notification Service
-- (Amazon SNS) topics are specified for both bounces and complaints. This
-- action is throttled at one request per second. For more information about
-- using notifications with Amazon SES, see the
-- <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/notifications.html
-- Amazon SES Developer Guide>.
--
-- <http://docs.aws.amazon.com/ses/latest/APIReference/API_SetIdentityFeedbackForwardingEnabled.html>
module Network.AWS.SES.SetIdentityFeedbackForwardingEnabled
    (
    -- * Request
      SetIdentityFeedbackForwardingEnabled
    -- ** Request constructor
    , setIdentityFeedbackForwardingEnabled
    -- ** Request lenses
    , siffeForwardingEnabled
    , siffeIdentity

    -- * Response
    , SetIdentityFeedbackForwardingEnabledResponse
    -- ** Response constructor
    , setIdentityFeedbackForwardingEnabledResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SES.Types
import qualified GHC.Exts

data SetIdentityFeedbackForwardingEnabled = SetIdentityFeedbackForwardingEnabled
    { _siffeForwardingEnabled :: Bool
    , _siffeIdentity          :: Text
    } deriving (Eq, Ord, Show)

-- | 'SetIdentityFeedbackForwardingEnabled' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'siffeForwardingEnabled' @::@ 'Bool'
--
-- * 'siffeIdentity' @::@ 'Text'
--
setIdentityFeedbackForwardingEnabled :: Text -- ^ 'siffeIdentity'
                                     -> Bool -- ^ 'siffeForwardingEnabled'
                                     -> SetIdentityFeedbackForwardingEnabled
setIdentityFeedbackForwardingEnabled p1 p2 = SetIdentityFeedbackForwardingEnabled
    { _siffeIdentity          = p1
    , _siffeForwardingEnabled = p2
    }

-- | Sets whether Amazon SES will forward bounce and complaint notifications
-- as email. @true@ specifies that Amazon SES will forward bounce and
-- complaint notifications as email, in addition to any Amazon SNS topic
-- publishing otherwise specified. @false@ specifies that Amazon SES will
-- publish bounce and complaint notifications only through Amazon SNS. This
-- value can only be set to @false@ when Amazon SNS topics are set for both
-- @Bounce@ and @Complaint@ notification types.
siffeForwardingEnabled :: Lens' SetIdentityFeedbackForwardingEnabled Bool
siffeForwardingEnabled =
    lens _siffeForwardingEnabled (\s a -> s { _siffeForwardingEnabled = a })

-- | The identity for which to set bounce and complaint notification
-- forwarding. Examples: @user@example.com@, @example.com@.
siffeIdentity :: Lens' SetIdentityFeedbackForwardingEnabled Text
siffeIdentity = lens _siffeIdentity (\s a -> s { _siffeIdentity = a })

data SetIdentityFeedbackForwardingEnabledResponse = SetIdentityFeedbackForwardingEnabledResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'SetIdentityFeedbackForwardingEnabledResponse' constructor.
setIdentityFeedbackForwardingEnabledResponse :: SetIdentityFeedbackForwardingEnabledResponse
setIdentityFeedbackForwardingEnabledResponse = SetIdentityFeedbackForwardingEnabledResponse

instance ToPath SetIdentityFeedbackForwardingEnabled where
    toPath = const "/"

instance ToQuery SetIdentityFeedbackForwardingEnabled where
    toQuery SetIdentityFeedbackForwardingEnabled{..} = mconcat
        [ "ForwardingEnabled" =? _siffeForwardingEnabled
        , "Identity"          =? _siffeIdentity
        ]

instance ToHeaders SetIdentityFeedbackForwardingEnabled

instance AWSRequest SetIdentityFeedbackForwardingEnabled where
    type Sv SetIdentityFeedbackForwardingEnabled = SES
    type Rs SetIdentityFeedbackForwardingEnabled = SetIdentityFeedbackForwardingEnabledResponse

    request  = post "SetIdentityFeedbackForwardingEnabled"
    response = nullResponse SetIdentityFeedbackForwardingEnabledResponse
