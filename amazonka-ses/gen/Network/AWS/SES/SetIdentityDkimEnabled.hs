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

-- Module      : Network.AWS.SES.SetIdentityDkimEnabled
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Enables or disables Easy DKIM signing of email sent from an identity:
--
-- If Easy DKIM signing is enabled for a domain name identity (e.g., 'example.com'), then Amazon SES will DKIM-sign all email sent by addresses under that
-- domain name (e.g., 'user@example.com'). If Easy DKIM signing is enabled for an
-- email address, then Amazon SES will DKIM-sign all email sent by that email
-- address.  For email addresses (e.g., 'user@example.com'), you can only enable
-- Easy DKIM signing if the corresponding domain (e.g., 'example.com') has been
-- set up for Easy DKIM using the AWS Console or the 'VerifyDomainDkim' action.
--
-- This action is throttled at one request per second.
--
-- For more information about Easy DKIM signing, go to the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim.html Amazon SES DeveloperGuide>.
--
-- <http://docs.aws.amazon.com/ses/latest/APIReference/API_SetIdentityDkimEnabled.html>
module Network.AWS.SES.SetIdentityDkimEnabled
    (
    -- * Request
      SetIdentityDkimEnabled
    -- ** Request constructor
    , setIdentityDkimEnabled
    -- ** Request lenses
    , sideDkimEnabled
    , sideIdentity

    -- * Response
    , SetIdentityDkimEnabledResponse
    -- ** Response constructor
    , setIdentityDkimEnabledResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SES.Types
import qualified GHC.Exts

data SetIdentityDkimEnabled = SetIdentityDkimEnabled
    { _sideDkimEnabled :: Bool
    , _sideIdentity    :: Text
    } deriving (Eq, Ord, Show)

-- | 'SetIdentityDkimEnabled' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sideDkimEnabled' @::@ 'Bool'
--
-- * 'sideIdentity' @::@ 'Text'
--
setIdentityDkimEnabled :: Text -- ^ 'sideIdentity'
                       -> Bool -- ^ 'sideDkimEnabled'
                       -> SetIdentityDkimEnabled
setIdentityDkimEnabled p1 p2 = SetIdentityDkimEnabled
    { _sideIdentity    = p1
    , _sideDkimEnabled = p2
    }

-- | Sets whether DKIM signing is enabled for an identity. Set to 'true' to enable
-- DKIM signing for this identity; 'false' to disable it.
sideDkimEnabled :: Lens' SetIdentityDkimEnabled Bool
sideDkimEnabled = lens _sideDkimEnabled (\s a -> s { _sideDkimEnabled = a })

-- | The identity for which DKIM signing should be enabled or disabled.
sideIdentity :: Lens' SetIdentityDkimEnabled Text
sideIdentity = lens _sideIdentity (\s a -> s { _sideIdentity = a })

data SetIdentityDkimEnabledResponse = SetIdentityDkimEnabledResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'SetIdentityDkimEnabledResponse' constructor.
setIdentityDkimEnabledResponse :: SetIdentityDkimEnabledResponse
setIdentityDkimEnabledResponse = SetIdentityDkimEnabledResponse

instance ToPath SetIdentityDkimEnabled where
    toPath = const "/"

instance ToQuery SetIdentityDkimEnabled where
    toQuery SetIdentityDkimEnabled{..} = mconcat
        [ "DkimEnabled" =? _sideDkimEnabled
        , "Identity"    =? _sideIdentity
        ]

instance ToHeaders SetIdentityDkimEnabled

instance AWSRequest SetIdentityDkimEnabled where
    type Sv SetIdentityDkimEnabled = SES
    type Rs SetIdentityDkimEnabled = SetIdentityDkimEnabledResponse

    request  = post "SetIdentityDkimEnabled"
    response = nullResponse SetIdentityDkimEnabledResponse
