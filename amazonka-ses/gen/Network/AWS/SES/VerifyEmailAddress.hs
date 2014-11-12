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

-- Module      : Network.AWS.SES.VerifyEmailAddress
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Verifies an email address. This action causes a confirmation email message
-- to be sent to the specified address. The VerifyEmailAddress action is
-- deprecated as of the May 15, 2012 release of Domain Verification. The
-- VerifyEmailIdentity action is now preferred. This action is throttled at
-- one request per second.
module Network.AWS.SES.VerifyEmailAddress
    (
    -- * Request
      VerifyEmailAddress
    -- ** Request constructor
    , verifyEmailAddress
    -- ** Request lenses
    , veaEmailAddress

    -- * Response
    , VerifyEmailAddressResponse
    -- ** Response constructor
    , verifyEmailAddressResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SES.Types

newtype VerifyEmailAddress = VerifyEmailAddress
    { _veaEmailAddress :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'VerifyEmailAddress' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'veaEmailAddress' @::@ 'Text'
--
verifyEmailAddress :: Text -- ^ 'veaEmailAddress'
                   -> VerifyEmailAddress
verifyEmailAddress p1 = VerifyEmailAddress
    { _veaEmailAddress = p1
    }

-- | The email address to be verified.
veaEmailAddress :: Lens' VerifyEmailAddress Text
veaEmailAddress = lens _veaEmailAddress (\s a -> s { _veaEmailAddress = a })

instance ToQuery VerifyEmailAddress

instance ToPath VerifyEmailAddress where
    toPath = const "/"

data VerifyEmailAddressResponse = VerifyEmailAddressResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'VerifyEmailAddressResponse' constructor.
verifyEmailAddressResponse :: VerifyEmailAddressResponse
verifyEmailAddressResponse = VerifyEmailAddressResponse

instance FromXML VerifyEmailAddressResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "VerifyEmailAddressResponse"

instance AWSRequest VerifyEmailAddress where
    type Sv VerifyEmailAddress = SES
    type Rs VerifyEmailAddress = VerifyEmailAddressResponse

    request  = post "VerifyEmailAddress"
    response = nullaryResponse VerifyEmailAddressResponse
