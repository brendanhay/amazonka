{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SES.VerifyEmailIdentity
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Verifies an email address. This action causes a confirmation email message
-- to be sent to the specified address. This action is throttled at one
-- request per second.
--
-- <http://docs.aws.amazon.com/ses/latest/APIReference/API_VerifyEmailIdentity.html>
module Network.AWS.SES.VerifyEmailIdentity
    (
    -- * Request
      VerifyEmailIdentity
    -- ** Request constructor
    , verifyEmailIdentity
    -- ** Request lenses
    , veiEmailAddress

    -- * Response
    , VerifyEmailIdentityResponse
    -- ** Response constructor
    , verifyEmailIdentityResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SES.Types
import qualified GHC.Exts

newtype VerifyEmailIdentity = VerifyEmailIdentity
    { _veiEmailAddress :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'VerifyEmailIdentity' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'veiEmailAddress' @::@ 'Text'
--
verifyEmailIdentity :: Text -- ^ 'veiEmailAddress'
                    -> VerifyEmailIdentity
verifyEmailIdentity p1 = VerifyEmailIdentity
    { _veiEmailAddress = p1
    }

-- | The email address to be verified.
veiEmailAddress :: Lens' VerifyEmailIdentity Text
veiEmailAddress = lens _veiEmailAddress (\s a -> s { _veiEmailAddress = a })

data VerifyEmailIdentityResponse = VerifyEmailIdentityResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'VerifyEmailIdentityResponse' constructor.
verifyEmailIdentityResponse :: VerifyEmailIdentityResponse
verifyEmailIdentityResponse = VerifyEmailIdentityResponse

instance ToPath VerifyEmailIdentity where
    toPath = const "/"

instance ToQuery VerifyEmailIdentity

instance ToHeaders VerifyEmailIdentity

instance AWSRequest VerifyEmailIdentity where
    type Sv VerifyEmailIdentity = SES
    type Rs VerifyEmailIdentity = VerifyEmailIdentityResponse

    request  = post "VerifyEmailIdentity"
    response = nullResponse VerifyEmailIdentityResponse
