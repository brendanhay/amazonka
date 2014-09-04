{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SES.V2010_12_01.VerifyEmailIdentity
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
-- request per second. POST / HTTP/1.1 Date: Sat, 12 May 2012 05:21:58 GMT
-- Host: email.us-east-1.amazonaws.com Content-Type:
-- application/x-www-form-urlencoded X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,
-- Signature=hQJj2pxypqJHQgU/BW1EZGUiNIYGhkQDf7tI6UgQ2qw=,
-- Algorithm=HmacSHA256, SignedHeaders=Date;Host Content-Length: 151
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE &Action=VerifyEmailIdentity
-- &EmailAddress=user%40domain.com &Timestamp=2012-05-12T05%3A21%3A58.000Z
-- &Version=2010-12-01 47e0ef1a-9bf2-11e1-9279-0100e8cf109a.
module Network.AWS.SES.V2010_12_01.VerifyEmailIdentity
    (
    -- * Request
      VerifyEmailIdentity
    -- ** Request constructor
    , mkVerifyEmailIdentityRequest
    -- ** Request lenses
    , veirEmailAddress

    -- * Response
    , VerifyEmailIdentityResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.SES.V2010_12_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'VerifyEmailIdentity' request.
mkVerifyEmailIdentityRequest :: Text -- ^ 'veirEmailAddress'
                             -> VerifyEmailIdentity
mkVerifyEmailIdentityRequest p1 = VerifyEmailIdentity
    { _veirEmailAddress = p1
    }
{-# INLINE mkVerifyEmailIdentityRequest #-}

newtype VerifyEmailIdentity = VerifyEmailIdentity
    { _veirEmailAddress :: Text
      -- ^ The email address to be verified.
    } deriving (Show, Generic)

-- | The email address to be verified.
veirEmailAddress :: Lens' VerifyEmailIdentity (Text)
veirEmailAddress = lens _veirEmailAddress (\s a -> s { _veirEmailAddress = a })
{-# INLINE veirEmailAddress #-}

instance ToQuery VerifyEmailIdentity where
    toQuery = genericQuery def

    deriving (Eq, Show, Generic)

instance AWSRequest VerifyEmailIdentity where
    type Sv VerifyEmailIdentity = SES
    type Rs VerifyEmailIdentity = VerifyEmailIdentityResponse

    request = post "VerifyEmailIdentity"
    response _ = nullaryResponse VerifyEmailIdentityResponse
