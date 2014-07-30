{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SES.V2010_12_01.VerifyEmailAddress
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
-- one request per second. POST / HTTP/1.1 Date: Thu, 18 Aug 2011 22:28:27 GMT
-- Host: email.us-east-1.amazonaws.com Content-Type:
-- application/x-www-form-urlencoded X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,
-- Signature=o9NK68jraFg5BnaTQiQhpxj2x1dGONOEFHHgsM6o5as=,
-- Algorithm=HmacSHA256, SignedHeaders=Date;Host Content-Length: 132
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE &Action=VerifyEmailAddress
-- &EmailAddress=user%40example.com &Timestamp=2011-08-18T22%3A28%3A27.000Z
-- 8edd7eb2-c864-11e0-9f8f-3da8fc215a7e.
module Network.AWS.SES.V2010_12_01.VerifyEmailAddress where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Error, Endpoint, Region)
import           Network.AWS.Request.Query
import           Network.AWS.SES.V2010_12_01.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

data VerifyEmailAddress = VerifyEmailAddress
    { _vearEmailAddress :: Text
      -- ^ The email address to be verified.
    } deriving (Generic)

instance ToQuery VerifyEmailAddress where
    toQuery = genericToQuery def

instance AWSRequest VerifyEmailAddress where
    type Sv VerifyEmailAddress = SES
    type Rs VerifyEmailAddress = VerifyEmailAddressResponse

    request = post "VerifyEmailAddress"
    response _ _ = return (Right VerifyEmailAddressResponse)

data VerifyEmailAddressResponse = VerifyEmailAddressResponse
    deriving (Eq, Show, Generic)
