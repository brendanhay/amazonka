{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SES.DeleteVerifiedEmailAddress
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified email address from the list of verified addresses.
-- The DeleteVerifiedEmailAddress action is deprecated as of the May 15, 2012
-- release of Domain Verification. The DeleteIdentity action is now preferred.
-- This action is throttled at one request per second. POST / HTTP/1.1 Date:
-- Thu, 18 Aug 2011 22:20:50 GMT Host: email.us-east-1.amazonaws.com
-- Content-Type: application/x-www-form-urlencoded X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,
-- Signature=Rxzyd6cQe/YjkV4yoQAZ243OzzNjFgrsclizTKwRIRc=,
-- Algorithm=HmacSHA256, SignedHeaders=Date;Host Content-Length: 142
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE &Action=DeleteVerifiedEmailAddress
-- &EmailAddress=user%40example.com &Timestamp=2011-08-18T22%3A20%3A50.000Z
-- 5634af08-c865-11e0-8986-3f99a698f914.
module Network.AWS.SES.DeleteVerifiedEmailAddress
    (
    -- * Request
      DeleteVerifiedEmailAddress
    -- ** Request constructor
    , deleteVerifiedEmailAddress
    -- ** Request lenses
    , dveaEmailAddress

    -- * Response
    , DeleteVerifiedEmailAddressResponse
    -- ** Response constructor
    , deleteVerifiedEmailAddressResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.SES.Types
import Network.AWS.Prelude

-- | Represents a request instructing the service to delete an address from the
-- list of verified email addresses.
newtype DeleteVerifiedEmailAddress = DeleteVerifiedEmailAddress
    { _dveaEmailAddress :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteVerifiedEmailAddress' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @EmailAddress ::@ @Text@
--
deleteVerifiedEmailAddress :: Text -- ^ 'dveaEmailAddress'
                           -> DeleteVerifiedEmailAddress
deleteVerifiedEmailAddress p1 = DeleteVerifiedEmailAddress
    { _dveaEmailAddress = p1
    }

-- | An email address to be removed from the list of verified addresses.
dveaEmailAddress :: Lens' DeleteVerifiedEmailAddress Text
dveaEmailAddress =
    lens _dveaEmailAddress (\s a -> s { _dveaEmailAddress = a })

instance ToQuery DeleteVerifiedEmailAddress where
    toQuery = genericQuery def

data DeleteVerifiedEmailAddressResponse = DeleteVerifiedEmailAddressResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteVerifiedEmailAddressResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
deleteVerifiedEmailAddressResponse :: DeleteVerifiedEmailAddressResponse
deleteVerifiedEmailAddressResponse = DeleteVerifiedEmailAddressResponse

instance AWSRequest DeleteVerifiedEmailAddress where
    type Sv DeleteVerifiedEmailAddress = SES
    type Rs DeleteVerifiedEmailAddress = DeleteVerifiedEmailAddressResponse

    request = post "DeleteVerifiedEmailAddress"
    response _ = nullaryResponse DeleteVerifiedEmailAddressResponse
