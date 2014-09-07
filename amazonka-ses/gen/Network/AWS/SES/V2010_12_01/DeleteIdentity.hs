{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SES.V2010_12_01.DeleteIdentity
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified identity (email address or domain) from the list of
-- verified identities. This action is throttled at one request per second.
-- POST / HTTP/1.1 Date: Sat, 12 May 2012 05:25:58 GMT Host:
-- email.us-east-1.amazonaws.com Content-Type:
-- application/x-www-form-urlencoded X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,
-- Signature=w943pl3zIvtszwzZxypi+LsgjzquvhYhnG42S6b2WLo=,
-- Algorithm=HmacSHA256, SignedHeaders=Date;Host Content-Length: 135
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE &Action=DeleteIdentity
-- &Identity=domain.com &Timestamp=2012-05-12T05%3A25%3A58.000Z
-- &Version=2010-12-01 d96bd874-9bf2-11e1-8ee7-c98a0037a2b6.
module Network.AWS.SES.V2010_12_01.DeleteIdentity
    (
    -- * Request
      DeleteIdentity
    -- ** Request constructor
    , mkDeleteIdentity
    -- ** Request lenses
    , diIdentity

    -- * Response
    , DeleteIdentityResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.SES.V2010_12_01.Types
import Network.AWS.Prelude

-- | Represents a request instructing the service to delete an identity from the
-- list of identities for the AWS Account.
newtype DeleteIdentity = DeleteIdentity
    { _diIdentity :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteIdentity' request.
mkDeleteIdentity :: Text -- ^ 'diIdentity'
                 -> DeleteIdentity
mkDeleteIdentity p1 = DeleteIdentity
    { _diIdentity = p1
    }

-- | The identity to be removed from the list of identities for the AWS Account.
diIdentity :: Lens' DeleteIdentity Text
diIdentity = lens _diIdentity (\s a -> s { _diIdentity = a })

instance ToQuery DeleteIdentity where
    toQuery = genericQuery def

-- | An empty element. Receiving this element indicates that the request
-- completed successfully.
data DeleteIdentityResponse = DeleteIdentityResponse
    deriving (Eq, Show, Generic)

instance AWSRequest DeleteIdentity where
    type Sv DeleteIdentity = SES
    type Rs DeleteIdentity = DeleteIdentityResponse

    request = post "DeleteIdentity"
    response _ = nullaryResponse DeleteIdentityResponse
