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

-- Module      : Network.AWS.KMS.RetireGrant
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

-- | Retires a grant. You can retire a grant when you're done using it to clean
-- up. You should revoke a grant when you intend to actively deny operations
-- that depend on it. The following are permitted to call this API:  The account
-- that created the grant The 'RetiringPrincipal', if present The 'GranteePrincipal', if
-- 'RetireGrant' is a grantee operation  The grant to retire must be identified by
-- its grant token or by a combination of the key ARN and the grant ID. A grant
-- token is a unique variable-length base64-encoded string. A grant ID is a 64
-- character unique identifier of a grant. Both are returned by the 'CreateGrant'
-- function.
--
-- <http://docs.aws.amazon.com/kms/latest/APIReference/API_RetireGrant.html>
module Network.AWS.KMS.RetireGrant
    (
    -- * Request
      RetireGrant
    -- ** Request constructor
    , retireGrant
    -- ** Request lenses
    , rgGrantId
    , rgGrantToken
    , rgKeyId

    -- * Response
    , RetireGrantResponse
    -- ** Response constructor
    , retireGrantResponse
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.KMS.Types
import qualified GHC.Exts

data RetireGrant = RetireGrant
    { _rgGrantId    :: Maybe Text
    , _rgGrantToken :: Maybe Text
    , _rgKeyId      :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'RetireGrant' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rgGrantId' @::@ 'Maybe' 'Text'
--
-- * 'rgGrantToken' @::@ 'Maybe' 'Text'
--
-- * 'rgKeyId' @::@ 'Maybe' 'Text'
--
retireGrant :: RetireGrant
retireGrant = RetireGrant
    { _rgGrantToken = Nothing
    , _rgKeyId      = Nothing
    , _rgGrantId    = Nothing
    }

-- | Unique identifier of the grant to be retired. The grant ID is returned by
-- the 'CreateGrant' function.  Grant ID Example -
-- 0123456789012345678901234567890123456789012345678901234567890123
rgGrantId :: Lens' RetireGrant (Maybe Text)
rgGrantId = lens _rgGrantId (\s a -> s { _rgGrantId = a })

-- | Token that identifies the grant to be retired.
rgGrantToken :: Lens' RetireGrant (Maybe Text)
rgGrantToken = lens _rgGrantToken (\s a -> s { _rgGrantToken = a })

-- | A unique identifier for the customer master key associated with the grant.
-- This value can be a globally unique identifier or a fully specified ARN of
-- the key.  Key ARN Example -
-- arn:aws:kms:us-east-1:123456789012:key/12345678-1234-1234-1234-123456789012 Globally Unique Key ID Example - 12345678-1234-1234-1234-123456789012
--
rgKeyId :: Lens' RetireGrant (Maybe Text)
rgKeyId = lens _rgKeyId (\s a -> s { _rgKeyId = a })

data RetireGrantResponse = RetireGrantResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'RetireGrantResponse' constructor.
retireGrantResponse :: RetireGrantResponse
retireGrantResponse = RetireGrantResponse

instance ToPath RetireGrant where
    toPath = const "/"

instance ToQuery RetireGrant where
    toQuery = const mempty

instance ToHeaders RetireGrant

instance ToJSON RetireGrant where
    toJSON RetireGrant{..} = object
        [ "GrantToken" .= _rgGrantToken
        , "KeyId"      .= _rgKeyId
        , "GrantId"    .= _rgGrantId
        ]

instance AWSRequest RetireGrant where
    type Sv RetireGrant = KMS
    type Rs RetireGrant = RetireGrantResponse

    request  = post "RetireGrant"
    response = nullResponse RetireGrantResponse
