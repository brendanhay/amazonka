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

-- Module      : Network.AWS.KMS.CreateGrant
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

-- | Adds a grant to a key to specify who can access the key and under what
-- conditions. Grants are alternate permission mechanisms to key policies. If
-- absent, access to the key is evaluated based on IAM policies attached to the
-- user. By default, grants do not expire. Grants can be listed, retired, or
-- revoked as indicated by the following APIs. Typically, when you are finished
-- using a grant, you retire it. When you want to end a grant immediately,
-- revoke it. For more information about grants, see <http://docs.aws.amazon.com/kms/latest/developerguide/grants.html Grants>.  'ListGrants' 'RetireGrant' 'RevokeGrant'
--
-- <http://docs.aws.amazon.com/kms/latest/APIReference/API_CreateGrant.html>
module Network.AWS.KMS.CreateGrant
    (
    -- * Request
      CreateGrant
    -- ** Request constructor
    , createGrant
    -- ** Request lenses
    , cgConstraints
    , cgGrantTokens
    , cgGranteePrincipal
    , cgKeyId
    , cgOperations
    , cgRetiringPrincipal

    -- * Response
    , CreateGrantResponse
    -- ** Response constructor
    , createGrantResponse
    -- ** Response lenses
    , cgrGrantId
    , cgrGrantToken
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.KMS.Types
import qualified GHC.Exts

data CreateGrant = CreateGrant
    { _cgConstraints       :: Maybe GrantConstraints
    , _cgGrantTokens       :: List "GrantTokens" Text
    , _cgGranteePrincipal  :: Text
    , _cgKeyId             :: Text
    , _cgOperations        :: List "Operations" GrantOperation
    , _cgRetiringPrincipal :: Maybe Text
    } deriving (Eq, Show)

-- | 'CreateGrant' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cgConstraints' @::@ 'Maybe' 'GrantConstraints'
--
-- * 'cgGrantTokens' @::@ ['Text']
--
-- * 'cgGranteePrincipal' @::@ 'Text'
--
-- * 'cgKeyId' @::@ 'Text'
--
-- * 'cgOperations' @::@ ['GrantOperation']
--
-- * 'cgRetiringPrincipal' @::@ 'Maybe' 'Text'
--
createGrant :: Text -- ^ 'cgKeyId'
            -> Text -- ^ 'cgGranteePrincipal'
            -> CreateGrant
createGrant p1 p2 = CreateGrant
    { _cgKeyId             = p1
    , _cgGranteePrincipal  = p2
    , _cgRetiringPrincipal = Nothing
    , _cgOperations        = mempty
    , _cgConstraints       = Nothing
    , _cgGrantTokens       = mempty
    }

-- | Specifies the conditions under which the actions specified by the 'Operations'
-- parameter are allowed.
cgConstraints :: Lens' CreateGrant (Maybe GrantConstraints)
cgConstraints = lens _cgConstraints (\s a -> s { _cgConstraints = a })

-- | List of grant tokens.
cgGrantTokens :: Lens' CreateGrant [Text]
cgGrantTokens = lens _cgGrantTokens (\s a -> s { _cgGrantTokens = a }) . _List

-- | Principal given permission by the grant to use the key identified by the 'keyId'
-- parameter.
cgGranteePrincipal :: Lens' CreateGrant Text
cgGranteePrincipal =
    lens _cgGranteePrincipal (\s a -> s { _cgGranteePrincipal = a })

-- | A unique key identifier for a customer master key. This value can be a
-- globally unique identifier, an ARN, or an alias.
cgKeyId :: Lens' CreateGrant Text
cgKeyId = lens _cgKeyId (\s a -> s { _cgKeyId = a })

-- | List of operations permitted by the grant. This can be any combination of one
-- or more of the following values:  Decrypt Encrypt GenerateDataKey GenerateDataKeyWithoutPlaintext
-- ReEncryptFrom ReEncryptTo CreateGrant
cgOperations :: Lens' CreateGrant [GrantOperation]
cgOperations = lens _cgOperations (\s a -> s { _cgOperations = a }) . _List

-- | Principal given permission to retire the grant. For more information, see 'RetireGrant'.
cgRetiringPrincipal :: Lens' CreateGrant (Maybe Text)
cgRetiringPrincipal =
    lens _cgRetiringPrincipal (\s a -> s { _cgRetiringPrincipal = a })

data CreateGrantResponse = CreateGrantResponse
    { _cgrGrantId    :: Maybe Text
    , _cgrGrantToken :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'CreateGrantResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cgrGrantId' @::@ 'Maybe' 'Text'
--
-- * 'cgrGrantToken' @::@ 'Maybe' 'Text'
--
createGrantResponse :: CreateGrantResponse
createGrantResponse = CreateGrantResponse
    { _cgrGrantToken = Nothing
    , _cgrGrantId    = Nothing
    }

-- | Unique grant identifier. You can use the /GrantId/ value to revoke a grant.
cgrGrantId :: Lens' CreateGrantResponse (Maybe Text)
cgrGrantId = lens _cgrGrantId (\s a -> s { _cgrGrantId = a })

-- | The grant token. A grant token is a string that identifies a grant and which
-- can be used to make a grant take effect immediately. A token contains all of
-- the information necessary to create a grant.
cgrGrantToken :: Lens' CreateGrantResponse (Maybe Text)
cgrGrantToken = lens _cgrGrantToken (\s a -> s { _cgrGrantToken = a })

instance ToPath CreateGrant where
    toPath = const "/"

instance ToQuery CreateGrant where
    toQuery = const mempty

instance ToHeaders CreateGrant

instance ToJSON CreateGrant where
    toJSON CreateGrant{..} = object
        [ "KeyId"             .= _cgKeyId
        , "GranteePrincipal"  .= _cgGranteePrincipal
        , "RetiringPrincipal" .= _cgRetiringPrincipal
        , "Operations"        .= _cgOperations
        , "Constraints"       .= _cgConstraints
        , "GrantTokens"       .= _cgGrantTokens
        ]

instance AWSRequest CreateGrant where
    type Sv CreateGrant = KMS
    type Rs CreateGrant = CreateGrantResponse

    request  = post "CreateGrant"
    response = jsonResponse

instance FromJSON CreateGrantResponse where
    parseJSON = withObject "CreateGrantResponse" $ \o -> CreateGrantResponse
        <$> o .:? "GrantId"
        <*> o .:? "GrantToken"
