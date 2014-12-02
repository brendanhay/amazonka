{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE ViewPatterns                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.STS.Types
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

module Network.AWS.STS.Types
    (
    -- * Service
      STS
    -- ** Error
    , RESTError (..)
    -- ** XML
    , ns

    -- * Credentials
    , Credentials
    , credentials
    , cAccessKeyId
    , cExpiration
    , cSecretAccessKey
    , cSessionToken

    -- * FederatedUser
    , FederatedUser
    , federatedUser
    , fuArn
    , fuFederatedUserId

    -- * AssumedRoleUser
    , AssumedRoleUser
    , assumedRoleUser
    , aruArn
    , aruAssumedRoleId
    ) where

import Network.AWS.Error
import Network.AWS.Prelude
import Network.AWS.Signing
import qualified GHC.Exts

-- | Version @2011-06-15@ of the Amazon Security Token Service service.
data STS

instance AWSService STS where
    type Sg STS = V4
    type Er STS = RESTError

    service = Service
        { _svcAbbrev       = "STS"
        , _svcPrefix       = "sts"
        , _svcVersion      = "2011-06-15"
        , _svcTargetPrefix = Nothing
        , _svcJSONVersion  = Nothing
        , _svcHandle       = restError statusSuccess
        , _svcDelay        = delay
        , _svcRetry        = retry
        }

delay :: Delay
delay = Exp 0.05 2 5
{-# INLINE delay #-}

retry :: AWSErrorCode -> Status -> a -> Retry
retry (statusCode -> s) (awsErrorCode -> e)
    | s == 500  = True -- General Server Error
    | s == 509  = True -- Limit Exceeded
    | s == 503  = True -- Service Unavailable
    | s == 400  = "Throttling" == e -- Throttling
    | otherwise = False
{-# INLINE retry #-}

ns :: Text
ns = "https://sts.amazonaws.com/doc/2011-06-15/"
{-# INLINE ns #-}

data Credentials = Credentials
    { _cAccessKeyId     :: Text
    , _cExpiration      :: ISO8601
    , _cSecretAccessKey :: Text
    , _cSessionToken    :: Text
    } deriving (Eq, Ord, Show)

-- | 'Credentials' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cAccessKeyId' @::@ 'Text'
--
-- * 'cExpiration' @::@ 'UTCTime'
--
-- * 'cSecretAccessKey' @::@ 'Text'
--
-- * 'cSessionToken' @::@ 'Text'
--
credentials :: Text -- ^ 'cAccessKeyId'
            -> Text -- ^ 'cSecretAccessKey'
            -> Text -- ^ 'cSessionToken'
            -> UTCTime -- ^ 'cExpiration'
            -> Credentials
credentials p1 p2 p3 p4 = Credentials
    { _cAccessKeyId     = p1
    , _cSecretAccessKey = p2
    , _cSessionToken    = p3
    , _cExpiration      = withIso _Time (const id) p4
    }

-- | The access key ID that identifies the temporary security credentials.
cAccessKeyId :: Lens' Credentials Text
cAccessKeyId = lens _cAccessKeyId (\s a -> s { _cAccessKeyId = a })

-- | The date on which the current credentials expire.
cExpiration :: Lens' Credentials UTCTime
cExpiration = lens _cExpiration (\s a -> s { _cExpiration = a }) . _Time

-- | The secret access key that can be used to sign requests.
cSecretAccessKey :: Lens' Credentials Text
cSecretAccessKey = lens _cSecretAccessKey (\s a -> s { _cSecretAccessKey = a })

-- | The token that users must pass to the service API to use the temporary
-- credentials.
cSessionToken :: Lens' Credentials Text
cSessionToken = lens _cSessionToken (\s a -> s { _cSessionToken = a })

instance FromXML Credentials where
    parseXML x = Credentials
        <$> x .@  "AccessKeyId"
        <*> x .@  "Expiration"
        <*> x .@  "SecretAccessKey"
        <*> x .@  "SessionToken"

instance ToQuery Credentials where
    toQuery Credentials{..} = mconcat
        [ "AccessKeyId"     =? _cAccessKeyId
        , "Expiration"      =? _cExpiration
        , "SecretAccessKey" =? _cSecretAccessKey
        , "SessionToken"    =? _cSessionToken
        ]

data FederatedUser = FederatedUser
    { _fuArn             :: Text
    , _fuFederatedUserId :: Text
    } deriving (Eq, Ord, Show)

-- | 'FederatedUser' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'fuArn' @::@ 'Text'
--
-- * 'fuFederatedUserId' @::@ 'Text'
--
federatedUser :: Text -- ^ 'fuFederatedUserId'
              -> Text -- ^ 'fuArn'
              -> FederatedUser
federatedUser p1 p2 = FederatedUser
    { _fuFederatedUserId = p1
    , _fuArn             = p2
    }

-- | The ARN that specifies the federated user that is associated with the
-- credentials. For more information about ARNs and how to use them in policies,
-- see Identifiers for IAM Entities in /Using IAM/.
fuArn :: Lens' FederatedUser Text
fuArn = lens _fuArn (\s a -> s { _fuArn = a })

-- | The string that identifies the federated user associated with the
-- credentials, similar to the unique ID of an IAM user.
fuFederatedUserId :: Lens' FederatedUser Text
fuFederatedUserId =
    lens _fuFederatedUserId (\s a -> s { _fuFederatedUserId = a })

instance FromXML FederatedUser where
    parseXML x = FederatedUser
        <$> x .@  "Arn"
        <*> x .@  "FederatedUserId"

instance ToQuery FederatedUser where
    toQuery FederatedUser{..} = mconcat
        [ "Arn"             =? _fuArn
        , "FederatedUserId" =? _fuFederatedUserId
        ]

data AssumedRoleUser = AssumedRoleUser
    { _aruArn           :: Text
    , _aruAssumedRoleId :: Text
    } deriving (Eq, Ord, Show)

-- | 'AssumedRoleUser' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aruArn' @::@ 'Text'
--
-- * 'aruAssumedRoleId' @::@ 'Text'
--
assumedRoleUser :: Text -- ^ 'aruAssumedRoleId'
                -> Text -- ^ 'aruArn'
                -> AssumedRoleUser
assumedRoleUser p1 p2 = AssumedRoleUser
    { _aruAssumedRoleId = p1
    , _aruArn           = p2
    }

-- | The ARN of the temporary security credentials that are returned from the 'AssumeRole' action. For more information about ARNs and how to use them in policies, see
-- Identifiers for IAM Entities  in /Using IAM/.
aruArn :: Lens' AssumedRoleUser Text
aruArn = lens _aruArn (\s a -> s { _aruArn = a })

-- | A unique identifier that contains the role ID and the role session name of
-- the role that is being assumed. The role ID is generated by AWS when the role
-- is created.
aruAssumedRoleId :: Lens' AssumedRoleUser Text
aruAssumedRoleId = lens _aruAssumedRoleId (\s a -> s { _aruAssumedRoleId = a })

instance FromXML AssumedRoleUser where
    parseXML x = AssumedRoleUser
        <$> x .@  "Arn"
        <*> x .@  "AssumedRoleId"

instance ToQuery AssumedRoleUser where
    toQuery AssumedRoleUser{..} = mconcat
        [ "Arn"           =? _aruArn
        , "AssumedRoleId" =? _aruAssumedRoleId
        ]
