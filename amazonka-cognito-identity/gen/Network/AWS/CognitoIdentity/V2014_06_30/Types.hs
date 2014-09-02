{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CognitoIdentity.V2014_06_30.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Amazon Cognito is a web service that facilitates the delivery of scoped,
-- temporary credentials to mobile devices or other untrusted environments.
-- Amazon Cognito uniquely identifies a device or user and supplies the user
-- with a consistent identity throughout the lifetime of an application.
-- Amazon Cognito lets users authenticate with third-party identity providers
-- (Facebook, Google, or Login with Amazon). As a developer, you decide which
-- identity providers to trust. You can also choose to support unauthenticated
-- access from your application. Your users are provided with Cognito tokens
-- that uniquely identify their device and any information provided about
-- third-party logins.
module Network.AWS.CognitoIdentity.V2014_06_30.Types
    ( module Network.AWS.CognitoIdentity.V2014_06_30.Types
    ) where

import Network.AWS.Prelude
import Network.AWS.Signing.V4

-- | Supported version (@2014-06-30@) of the
-- @Amazon Cognito Identity@ service.
data CognitoIdentity deriving (Typeable)

instance AWSService CognitoIdentity where
    type Sg CognitoIdentity = V4
    data Er CognitoIdentity
        = CognitoIdentityClient HttpException
        | CognitoIdentitySerializer String
        | CognitoIdentityService String
        | InternalErrorException
            { _ieeMessage :: Maybe Text
            }
        | InvalidParameterException
            { _ipeMessage :: Maybe Text
            }
        | LimitExceededException
            { _leeMessage :: Maybe Text
            }
        | NotAuthorizedException
            { _naeMessage :: Maybe Text
            }
        | ResourceConflictException
            { _rceMessage :: Maybe Text
            }
        | ResourceNotFoundException
            { _rnfeMessage :: Maybe Text
            }
        | TooManyRequestsException
            { _tmreMessage :: Maybe Text
            }

    service = Service'
        { _svcEndpoint = Regional
        , _svcPrefix   = "cognito-identity"
        , _svcVersion  = "2014-06-30"
        , _svcTarget   = Nothing
        }

deriving instance Show    (Er CognitoIdentity)
deriving instance Generic (Er CognitoIdentity)

instance AWSError (Er CognitoIdentity) where
    awsError = const "CognitoIdentityError"

instance AWSServiceError (Er CognitoIdentity) where
    serviceError    = CognitoIdentityService
    clientError     = CognitoIdentityClient
    serializerError = CognitoIdentitySerializer

instance Exception (Er CognitoIdentity)

-- | A description of the identity.
data IdentityDescription = IdentityDescription
    { _idIdentityId :: Maybe Text
      -- ^ A unique identifier in the format REGION:GUID.
    , _idLogins :: [Text]
      -- ^ A set of optional name/value pairs that map provider names to
      -- provider tokens.
    } deriving (Show, Generic)

instance FromJSON IdentityDescription

-- | A description of the identity pool.
data IdentityPoolShortDescription = IdentityPoolShortDescription
    { _ipsdIdentityPoolId :: Maybe Text
      -- ^ An identity pool ID in the format REGION:GUID.
    , _ipsdIdentityPoolName :: Maybe Text
      -- ^ A string that you provide.
    } deriving (Show, Generic)

instance FromJSON IdentityPoolShortDescription

makeLenses ''IdentityDescription
makeLenses ''IdentityPoolShortDescription
