{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.AuthenticationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppSync.Types.AuthenticationType
  ( AuthenticationType
    ( AuthenticationType'
    , AuthenticationTypeApiKey
    , AuthenticationTypeAwsIam
    , AuthenticationTypeAmazonCognitoUserPools
    , AuthenticationTypeOpenidConnect
    , fromAuthenticationType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype AuthenticationType = AuthenticationType'{fromAuthenticationType
                                                 :: Core.Text}
                               deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                               Core.Generic)
                               deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                 Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                 Core.FromJSON, Core.ToXML, Core.FromXML,
                                                 Core.ToText, Core.FromText, Core.ToByteString,
                                                 Core.ToQuery, Core.ToHeader)

pattern AuthenticationTypeApiKey :: AuthenticationType
pattern AuthenticationTypeApiKey = AuthenticationType' "API_KEY"

pattern AuthenticationTypeAwsIam :: AuthenticationType
pattern AuthenticationTypeAwsIam = AuthenticationType' "AWS_IAM"

pattern AuthenticationTypeAmazonCognitoUserPools :: AuthenticationType
pattern AuthenticationTypeAmazonCognitoUserPools = AuthenticationType' "AMAZON_COGNITO_USER_POOLS"

pattern AuthenticationTypeOpenidConnect :: AuthenticationType
pattern AuthenticationTypeOpenidConnect = AuthenticationType' "OPENID_CONNECT"

{-# COMPLETE 
  AuthenticationTypeApiKey,

  AuthenticationTypeAwsIam,

  AuthenticationTypeAmazonCognitoUserPools,

  AuthenticationTypeOpenidConnect,
  AuthenticationType'
  #-}
