{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.Types.AuthorizerType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ApiGateway.Types.AuthorizerType
  ( AuthorizerType
    ( AuthorizerType'
    , AuthorizerTypeAuthorizerToken
    , AuthorizerTypeAuthorizerRequest
    , AuthorizerTypeAuthorizerCognitoUserPools
    , fromAuthorizerType
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | The authorizer type. Valid values are @TOKEN@ for a Lambda function using a single authorization token submitted in a custom header, @REQUEST@ for a Lambda function using incoming request parameters, and @COGNITO_USER_POOLS@ for using an Amazon Cognito user pool.
newtype AuthorizerType = AuthorizerType'{fromAuthorizerType ::
                                         Core.Text}
                           deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                           Core.Generic)
                           deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                             Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                             Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                             Core.FromText, Core.ToByteString, Core.ToQuery,
                                             Core.ToHeader)

pattern AuthorizerTypeAuthorizerToken :: AuthorizerType
pattern AuthorizerTypeAuthorizerToken = AuthorizerType' "TOKEN"

pattern AuthorizerTypeAuthorizerRequest :: AuthorizerType
pattern AuthorizerTypeAuthorizerRequest = AuthorizerType' "REQUEST"

pattern AuthorizerTypeAuthorizerCognitoUserPools :: AuthorizerType
pattern AuthorizerTypeAuthorizerCognitoUserPools = AuthorizerType' "COGNITO_USER_POOLS"

{-# COMPLETE 
  AuthorizerTypeAuthorizerToken,

  AuthorizerTypeAuthorizerRequest,

  AuthorizerTypeAuthorizerCognitoUserPools,
  AuthorizerType'
  #-}
