{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.AuthorizerType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.AuthorizerType
  ( AuthorizerType
      ( AuthorizerType',
        AuthorizerToken,
        AuthorizerRequest,
        AuthorizerCognitoUserPools
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | The authorizer type. Valid values are @TOKEN@ for a Lambda function using a single authorization token submitted in a custom header, @REQUEST@ for a Lambda function using incoming request parameters, and @COGNITO_USER_POOLS@ for using an Amazon Cognito user pool.
newtype AuthorizerType = AuthorizerType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern AuthorizerToken :: AuthorizerType
pattern AuthorizerToken = AuthorizerType' "TOKEN"

pattern AuthorizerRequest :: AuthorizerType
pattern AuthorizerRequest = AuthorizerType' "REQUEST"

pattern AuthorizerCognitoUserPools :: AuthorizerType
pattern AuthorizerCognitoUserPools = AuthorizerType' "COGNITO_USER_POOLS"

{-# COMPLETE
  AuthorizerToken,
  AuthorizerRequest,
  AuthorizerCognitoUserPools,
  AuthorizerType'
  #-}
