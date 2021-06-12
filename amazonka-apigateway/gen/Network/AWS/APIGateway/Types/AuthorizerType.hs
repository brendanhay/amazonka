{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.AuthorizerType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.AuthorizerType
  ( AuthorizerType
      ( ..,
        AuthorizerType_COGNITO_USER_POOLS,
        AuthorizerType_REQUEST,
        AuthorizerType_TOKEN
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | The authorizer type. Valid values are @TOKEN@ for a Lambda function
-- using a single authorization token submitted in a custom header,
-- @REQUEST@ for a Lambda function using incoming request parameters, and
-- @COGNITO_USER_POOLS@ for using an Amazon Cognito user pool.
newtype AuthorizerType = AuthorizerType'
  { fromAuthorizerType ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern AuthorizerType_COGNITO_USER_POOLS :: AuthorizerType
pattern AuthorizerType_COGNITO_USER_POOLS = AuthorizerType' "COGNITO_USER_POOLS"

pattern AuthorizerType_REQUEST :: AuthorizerType
pattern AuthorizerType_REQUEST = AuthorizerType' "REQUEST"

pattern AuthorizerType_TOKEN :: AuthorizerType
pattern AuthorizerType_TOKEN = AuthorizerType' "TOKEN"

{-# COMPLETE
  AuthorizerType_COGNITO_USER_POOLS,
  AuthorizerType_REQUEST,
  AuthorizerType_TOKEN,
  AuthorizerType'
  #-}
