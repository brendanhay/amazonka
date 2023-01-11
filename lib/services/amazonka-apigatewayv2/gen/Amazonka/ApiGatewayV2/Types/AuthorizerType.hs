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
-- Module      : Amazonka.ApiGatewayV2.Types.AuthorizerType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ApiGatewayV2.Types.AuthorizerType
  ( AuthorizerType
      ( ..,
        AuthorizerType_JWT,
        AuthorizerType_REQUEST
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The authorizer type. Specify REQUEST for a Lambda function using
-- incoming request parameters. Specify JWT to use JSON Web Tokens
-- (supported only for HTTP APIs).
newtype AuthorizerType = AuthorizerType'
  { fromAuthorizerType ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern AuthorizerType_JWT :: AuthorizerType
pattern AuthorizerType_JWT = AuthorizerType' "JWT"

pattern AuthorizerType_REQUEST :: AuthorizerType
pattern AuthorizerType_REQUEST = AuthorizerType' "REQUEST"

{-# COMPLETE
  AuthorizerType_JWT,
  AuthorizerType_REQUEST,
  AuthorizerType'
  #-}
