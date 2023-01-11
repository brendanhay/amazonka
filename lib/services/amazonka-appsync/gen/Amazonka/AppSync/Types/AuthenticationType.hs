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
-- Module      : Amazonka.AppSync.Types.AuthenticationType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppSync.Types.AuthenticationType
  ( AuthenticationType
      ( ..,
        AuthenticationType_AMAZON_COGNITO_USER_POOLS,
        AuthenticationType_API_KEY,
        AuthenticationType_AWS_IAM,
        AuthenticationType_AWS_LAMBDA,
        AuthenticationType_OPENID_CONNECT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AuthenticationType = AuthenticationType'
  { fromAuthenticationType ::
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

pattern AuthenticationType_AMAZON_COGNITO_USER_POOLS :: AuthenticationType
pattern AuthenticationType_AMAZON_COGNITO_USER_POOLS = AuthenticationType' "AMAZON_COGNITO_USER_POOLS"

pattern AuthenticationType_API_KEY :: AuthenticationType
pattern AuthenticationType_API_KEY = AuthenticationType' "API_KEY"

pattern AuthenticationType_AWS_IAM :: AuthenticationType
pattern AuthenticationType_AWS_IAM = AuthenticationType' "AWS_IAM"

pattern AuthenticationType_AWS_LAMBDA :: AuthenticationType
pattern AuthenticationType_AWS_LAMBDA = AuthenticationType' "AWS_LAMBDA"

pattern AuthenticationType_OPENID_CONNECT :: AuthenticationType
pattern AuthenticationType_OPENID_CONNECT = AuthenticationType' "OPENID_CONNECT"

{-# COMPLETE
  AuthenticationType_AMAZON_COGNITO_USER_POOLS,
  AuthenticationType_API_KEY,
  AuthenticationType_AWS_IAM,
  AuthenticationType_AWS_LAMBDA,
  AuthenticationType_OPENID_CONNECT,
  AuthenticationType'
  #-}
