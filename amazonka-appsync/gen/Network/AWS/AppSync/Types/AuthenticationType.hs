{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.AuthenticationType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.AuthenticationType
  ( AuthenticationType
      ( ..,
        AuthenticationType_AMAZON_COGNITO_USER_POOLS,
        AuthenticationType_API_KEY,
        AuthenticationType_AWS_IAM,
        AuthenticationType_OPENID_CONNECT
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype AuthenticationType = AuthenticationType'
  { fromAuthenticationType ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern AuthenticationType_AMAZON_COGNITO_USER_POOLS :: AuthenticationType
pattern AuthenticationType_AMAZON_COGNITO_USER_POOLS = AuthenticationType' "AMAZON_COGNITO_USER_POOLS"

pattern AuthenticationType_API_KEY :: AuthenticationType
pattern AuthenticationType_API_KEY = AuthenticationType' "API_KEY"

pattern AuthenticationType_AWS_IAM :: AuthenticationType
pattern AuthenticationType_AWS_IAM = AuthenticationType' "AWS_IAM"

pattern AuthenticationType_OPENID_CONNECT :: AuthenticationType
pattern AuthenticationType_OPENID_CONNECT = AuthenticationType' "OPENID_CONNECT"

{-# COMPLETE
  AuthenticationType_AMAZON_COGNITO_USER_POOLS,
  AuthenticationType_API_KEY,
  AuthenticationType_AWS_IAM,
  AuthenticationType_OPENID_CONNECT,
  AuthenticationType'
  #-}
