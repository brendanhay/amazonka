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
-- Module      : Network.AWS.ApiGatewayV2.Types.AuthorizationType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ApiGatewayV2.Types.AuthorizationType
  ( AuthorizationType
      ( ..,
        AuthorizationType_AWS_IAM,
        AuthorizationType_CUSTOM,
        AuthorizationType_JWT,
        AuthorizationType_NONE
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | The authorization type. For WebSocket APIs, valid values are NONE for
-- open access, AWS_IAM for using AWS IAM permissions, and CUSTOM for using
-- a Lambda authorizer. For HTTP APIs, valid values are NONE for open
-- access, JWT for using JSON Web Tokens, AWS_IAM for using AWS IAM
-- permissions, and CUSTOM for using a Lambda authorizer.
newtype AuthorizationType = AuthorizationType'
  { fromAuthorizationType ::
      Core.Text
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

pattern AuthorizationType_AWS_IAM :: AuthorizationType
pattern AuthorizationType_AWS_IAM = AuthorizationType' "AWS_IAM"

pattern AuthorizationType_CUSTOM :: AuthorizationType
pattern AuthorizationType_CUSTOM = AuthorizationType' "CUSTOM"

pattern AuthorizationType_JWT :: AuthorizationType
pattern AuthorizationType_JWT = AuthorizationType' "JWT"

pattern AuthorizationType_NONE :: AuthorizationType
pattern AuthorizationType_NONE = AuthorizationType' "NONE"

{-# COMPLETE
  AuthorizationType_AWS_IAM,
  AuthorizationType_CUSTOM,
  AuthorizationType_JWT,
  AuthorizationType_NONE,
  AuthorizationType'
  #-}
