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
-- Module      : Network.AWS.CodeBuild.Types.AuthType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.AuthType
  ( AuthType
      ( ..,
        AuthType_BASIC_AUTH,
        AuthType_OAUTH,
        AuthType_PERSONAL_ACCESS_TOKEN
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype AuthType = AuthType'
  { fromAuthType ::
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

pattern AuthType_BASIC_AUTH :: AuthType
pattern AuthType_BASIC_AUTH = AuthType' "BASIC_AUTH"

pattern AuthType_OAUTH :: AuthType
pattern AuthType_OAUTH = AuthType' "OAUTH"

pattern AuthType_PERSONAL_ACCESS_TOKEN :: AuthType
pattern AuthType_PERSONAL_ACCESS_TOKEN = AuthType' "PERSONAL_ACCESS_TOKEN"

{-# COMPLETE
  AuthType_BASIC_AUTH,
  AuthType_OAUTH,
  AuthType_PERSONAL_ACCESS_TOKEN,
  AuthType'
  #-}
