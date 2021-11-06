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
-- Module      : Amazonka.DMS.Types.RedisAuthTypeValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.RedisAuthTypeValue
  ( RedisAuthTypeValue
      ( ..,
        RedisAuthTypeValue_Auth_role,
        RedisAuthTypeValue_Auth_token,
        RedisAuthTypeValue_None
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype RedisAuthTypeValue = RedisAuthTypeValue'
  { fromRedisAuthTypeValue ::
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

pattern RedisAuthTypeValue_Auth_role :: RedisAuthTypeValue
pattern RedisAuthTypeValue_Auth_role = RedisAuthTypeValue' "auth-role"

pattern RedisAuthTypeValue_Auth_token :: RedisAuthTypeValue
pattern RedisAuthTypeValue_Auth_token = RedisAuthTypeValue' "auth-token"

pattern RedisAuthTypeValue_None :: RedisAuthTypeValue
pattern RedisAuthTypeValue_None = RedisAuthTypeValue' "none"

{-# COMPLETE
  RedisAuthTypeValue_Auth_role,
  RedisAuthTypeValue_Auth_token,
  RedisAuthTypeValue_None,
  RedisAuthTypeValue'
  #-}
