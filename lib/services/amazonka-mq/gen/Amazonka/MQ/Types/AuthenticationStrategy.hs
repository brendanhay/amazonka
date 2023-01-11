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
-- Module      : Amazonka.MQ.Types.AuthenticationStrategy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MQ.Types.AuthenticationStrategy
  ( AuthenticationStrategy
      ( ..,
        AuthenticationStrategy_LDAP,
        AuthenticationStrategy_SIMPLE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Optional. The authentication strategy used to secure the broker. The
-- default is SIMPLE.
newtype AuthenticationStrategy = AuthenticationStrategy'
  { fromAuthenticationStrategy ::
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

pattern AuthenticationStrategy_LDAP :: AuthenticationStrategy
pattern AuthenticationStrategy_LDAP = AuthenticationStrategy' "LDAP"

pattern AuthenticationStrategy_SIMPLE :: AuthenticationStrategy
pattern AuthenticationStrategy_SIMPLE = AuthenticationStrategy' "SIMPLE"

{-# COMPLETE
  AuthenticationStrategy_LDAP,
  AuthenticationStrategy_SIMPLE,
  AuthenticationStrategy'
  #-}
