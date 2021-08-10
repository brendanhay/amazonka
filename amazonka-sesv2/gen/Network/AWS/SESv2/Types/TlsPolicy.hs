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
-- Module      : Network.AWS.SESv2.Types.TlsPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SESv2.Types.TlsPolicy
  ( TlsPolicy
      ( ..,
        TlsPolicy_OPTIONAL,
        TlsPolicy_REQUIRE
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Specifies whether messages that use the configuration set are required
-- to use Transport Layer Security (TLS). If the value is @Require@,
-- messages are only delivered if a TLS connection can be established. If
-- the value is @Optional@, messages can be delivered in plain text if a
-- TLS connection can\'t be established.
newtype TlsPolicy = TlsPolicy'
  { fromTlsPolicy ::
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

pattern TlsPolicy_OPTIONAL :: TlsPolicy
pattern TlsPolicy_OPTIONAL = TlsPolicy' "OPTIONAL"

pattern TlsPolicy_REQUIRE :: TlsPolicy
pattern TlsPolicy_REQUIRE = TlsPolicy' "REQUIRE"

{-# COMPLETE
  TlsPolicy_OPTIONAL,
  TlsPolicy_REQUIRE,
  TlsPolicy'
  #-}
