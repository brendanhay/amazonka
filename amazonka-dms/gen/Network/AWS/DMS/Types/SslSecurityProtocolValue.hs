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
-- Module      : Network.AWS.DMS.Types.SslSecurityProtocolValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.SslSecurityProtocolValue
  ( SslSecurityProtocolValue
      ( ..,
        SslSecurityProtocolValue_Plaintext,
        SslSecurityProtocolValue_Ssl_encryption
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype SslSecurityProtocolValue = SslSecurityProtocolValue'
  { fromSslSecurityProtocolValue ::
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

pattern SslSecurityProtocolValue_Plaintext :: SslSecurityProtocolValue
pattern SslSecurityProtocolValue_Plaintext = SslSecurityProtocolValue' "plaintext"

pattern SslSecurityProtocolValue_Ssl_encryption :: SslSecurityProtocolValue
pattern SslSecurityProtocolValue_Ssl_encryption = SslSecurityProtocolValue' "ssl-encryption"

{-# COMPLETE
  SslSecurityProtocolValue_Plaintext,
  SslSecurityProtocolValue_Ssl_encryption,
  SslSecurityProtocolValue'
  #-}
