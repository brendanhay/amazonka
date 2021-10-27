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
-- Module      : Network.AWS.SecurityHub.Types.ThreatIntelIndicatorType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SecurityHub.Types.ThreatIntelIndicatorType
  ( ThreatIntelIndicatorType
      ( ..,
        ThreatIntelIndicatorType_DOMAIN,
        ThreatIntelIndicatorType_EMAIL_ADDRESS,
        ThreatIntelIndicatorType_HASH_MD5,
        ThreatIntelIndicatorType_HASH_SHA1,
        ThreatIntelIndicatorType_HASH_SHA256,
        ThreatIntelIndicatorType_HASH_SHA512,
        ThreatIntelIndicatorType_IPV4_ADDRESS,
        ThreatIntelIndicatorType_IPV6_ADDRESS,
        ThreatIntelIndicatorType_MUTEX,
        ThreatIntelIndicatorType_PROCESS,
        ThreatIntelIndicatorType_URL
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ThreatIntelIndicatorType = ThreatIntelIndicatorType'
  { fromThreatIntelIndicatorType ::
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

pattern ThreatIntelIndicatorType_DOMAIN :: ThreatIntelIndicatorType
pattern ThreatIntelIndicatorType_DOMAIN = ThreatIntelIndicatorType' "DOMAIN"

pattern ThreatIntelIndicatorType_EMAIL_ADDRESS :: ThreatIntelIndicatorType
pattern ThreatIntelIndicatorType_EMAIL_ADDRESS = ThreatIntelIndicatorType' "EMAIL_ADDRESS"

pattern ThreatIntelIndicatorType_HASH_MD5 :: ThreatIntelIndicatorType
pattern ThreatIntelIndicatorType_HASH_MD5 = ThreatIntelIndicatorType' "HASH_MD5"

pattern ThreatIntelIndicatorType_HASH_SHA1 :: ThreatIntelIndicatorType
pattern ThreatIntelIndicatorType_HASH_SHA1 = ThreatIntelIndicatorType' "HASH_SHA1"

pattern ThreatIntelIndicatorType_HASH_SHA256 :: ThreatIntelIndicatorType
pattern ThreatIntelIndicatorType_HASH_SHA256 = ThreatIntelIndicatorType' "HASH_SHA256"

pattern ThreatIntelIndicatorType_HASH_SHA512 :: ThreatIntelIndicatorType
pattern ThreatIntelIndicatorType_HASH_SHA512 = ThreatIntelIndicatorType' "HASH_SHA512"

pattern ThreatIntelIndicatorType_IPV4_ADDRESS :: ThreatIntelIndicatorType
pattern ThreatIntelIndicatorType_IPV4_ADDRESS = ThreatIntelIndicatorType' "IPV4_ADDRESS"

pattern ThreatIntelIndicatorType_IPV6_ADDRESS :: ThreatIntelIndicatorType
pattern ThreatIntelIndicatorType_IPV6_ADDRESS = ThreatIntelIndicatorType' "IPV6_ADDRESS"

pattern ThreatIntelIndicatorType_MUTEX :: ThreatIntelIndicatorType
pattern ThreatIntelIndicatorType_MUTEX = ThreatIntelIndicatorType' "MUTEX"

pattern ThreatIntelIndicatorType_PROCESS :: ThreatIntelIndicatorType
pattern ThreatIntelIndicatorType_PROCESS = ThreatIntelIndicatorType' "PROCESS"

pattern ThreatIntelIndicatorType_URL :: ThreatIntelIndicatorType
pattern ThreatIntelIndicatorType_URL = ThreatIntelIndicatorType' "URL"

{-# COMPLETE
  ThreatIntelIndicatorType_DOMAIN,
  ThreatIntelIndicatorType_EMAIL_ADDRESS,
  ThreatIntelIndicatorType_HASH_MD5,
  ThreatIntelIndicatorType_HASH_SHA1,
  ThreatIntelIndicatorType_HASH_SHA256,
  ThreatIntelIndicatorType_HASH_SHA512,
  ThreatIntelIndicatorType_IPV4_ADDRESS,
  ThreatIntelIndicatorType_IPV6_ADDRESS,
  ThreatIntelIndicatorType_MUTEX,
  ThreatIntelIndicatorType_PROCESS,
  ThreatIntelIndicatorType_URL,
  ThreatIntelIndicatorType'
  #-}
