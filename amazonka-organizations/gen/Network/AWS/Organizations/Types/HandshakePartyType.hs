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
-- Module      : Network.AWS.Organizations.Types.HandshakePartyType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.HandshakePartyType
  ( HandshakePartyType
      ( ..,
        HandshakePartyType_ACCOUNT,
        HandshakePartyType_EMAIL,
        HandshakePartyType_ORGANIZATION
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype HandshakePartyType = HandshakePartyType'
  { fromHandshakePartyType ::
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

pattern HandshakePartyType_ACCOUNT :: HandshakePartyType
pattern HandshakePartyType_ACCOUNT = HandshakePartyType' "ACCOUNT"

pattern HandshakePartyType_EMAIL :: HandshakePartyType
pattern HandshakePartyType_EMAIL = HandshakePartyType' "EMAIL"

pattern HandshakePartyType_ORGANIZATION :: HandshakePartyType
pattern HandshakePartyType_ORGANIZATION = HandshakePartyType' "ORGANIZATION"

{-# COMPLETE
  HandshakePartyType_ACCOUNT,
  HandshakePartyType_EMAIL,
  HandshakePartyType_ORGANIZATION,
  HandshakePartyType'
  #-}
