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
-- Module      : Amazonka.Organizations.Types.HandshakeResourceType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Organizations.Types.HandshakeResourceType
  ( HandshakeResourceType
      ( ..,
        HandshakeResourceType_ACCOUNT,
        HandshakeResourceType_EMAIL,
        HandshakeResourceType_MASTER_EMAIL,
        HandshakeResourceType_MASTER_NAME,
        HandshakeResourceType_NOTES,
        HandshakeResourceType_ORGANIZATION,
        HandshakeResourceType_ORGANIZATION_FEATURE_SET,
        HandshakeResourceType_PARENT_HANDSHAKE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype HandshakeResourceType = HandshakeResourceType'
  { fromHandshakeResourceType ::
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

pattern HandshakeResourceType_ACCOUNT :: HandshakeResourceType
pattern HandshakeResourceType_ACCOUNT = HandshakeResourceType' "ACCOUNT"

pattern HandshakeResourceType_EMAIL :: HandshakeResourceType
pattern HandshakeResourceType_EMAIL = HandshakeResourceType' "EMAIL"

pattern HandshakeResourceType_MASTER_EMAIL :: HandshakeResourceType
pattern HandshakeResourceType_MASTER_EMAIL = HandshakeResourceType' "MASTER_EMAIL"

pattern HandshakeResourceType_MASTER_NAME :: HandshakeResourceType
pattern HandshakeResourceType_MASTER_NAME = HandshakeResourceType' "MASTER_NAME"

pattern HandshakeResourceType_NOTES :: HandshakeResourceType
pattern HandshakeResourceType_NOTES = HandshakeResourceType' "NOTES"

pattern HandshakeResourceType_ORGANIZATION :: HandshakeResourceType
pattern HandshakeResourceType_ORGANIZATION = HandshakeResourceType' "ORGANIZATION"

pattern HandshakeResourceType_ORGANIZATION_FEATURE_SET :: HandshakeResourceType
pattern HandshakeResourceType_ORGANIZATION_FEATURE_SET = HandshakeResourceType' "ORGANIZATION_FEATURE_SET"

pattern HandshakeResourceType_PARENT_HANDSHAKE :: HandshakeResourceType
pattern HandshakeResourceType_PARENT_HANDSHAKE = HandshakeResourceType' "PARENT_HANDSHAKE"

{-# COMPLETE
  HandshakeResourceType_ACCOUNT,
  HandshakeResourceType_EMAIL,
  HandshakeResourceType_MASTER_EMAIL,
  HandshakeResourceType_MASTER_NAME,
  HandshakeResourceType_NOTES,
  HandshakeResourceType_ORGANIZATION,
  HandshakeResourceType_ORGANIZATION_FEATURE_SET,
  HandshakeResourceType_PARENT_HANDSHAKE,
  HandshakeResourceType'
  #-}
