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
-- Module      : Amazonka.KafkaConnect.Types.KafkaClusterEncryptionInTransitType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KafkaConnect.Types.KafkaClusterEncryptionInTransitType
  ( KafkaClusterEncryptionInTransitType
      ( ..,
        KafkaClusterEncryptionInTransitType_PLAINTEXT,
        KafkaClusterEncryptionInTransitType_TLS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype KafkaClusterEncryptionInTransitType = KafkaClusterEncryptionInTransitType'
  { fromKafkaClusterEncryptionInTransitType ::
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

pattern KafkaClusterEncryptionInTransitType_PLAINTEXT :: KafkaClusterEncryptionInTransitType
pattern KafkaClusterEncryptionInTransitType_PLAINTEXT = KafkaClusterEncryptionInTransitType' "PLAINTEXT"

pattern KafkaClusterEncryptionInTransitType_TLS :: KafkaClusterEncryptionInTransitType
pattern KafkaClusterEncryptionInTransitType_TLS = KafkaClusterEncryptionInTransitType' "TLS"

{-# COMPLETE
  KafkaClusterEncryptionInTransitType_PLAINTEXT,
  KafkaClusterEncryptionInTransitType_TLS,
  KafkaClusterEncryptionInTransitType'
  #-}
