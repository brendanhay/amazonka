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
-- Module      : Amazonka.DMS.Types.KafkaSaslMechanism
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.KafkaSaslMechanism
  ( KafkaSaslMechanism
      ( ..,
        KafkaSaslMechanism_Plain,
        KafkaSaslMechanism_Scram_sha_512
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype KafkaSaslMechanism = KafkaSaslMechanism'
  { fromKafkaSaslMechanism ::
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

pattern KafkaSaslMechanism_Plain :: KafkaSaslMechanism
pattern KafkaSaslMechanism_Plain = KafkaSaslMechanism' "plain"

pattern KafkaSaslMechanism_Scram_sha_512 :: KafkaSaslMechanism
pattern KafkaSaslMechanism_Scram_sha_512 = KafkaSaslMechanism' "scram-sha-512"

{-# COMPLETE
  KafkaSaslMechanism_Plain,
  KafkaSaslMechanism_Scram_sha_512,
  KafkaSaslMechanism'
  #-}
