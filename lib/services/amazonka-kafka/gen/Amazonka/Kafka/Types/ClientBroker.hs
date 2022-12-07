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
-- Module      : Amazonka.Kafka.Types.ClientBroker
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kafka.Types.ClientBroker
  ( ClientBroker
      ( ..,
        ClientBroker_PLAINTEXT,
        ClientBroker_TLS,
        ClientBroker_TLS_PLAINTEXT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Client-broker encryption in transit setting.
newtype ClientBroker = ClientBroker'
  { fromClientBroker ::
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

pattern ClientBroker_PLAINTEXT :: ClientBroker
pattern ClientBroker_PLAINTEXT = ClientBroker' "PLAINTEXT"

pattern ClientBroker_TLS :: ClientBroker
pattern ClientBroker_TLS = ClientBroker' "TLS"

pattern ClientBroker_TLS_PLAINTEXT :: ClientBroker
pattern ClientBroker_TLS_PLAINTEXT = ClientBroker' "TLS_PLAINTEXT"

{-# COMPLETE
  ClientBroker_PLAINTEXT,
  ClientBroker_TLS,
  ClientBroker_TLS_PLAINTEXT,
  ClientBroker'
  #-}
