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
-- Module      : Amazonka.CodeStarConnections.Types.ConnectionStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeStarConnections.Types.ConnectionStatus
  ( ConnectionStatus
      ( ..,
        ConnectionStatus_AVAILABLE,
        ConnectionStatus_ERROR,
        ConnectionStatus_PENDING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ConnectionStatus = ConnectionStatus'
  { fromConnectionStatus ::
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

pattern ConnectionStatus_AVAILABLE :: ConnectionStatus
pattern ConnectionStatus_AVAILABLE = ConnectionStatus' "AVAILABLE"

pattern ConnectionStatus_ERROR :: ConnectionStatus
pattern ConnectionStatus_ERROR = ConnectionStatus' "ERROR"

pattern ConnectionStatus_PENDING :: ConnectionStatus
pattern ConnectionStatus_PENDING = ConnectionStatus' "PENDING"

{-# COMPLETE
  ConnectionStatus_AVAILABLE,
  ConnectionStatus_ERROR,
  ConnectionStatus_PENDING,
  ConnectionStatus'
  #-}
