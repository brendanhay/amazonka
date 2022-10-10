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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
import qualified Amazonka.Prelude as Prelude

newtype ConnectionStatus = ConnectionStatus'
  { fromConnectionStatus ::
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
