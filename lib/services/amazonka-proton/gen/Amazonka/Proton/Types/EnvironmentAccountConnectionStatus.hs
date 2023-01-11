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
-- Module      : Amazonka.Proton.Types.EnvironmentAccountConnectionStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Proton.Types.EnvironmentAccountConnectionStatus
  ( EnvironmentAccountConnectionStatus
      ( ..,
        EnvironmentAccountConnectionStatus_CONNECTED,
        EnvironmentAccountConnectionStatus_PENDING,
        EnvironmentAccountConnectionStatus_REJECTED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype EnvironmentAccountConnectionStatus = EnvironmentAccountConnectionStatus'
  { fromEnvironmentAccountConnectionStatus ::
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

pattern EnvironmentAccountConnectionStatus_CONNECTED :: EnvironmentAccountConnectionStatus
pattern EnvironmentAccountConnectionStatus_CONNECTED = EnvironmentAccountConnectionStatus' "CONNECTED"

pattern EnvironmentAccountConnectionStatus_PENDING :: EnvironmentAccountConnectionStatus
pattern EnvironmentAccountConnectionStatus_PENDING = EnvironmentAccountConnectionStatus' "PENDING"

pattern EnvironmentAccountConnectionStatus_REJECTED :: EnvironmentAccountConnectionStatus
pattern EnvironmentAccountConnectionStatus_REJECTED = EnvironmentAccountConnectionStatus' "REJECTED"

{-# COMPLETE
  EnvironmentAccountConnectionStatus_CONNECTED,
  EnvironmentAccountConnectionStatus_PENDING,
  EnvironmentAccountConnectionStatus_REJECTED,
  EnvironmentAccountConnectionStatus'
  #-}
