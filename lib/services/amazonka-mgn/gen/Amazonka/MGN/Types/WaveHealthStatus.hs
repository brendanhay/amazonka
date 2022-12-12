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
-- Module      : Amazonka.MGN.Types.WaveHealthStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.WaveHealthStatus
  ( WaveHealthStatus
      ( ..,
        WaveHealthStatus_ERROR,
        WaveHealthStatus_HEALTHY,
        WaveHealthStatus_LAGGING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype WaveHealthStatus = WaveHealthStatus'
  { fromWaveHealthStatus ::
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

pattern WaveHealthStatus_ERROR :: WaveHealthStatus
pattern WaveHealthStatus_ERROR = WaveHealthStatus' "ERROR"

pattern WaveHealthStatus_HEALTHY :: WaveHealthStatus
pattern WaveHealthStatus_HEALTHY = WaveHealthStatus' "HEALTHY"

pattern WaveHealthStatus_LAGGING :: WaveHealthStatus
pattern WaveHealthStatus_LAGGING = WaveHealthStatus' "LAGGING"

{-# COMPLETE
  WaveHealthStatus_ERROR,
  WaveHealthStatus_HEALTHY,
  WaveHealthStatus_LAGGING,
  WaveHealthStatus'
  #-}
