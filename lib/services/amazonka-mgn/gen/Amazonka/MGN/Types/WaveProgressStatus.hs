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
-- Module      : Amazonka.MGN.Types.WaveProgressStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.WaveProgressStatus
  ( WaveProgressStatus
      ( ..,
        WaveProgressStatus_COMPLETED,
        WaveProgressStatus_IN_PROGRESS,
        WaveProgressStatus_NOT_STARTED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype WaveProgressStatus = WaveProgressStatus'
  { fromWaveProgressStatus ::
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

pattern WaveProgressStatus_COMPLETED :: WaveProgressStatus
pattern WaveProgressStatus_COMPLETED = WaveProgressStatus' "COMPLETED"

pattern WaveProgressStatus_IN_PROGRESS :: WaveProgressStatus
pattern WaveProgressStatus_IN_PROGRESS = WaveProgressStatus' "IN_PROGRESS"

pattern WaveProgressStatus_NOT_STARTED :: WaveProgressStatus
pattern WaveProgressStatus_NOT_STARTED = WaveProgressStatus' "NOT_STARTED"

{-# COMPLETE
  WaveProgressStatus_COMPLETED,
  WaveProgressStatus_IN_PROGRESS,
  WaveProgressStatus_NOT_STARTED,
  WaveProgressStatus'
  #-}
