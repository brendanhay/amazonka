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
-- Module      : Amazonka.GamesParks.Types.GeneratedCodeJobState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GamesParks.Types.GeneratedCodeJobState
  ( GeneratedCodeJobState
      ( ..,
        GeneratedCodeJobState_COMPLETED,
        GeneratedCodeJobState_FAILED,
        GeneratedCodeJobState_IN_PROGRESS,
        GeneratedCodeJobState_PENDING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype GeneratedCodeJobState = GeneratedCodeJobState'
  { fromGeneratedCodeJobState ::
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

pattern GeneratedCodeJobState_COMPLETED :: GeneratedCodeJobState
pattern GeneratedCodeJobState_COMPLETED = GeneratedCodeJobState' "COMPLETED"

pattern GeneratedCodeJobState_FAILED :: GeneratedCodeJobState
pattern GeneratedCodeJobState_FAILED = GeneratedCodeJobState' "FAILED"

pattern GeneratedCodeJobState_IN_PROGRESS :: GeneratedCodeJobState
pattern GeneratedCodeJobState_IN_PROGRESS = GeneratedCodeJobState' "IN_PROGRESS"

pattern GeneratedCodeJobState_PENDING :: GeneratedCodeJobState
pattern GeneratedCodeJobState_PENDING = GeneratedCodeJobState' "PENDING"

{-# COMPLETE
  GeneratedCodeJobState_COMPLETED,
  GeneratedCodeJobState_FAILED,
  GeneratedCodeJobState_IN_PROGRESS,
  GeneratedCodeJobState_PENDING,
  GeneratedCodeJobState'
  #-}
