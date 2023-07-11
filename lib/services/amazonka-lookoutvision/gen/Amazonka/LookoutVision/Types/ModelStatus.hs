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
-- Module      : Amazonka.LookoutVision.Types.ModelStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutVision.Types.ModelStatus
  ( ModelStatus
      ( ..,
        ModelStatus_DELETING,
        ModelStatus_HOSTED,
        ModelStatus_HOSTING_FAILED,
        ModelStatus_STARTING_HOSTING,
        ModelStatus_STOPPING_HOSTING,
        ModelStatus_SYSTEM_UPDATING,
        ModelStatus_TRAINED,
        ModelStatus_TRAINING,
        ModelStatus_TRAINING_FAILED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ModelStatus = ModelStatus'
  { fromModelStatus ::
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

pattern ModelStatus_DELETING :: ModelStatus
pattern ModelStatus_DELETING = ModelStatus' "DELETING"

pattern ModelStatus_HOSTED :: ModelStatus
pattern ModelStatus_HOSTED = ModelStatus' "HOSTED"

pattern ModelStatus_HOSTING_FAILED :: ModelStatus
pattern ModelStatus_HOSTING_FAILED = ModelStatus' "HOSTING_FAILED"

pattern ModelStatus_STARTING_HOSTING :: ModelStatus
pattern ModelStatus_STARTING_HOSTING = ModelStatus' "STARTING_HOSTING"

pattern ModelStatus_STOPPING_HOSTING :: ModelStatus
pattern ModelStatus_STOPPING_HOSTING = ModelStatus' "STOPPING_HOSTING"

pattern ModelStatus_SYSTEM_UPDATING :: ModelStatus
pattern ModelStatus_SYSTEM_UPDATING = ModelStatus' "SYSTEM_UPDATING"

pattern ModelStatus_TRAINED :: ModelStatus
pattern ModelStatus_TRAINED = ModelStatus' "TRAINED"

pattern ModelStatus_TRAINING :: ModelStatus
pattern ModelStatus_TRAINING = ModelStatus' "TRAINING"

pattern ModelStatus_TRAINING_FAILED :: ModelStatus
pattern ModelStatus_TRAINING_FAILED = ModelStatus' "TRAINING_FAILED"

{-# COMPLETE
  ModelStatus_DELETING,
  ModelStatus_HOSTED,
  ModelStatus_HOSTING_FAILED,
  ModelStatus_STARTING_HOSTING,
  ModelStatus_STOPPING_HOSTING,
  ModelStatus_SYSTEM_UPDATING,
  ModelStatus_TRAINED,
  ModelStatus_TRAINING,
  ModelStatus_TRAINING_FAILED,
  ModelStatus'
  #-}
