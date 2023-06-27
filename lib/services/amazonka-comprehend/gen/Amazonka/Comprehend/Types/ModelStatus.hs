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
-- Module      : Amazonka.Comprehend.Types.ModelStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.ModelStatus
  ( ModelStatus
      ( ..,
        ModelStatus_DELETING,
        ModelStatus_IN_ERROR,
        ModelStatus_STOPPED,
        ModelStatus_STOP_REQUESTED,
        ModelStatus_SUBMITTED,
        ModelStatus_TRAINED,
        ModelStatus_TRAINED_WITH_WARNING,
        ModelStatus_TRAINING
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

pattern ModelStatus_IN_ERROR :: ModelStatus
pattern ModelStatus_IN_ERROR = ModelStatus' "IN_ERROR"

pattern ModelStatus_STOPPED :: ModelStatus
pattern ModelStatus_STOPPED = ModelStatus' "STOPPED"

pattern ModelStatus_STOP_REQUESTED :: ModelStatus
pattern ModelStatus_STOP_REQUESTED = ModelStatus' "STOP_REQUESTED"

pattern ModelStatus_SUBMITTED :: ModelStatus
pattern ModelStatus_SUBMITTED = ModelStatus' "SUBMITTED"

pattern ModelStatus_TRAINED :: ModelStatus
pattern ModelStatus_TRAINED = ModelStatus' "TRAINED"

pattern ModelStatus_TRAINED_WITH_WARNING :: ModelStatus
pattern ModelStatus_TRAINED_WITH_WARNING = ModelStatus' "TRAINED_WITH_WARNING"

pattern ModelStatus_TRAINING :: ModelStatus
pattern ModelStatus_TRAINING = ModelStatus' "TRAINING"

{-# COMPLETE
  ModelStatus_DELETING,
  ModelStatus_IN_ERROR,
  ModelStatus_STOPPED,
  ModelStatus_STOP_REQUESTED,
  ModelStatus_SUBMITTED,
  ModelStatus_TRAINED,
  ModelStatus_TRAINED_WITH_WARNING,
  ModelStatus_TRAINING,
  ModelStatus'
  #-}
