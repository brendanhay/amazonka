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
-- Module      : Amazonka.LookoutEquipment.Types.ModelStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutEquipment.Types.ModelStatus
  ( ModelStatus
      ( ..,
        ModelStatus_FAILED,
        ModelStatus_IN_PROGRESS,
        ModelStatus_SUCCESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype ModelStatus = ModelStatus'
  { fromModelStatus ::
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

pattern ModelStatus_FAILED :: ModelStatus
pattern ModelStatus_FAILED = ModelStatus' "FAILED"

pattern ModelStatus_IN_PROGRESS :: ModelStatus
pattern ModelStatus_IN_PROGRESS = ModelStatus' "IN_PROGRESS"

pattern ModelStatus_SUCCESS :: ModelStatus
pattern ModelStatus_SUCCESS = ModelStatus' "SUCCESS"

{-# COMPLETE
  ModelStatus_FAILED,
  ModelStatus_IN_PROGRESS,
  ModelStatus_SUCCESS,
  ModelStatus'
  #-}
