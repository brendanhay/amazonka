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
-- Module      : Amazonka.OpenSearch.Types.ActionType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.ActionType
  ( ActionType
      ( ..,
        ActionType_JVM_HEAP_SIZE_TUNING,
        ActionType_JVM_YOUNG_GEN_TUNING,
        ActionType_SERVICE_SOFTWARE_UPDATE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ActionType = ActionType'
  { fromActionType ::
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

pattern ActionType_JVM_HEAP_SIZE_TUNING :: ActionType
pattern ActionType_JVM_HEAP_SIZE_TUNING = ActionType' "JVM_HEAP_SIZE_TUNING"

pattern ActionType_JVM_YOUNG_GEN_TUNING :: ActionType
pattern ActionType_JVM_YOUNG_GEN_TUNING = ActionType' "JVM_YOUNG_GEN_TUNING"

pattern ActionType_SERVICE_SOFTWARE_UPDATE :: ActionType
pattern ActionType_SERVICE_SOFTWARE_UPDATE = ActionType' "SERVICE_SOFTWARE_UPDATE"

{-# COMPLETE
  ActionType_JVM_HEAP_SIZE_TUNING,
  ActionType_JVM_YOUNG_GEN_TUNING,
  ActionType_SERVICE_SOFTWARE_UPDATE,
  ActionType'
  #-}
