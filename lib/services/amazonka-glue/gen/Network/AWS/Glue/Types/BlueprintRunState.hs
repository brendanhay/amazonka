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
-- Module      : Amazonka.Glue.Types.BlueprintRunState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.BlueprintRunState
  ( BlueprintRunState
      ( ..,
        BlueprintRunState_FAILED,
        BlueprintRunState_ROLLING_BACK,
        BlueprintRunState_RUNNING,
        BlueprintRunState_SUCCEEDED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype BlueprintRunState = BlueprintRunState'
  { fromBlueprintRunState ::
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

pattern BlueprintRunState_FAILED :: BlueprintRunState
pattern BlueprintRunState_FAILED = BlueprintRunState' "FAILED"

pattern BlueprintRunState_ROLLING_BACK :: BlueprintRunState
pattern BlueprintRunState_ROLLING_BACK = BlueprintRunState' "ROLLING_BACK"

pattern BlueprintRunState_RUNNING :: BlueprintRunState
pattern BlueprintRunState_RUNNING = BlueprintRunState' "RUNNING"

pattern BlueprintRunState_SUCCEEDED :: BlueprintRunState
pattern BlueprintRunState_SUCCEEDED = BlueprintRunState' "SUCCEEDED"

{-# COMPLETE
  BlueprintRunState_FAILED,
  BlueprintRunState_ROLLING_BACK,
  BlueprintRunState_RUNNING,
  BlueprintRunState_SUCCEEDED,
  BlueprintRunState'
  #-}
