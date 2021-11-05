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
-- Module      : Amazonka.DataSync.Types.PhaseStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataSync.Types.PhaseStatus
  ( PhaseStatus
      ( ..,
        PhaseStatus_ERROR,
        PhaseStatus_PENDING,
        PhaseStatus_SUCCESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype PhaseStatus = PhaseStatus'
  { fromPhaseStatus ::
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

pattern PhaseStatus_ERROR :: PhaseStatus
pattern PhaseStatus_ERROR = PhaseStatus' "ERROR"

pattern PhaseStatus_PENDING :: PhaseStatus
pattern PhaseStatus_PENDING = PhaseStatus' "PENDING"

pattern PhaseStatus_SUCCESS :: PhaseStatus
pattern PhaseStatus_SUCCESS = PhaseStatus' "SUCCESS"

{-# COMPLETE
  PhaseStatus_ERROR,
  PhaseStatus_PENDING,
  PhaseStatus_SUCCESS,
  PhaseStatus'
  #-}
