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
-- Module      : Amazonka.FinSpaceData.Types.ChangesetStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FinSpaceData.Types.ChangesetStatus
  ( ChangesetStatus
      ( ..,
        ChangesetStatus_FAILED,
        ChangesetStatus_PENDING,
        ChangesetStatus_RUNNING,
        ChangesetStatus_STOP_REQUESTED,
        ChangesetStatus_SUCCESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype ChangesetStatus = ChangesetStatus'
  { fromChangesetStatus ::
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

pattern ChangesetStatus_FAILED :: ChangesetStatus
pattern ChangesetStatus_FAILED = ChangesetStatus' "FAILED"

pattern ChangesetStatus_PENDING :: ChangesetStatus
pattern ChangesetStatus_PENDING = ChangesetStatus' "PENDING"

pattern ChangesetStatus_RUNNING :: ChangesetStatus
pattern ChangesetStatus_RUNNING = ChangesetStatus' "RUNNING"

pattern ChangesetStatus_STOP_REQUESTED :: ChangesetStatus
pattern ChangesetStatus_STOP_REQUESTED = ChangesetStatus' "STOP_REQUESTED"

pattern ChangesetStatus_SUCCESS :: ChangesetStatus
pattern ChangesetStatus_SUCCESS = ChangesetStatus' "SUCCESS"

{-# COMPLETE
  ChangesetStatus_FAILED,
  ChangesetStatus_PENDING,
  ChangesetStatus_RUNNING,
  ChangesetStatus_STOP_REQUESTED,
  ChangesetStatus_SUCCESS,
  ChangesetStatus'
  #-}
