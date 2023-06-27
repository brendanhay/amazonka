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
-- Module      : Amazonka.VPCLattice.Types.TargetGroupStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VPCLattice.Types.TargetGroupStatus
  ( TargetGroupStatus
      ( ..,
        TargetGroupStatus_ACTIVE,
        TargetGroupStatus_CREATE_FAILED,
        TargetGroupStatus_CREATE_IN_PROGRESS,
        TargetGroupStatus_DELETE_FAILED,
        TargetGroupStatus_DELETE_IN_PROGRESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TargetGroupStatus = TargetGroupStatus'
  { fromTargetGroupStatus ::
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

pattern TargetGroupStatus_ACTIVE :: TargetGroupStatus
pattern TargetGroupStatus_ACTIVE = TargetGroupStatus' "ACTIVE"

pattern TargetGroupStatus_CREATE_FAILED :: TargetGroupStatus
pattern TargetGroupStatus_CREATE_FAILED = TargetGroupStatus' "CREATE_FAILED"

pattern TargetGroupStatus_CREATE_IN_PROGRESS :: TargetGroupStatus
pattern TargetGroupStatus_CREATE_IN_PROGRESS = TargetGroupStatus' "CREATE_IN_PROGRESS"

pattern TargetGroupStatus_DELETE_FAILED :: TargetGroupStatus
pattern TargetGroupStatus_DELETE_FAILED = TargetGroupStatus' "DELETE_FAILED"

pattern TargetGroupStatus_DELETE_IN_PROGRESS :: TargetGroupStatus
pattern TargetGroupStatus_DELETE_IN_PROGRESS = TargetGroupStatus' "DELETE_IN_PROGRESS"

{-# COMPLETE
  TargetGroupStatus_ACTIVE,
  TargetGroupStatus_CREATE_FAILED,
  TargetGroupStatus_CREATE_IN_PROGRESS,
  TargetGroupStatus_DELETE_FAILED,
  TargetGroupStatus_DELETE_IN_PROGRESS,
  TargetGroupStatus'
  #-}
