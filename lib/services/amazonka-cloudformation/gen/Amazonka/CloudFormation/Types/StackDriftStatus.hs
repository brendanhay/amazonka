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
-- Module      : Amazonka.CloudFormation.Types.StackDriftStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types.StackDriftStatus
  ( StackDriftStatus
      ( ..,
        StackDriftStatus_DRIFTED,
        StackDriftStatus_IN_SYNC,
        StackDriftStatus_NOT_CHECKED,
        StackDriftStatus_UNKNOWN
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype StackDriftStatus = StackDriftStatus'
  { fromStackDriftStatus ::
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

pattern StackDriftStatus_DRIFTED :: StackDriftStatus
pattern StackDriftStatus_DRIFTED = StackDriftStatus' "DRIFTED"

pattern StackDriftStatus_IN_SYNC :: StackDriftStatus
pattern StackDriftStatus_IN_SYNC = StackDriftStatus' "IN_SYNC"

pattern StackDriftStatus_NOT_CHECKED :: StackDriftStatus
pattern StackDriftStatus_NOT_CHECKED = StackDriftStatus' "NOT_CHECKED"

pattern StackDriftStatus_UNKNOWN :: StackDriftStatus
pattern StackDriftStatus_UNKNOWN = StackDriftStatus' "UNKNOWN"

{-# COMPLETE
  StackDriftStatus_DRIFTED,
  StackDriftStatus_IN_SYNC,
  StackDriftStatus_NOT_CHECKED,
  StackDriftStatus_UNKNOWN,
  StackDriftStatus'
  #-}
