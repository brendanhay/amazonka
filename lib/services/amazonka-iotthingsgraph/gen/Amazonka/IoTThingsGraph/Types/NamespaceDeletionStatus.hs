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
-- Module      : Amazonka.IoTThingsGraph.Types.NamespaceDeletionStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTThingsGraph.Types.NamespaceDeletionStatus
  ( NamespaceDeletionStatus
      ( ..,
        NamespaceDeletionStatus_FAILED,
        NamespaceDeletionStatus_IN_PROGRESS,
        NamespaceDeletionStatus_SUCCEEDED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype NamespaceDeletionStatus = NamespaceDeletionStatus'
  { fromNamespaceDeletionStatus ::
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

pattern NamespaceDeletionStatus_FAILED :: NamespaceDeletionStatus
pattern NamespaceDeletionStatus_FAILED = NamespaceDeletionStatus' "FAILED"

pattern NamespaceDeletionStatus_IN_PROGRESS :: NamespaceDeletionStatus
pattern NamespaceDeletionStatus_IN_PROGRESS = NamespaceDeletionStatus' "IN_PROGRESS"

pattern NamespaceDeletionStatus_SUCCEEDED :: NamespaceDeletionStatus
pattern NamespaceDeletionStatus_SUCCEEDED = NamespaceDeletionStatus' "SUCCEEDED"

{-# COMPLETE
  NamespaceDeletionStatus_FAILED,
  NamespaceDeletionStatus_IN_PROGRESS,
  NamespaceDeletionStatus_SUCCEEDED,
  NamespaceDeletionStatus'
  #-}
