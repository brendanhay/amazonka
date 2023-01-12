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
-- Module      : Amazonka.IAM.Types.DeletionTaskStatusType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IAM.Types.DeletionTaskStatusType
  ( DeletionTaskStatusType
      ( ..,
        DeletionTaskStatusType_FAILED,
        DeletionTaskStatusType_IN_PROGRESS,
        DeletionTaskStatusType_NOT_STARTED,
        DeletionTaskStatusType_SUCCEEDED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DeletionTaskStatusType = DeletionTaskStatusType'
  { fromDeletionTaskStatusType ::
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

pattern DeletionTaskStatusType_FAILED :: DeletionTaskStatusType
pattern DeletionTaskStatusType_FAILED = DeletionTaskStatusType' "FAILED"

pattern DeletionTaskStatusType_IN_PROGRESS :: DeletionTaskStatusType
pattern DeletionTaskStatusType_IN_PROGRESS = DeletionTaskStatusType' "IN_PROGRESS"

pattern DeletionTaskStatusType_NOT_STARTED :: DeletionTaskStatusType
pattern DeletionTaskStatusType_NOT_STARTED = DeletionTaskStatusType' "NOT_STARTED"

pattern DeletionTaskStatusType_SUCCEEDED :: DeletionTaskStatusType
pattern DeletionTaskStatusType_SUCCEEDED = DeletionTaskStatusType' "SUCCEEDED"

{-# COMPLETE
  DeletionTaskStatusType_FAILED,
  DeletionTaskStatusType_IN_PROGRESS,
  DeletionTaskStatusType_NOT_STARTED,
  DeletionTaskStatusType_SUCCEEDED,
  DeletionTaskStatusType'
  #-}
