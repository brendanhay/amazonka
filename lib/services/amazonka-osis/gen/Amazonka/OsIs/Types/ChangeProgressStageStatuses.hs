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
-- Module      : Amazonka.OsIs.Types.ChangeProgressStageStatuses
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OsIs.Types.ChangeProgressStageStatuses
  ( ChangeProgressStageStatuses
      ( ..,
        ChangeProgressStageStatuses_COMPLETED,
        ChangeProgressStageStatuses_FAILED,
        ChangeProgressStageStatuses_IN_PROGRESS,
        ChangeProgressStageStatuses_PENDING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ChangeProgressStageStatuses = ChangeProgressStageStatuses'
  { fromChangeProgressStageStatuses ::
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

pattern ChangeProgressStageStatuses_COMPLETED :: ChangeProgressStageStatuses
pattern ChangeProgressStageStatuses_COMPLETED = ChangeProgressStageStatuses' "COMPLETED"

pattern ChangeProgressStageStatuses_FAILED :: ChangeProgressStageStatuses
pattern ChangeProgressStageStatuses_FAILED = ChangeProgressStageStatuses' "FAILED"

pattern ChangeProgressStageStatuses_IN_PROGRESS :: ChangeProgressStageStatuses
pattern ChangeProgressStageStatuses_IN_PROGRESS = ChangeProgressStageStatuses' "IN_PROGRESS"

pattern ChangeProgressStageStatuses_PENDING :: ChangeProgressStageStatuses
pattern ChangeProgressStageStatuses_PENDING = ChangeProgressStageStatuses' "PENDING"

{-# COMPLETE
  ChangeProgressStageStatuses_COMPLETED,
  ChangeProgressStageStatuses_FAILED,
  ChangeProgressStageStatuses_IN_PROGRESS,
  ChangeProgressStageStatuses_PENDING,
  ChangeProgressStageStatuses'
  #-}
