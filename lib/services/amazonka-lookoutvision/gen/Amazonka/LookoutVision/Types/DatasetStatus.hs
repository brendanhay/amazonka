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
-- Module      : Amazonka.LookoutVision.Types.DatasetStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutVision.Types.DatasetStatus
  ( DatasetStatus
      ( ..,
        DatasetStatus_CREATE_COMPLETE,
        DatasetStatus_CREATE_FAILED,
        DatasetStatus_CREATE_IN_PROGRESS,
        DatasetStatus_DELETE_COMPLETE,
        DatasetStatus_DELETE_FAILED,
        DatasetStatus_DELETE_IN_PROGRESS,
        DatasetStatus_UPDATE_COMPLETE,
        DatasetStatus_UPDATE_FAILED_ROLLBACK_COMPLETE,
        DatasetStatus_UPDATE_FAILED_ROLLBACK_IN_PROGRESS,
        DatasetStatus_UPDATE_IN_PROGRESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DatasetStatus = DatasetStatus'
  { fromDatasetStatus ::
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

pattern DatasetStatus_CREATE_COMPLETE :: DatasetStatus
pattern DatasetStatus_CREATE_COMPLETE = DatasetStatus' "CREATE_COMPLETE"

pattern DatasetStatus_CREATE_FAILED :: DatasetStatus
pattern DatasetStatus_CREATE_FAILED = DatasetStatus' "CREATE_FAILED"

pattern DatasetStatus_CREATE_IN_PROGRESS :: DatasetStatus
pattern DatasetStatus_CREATE_IN_PROGRESS = DatasetStatus' "CREATE_IN_PROGRESS"

pattern DatasetStatus_DELETE_COMPLETE :: DatasetStatus
pattern DatasetStatus_DELETE_COMPLETE = DatasetStatus' "DELETE_COMPLETE"

pattern DatasetStatus_DELETE_FAILED :: DatasetStatus
pattern DatasetStatus_DELETE_FAILED = DatasetStatus' "DELETE_FAILED"

pattern DatasetStatus_DELETE_IN_PROGRESS :: DatasetStatus
pattern DatasetStatus_DELETE_IN_PROGRESS = DatasetStatus' "DELETE_IN_PROGRESS"

pattern DatasetStatus_UPDATE_COMPLETE :: DatasetStatus
pattern DatasetStatus_UPDATE_COMPLETE = DatasetStatus' "UPDATE_COMPLETE"

pattern DatasetStatus_UPDATE_FAILED_ROLLBACK_COMPLETE :: DatasetStatus
pattern DatasetStatus_UPDATE_FAILED_ROLLBACK_COMPLETE = DatasetStatus' "UPDATE_FAILED_ROLLBACK_COMPLETE"

pattern DatasetStatus_UPDATE_FAILED_ROLLBACK_IN_PROGRESS :: DatasetStatus
pattern DatasetStatus_UPDATE_FAILED_ROLLBACK_IN_PROGRESS = DatasetStatus' "UPDATE_FAILED_ROLLBACK_IN_PROGRESS"

pattern DatasetStatus_UPDATE_IN_PROGRESS :: DatasetStatus
pattern DatasetStatus_UPDATE_IN_PROGRESS = DatasetStatus' "UPDATE_IN_PROGRESS"

{-# COMPLETE
  DatasetStatus_CREATE_COMPLETE,
  DatasetStatus_CREATE_FAILED,
  DatasetStatus_CREATE_IN_PROGRESS,
  DatasetStatus_DELETE_COMPLETE,
  DatasetStatus_DELETE_FAILED,
  DatasetStatus_DELETE_IN_PROGRESS,
  DatasetStatus_UPDATE_COMPLETE,
  DatasetStatus_UPDATE_FAILED_ROLLBACK_COMPLETE,
  DatasetStatus_UPDATE_FAILED_ROLLBACK_IN_PROGRESS,
  DatasetStatus_UPDATE_IN_PROGRESS,
  DatasetStatus'
  #-}
