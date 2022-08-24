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
-- Module      : Amazonka.FinSpaceData.Types.DatasetStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FinSpaceData.Types.DatasetStatus
  ( DatasetStatus
      ( ..,
        DatasetStatus_FAILED,
        DatasetStatus_PENDING,
        DatasetStatus_RUNNING,
        DatasetStatus_SUCCESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | Status of the dataset process returned from scheduler service.
newtype DatasetStatus = DatasetStatus'
  { fromDatasetStatus ::
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

pattern DatasetStatus_FAILED :: DatasetStatus
pattern DatasetStatus_FAILED = DatasetStatus' "FAILED"

pattern DatasetStatus_PENDING :: DatasetStatus
pattern DatasetStatus_PENDING = DatasetStatus' "PENDING"

pattern DatasetStatus_RUNNING :: DatasetStatus
pattern DatasetStatus_RUNNING = DatasetStatus' "RUNNING"

pattern DatasetStatus_SUCCESS :: DatasetStatus
pattern DatasetStatus_SUCCESS = DatasetStatus' "SUCCESS"

{-# COMPLETE
  DatasetStatus_FAILED,
  DatasetStatus_PENDING,
  DatasetStatus_RUNNING,
  DatasetStatus_SUCCESS,
  DatasetStatus'
  #-}
