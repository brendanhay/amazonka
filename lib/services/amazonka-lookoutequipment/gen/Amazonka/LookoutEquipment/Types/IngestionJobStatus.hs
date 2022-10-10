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
-- Module      : Amazonka.LookoutEquipment.Types.IngestionJobStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutEquipment.Types.IngestionJobStatus
  ( IngestionJobStatus
      ( ..,
        IngestionJobStatus_FAILED,
        IngestionJobStatus_IN_PROGRESS,
        IngestionJobStatus_SUCCESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype IngestionJobStatus = IngestionJobStatus'
  { fromIngestionJobStatus ::
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

pattern IngestionJobStatus_FAILED :: IngestionJobStatus
pattern IngestionJobStatus_FAILED = IngestionJobStatus' "FAILED"

pattern IngestionJobStatus_IN_PROGRESS :: IngestionJobStatus
pattern IngestionJobStatus_IN_PROGRESS = IngestionJobStatus' "IN_PROGRESS"

pattern IngestionJobStatus_SUCCESS :: IngestionJobStatus
pattern IngestionJobStatus_SUCCESS = IngestionJobStatus' "SUCCESS"

{-# COMPLETE
  IngestionJobStatus_FAILED,
  IngestionJobStatus_IN_PROGRESS,
  IngestionJobStatus_SUCCESS,
  IngestionJobStatus'
  #-}
