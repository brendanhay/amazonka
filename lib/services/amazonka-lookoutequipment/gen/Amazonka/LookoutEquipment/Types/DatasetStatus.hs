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
-- Module      : Amazonka.LookoutEquipment.Types.DatasetStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutEquipment.Types.DatasetStatus
  ( DatasetStatus
      ( ..,
        DatasetStatus_ACTIVE,
        DatasetStatus_CREATED,
        DatasetStatus_INGESTION_IN_PROGRESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

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

pattern DatasetStatus_ACTIVE :: DatasetStatus
pattern DatasetStatus_ACTIVE = DatasetStatus' "ACTIVE"

pattern DatasetStatus_CREATED :: DatasetStatus
pattern DatasetStatus_CREATED = DatasetStatus' "CREATED"

pattern DatasetStatus_INGESTION_IN_PROGRESS :: DatasetStatus
pattern DatasetStatus_INGESTION_IN_PROGRESS = DatasetStatus' "INGESTION_IN_PROGRESS"

{-# COMPLETE
  DatasetStatus_ACTIVE,
  DatasetStatus_CREATED,
  DatasetStatus_INGESTION_IN_PROGRESS,
  DatasetStatus'
  #-}
