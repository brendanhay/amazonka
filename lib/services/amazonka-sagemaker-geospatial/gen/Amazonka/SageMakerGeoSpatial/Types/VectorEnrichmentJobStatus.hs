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
-- Module      : Amazonka.SageMakerGeoSpatial.Types.VectorEnrichmentJobStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.VectorEnrichmentJobStatus
  ( VectorEnrichmentJobStatus
      ( ..,
        VectorEnrichmentJobStatus_COMPLETED,
        VectorEnrichmentJobStatus_DELETED,
        VectorEnrichmentJobStatus_DELETING,
        VectorEnrichmentJobStatus_FAILED,
        VectorEnrichmentJobStatus_INITIALIZING,
        VectorEnrichmentJobStatus_IN_PROGRESS,
        VectorEnrichmentJobStatus_STOPPED,
        VectorEnrichmentJobStatus_STOPPING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype VectorEnrichmentJobStatus = VectorEnrichmentJobStatus'
  { fromVectorEnrichmentJobStatus ::
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

pattern VectorEnrichmentJobStatus_COMPLETED :: VectorEnrichmentJobStatus
pattern VectorEnrichmentJobStatus_COMPLETED = VectorEnrichmentJobStatus' "COMPLETED"

pattern VectorEnrichmentJobStatus_DELETED :: VectorEnrichmentJobStatus
pattern VectorEnrichmentJobStatus_DELETED = VectorEnrichmentJobStatus' "DELETED"

pattern VectorEnrichmentJobStatus_DELETING :: VectorEnrichmentJobStatus
pattern VectorEnrichmentJobStatus_DELETING = VectorEnrichmentJobStatus' "DELETING"

pattern VectorEnrichmentJobStatus_FAILED :: VectorEnrichmentJobStatus
pattern VectorEnrichmentJobStatus_FAILED = VectorEnrichmentJobStatus' "FAILED"

pattern VectorEnrichmentJobStatus_INITIALIZING :: VectorEnrichmentJobStatus
pattern VectorEnrichmentJobStatus_INITIALIZING = VectorEnrichmentJobStatus' "INITIALIZING"

pattern VectorEnrichmentJobStatus_IN_PROGRESS :: VectorEnrichmentJobStatus
pattern VectorEnrichmentJobStatus_IN_PROGRESS = VectorEnrichmentJobStatus' "IN_PROGRESS"

pattern VectorEnrichmentJobStatus_STOPPED :: VectorEnrichmentJobStatus
pattern VectorEnrichmentJobStatus_STOPPED = VectorEnrichmentJobStatus' "STOPPED"

pattern VectorEnrichmentJobStatus_STOPPING :: VectorEnrichmentJobStatus
pattern VectorEnrichmentJobStatus_STOPPING = VectorEnrichmentJobStatus' "STOPPING"

{-# COMPLETE
  VectorEnrichmentJobStatus_COMPLETED,
  VectorEnrichmentJobStatus_DELETED,
  VectorEnrichmentJobStatus_DELETING,
  VectorEnrichmentJobStatus_FAILED,
  VectorEnrichmentJobStatus_INITIALIZING,
  VectorEnrichmentJobStatus_IN_PROGRESS,
  VectorEnrichmentJobStatus_STOPPED,
  VectorEnrichmentJobStatus_STOPPING,
  VectorEnrichmentJobStatus'
  #-}
