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
-- Module      : Amazonka.SageMakerGeoSpatial.Types.VectorEnrichmentJobExportStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.VectorEnrichmentJobExportStatus
  ( VectorEnrichmentJobExportStatus
      ( ..,
        VectorEnrichmentJobExportStatus_FAILED,
        VectorEnrichmentJobExportStatus_IN_PROGRESS,
        VectorEnrichmentJobExportStatus_SUCCEEDED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype VectorEnrichmentJobExportStatus = VectorEnrichmentJobExportStatus'
  { fromVectorEnrichmentJobExportStatus ::
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

pattern VectorEnrichmentJobExportStatus_FAILED :: VectorEnrichmentJobExportStatus
pattern VectorEnrichmentJobExportStatus_FAILED = VectorEnrichmentJobExportStatus' "FAILED"

pattern VectorEnrichmentJobExportStatus_IN_PROGRESS :: VectorEnrichmentJobExportStatus
pattern VectorEnrichmentJobExportStatus_IN_PROGRESS = VectorEnrichmentJobExportStatus' "IN_PROGRESS"

pattern VectorEnrichmentJobExportStatus_SUCCEEDED :: VectorEnrichmentJobExportStatus
pattern VectorEnrichmentJobExportStatus_SUCCEEDED = VectorEnrichmentJobExportStatus' "SUCCEEDED"

{-# COMPLETE
  VectorEnrichmentJobExportStatus_FAILED,
  VectorEnrichmentJobExportStatus_IN_PROGRESS,
  VectorEnrichmentJobExportStatus_SUCCEEDED,
  VectorEnrichmentJobExportStatus'
  #-}
