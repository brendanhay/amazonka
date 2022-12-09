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
-- Module      : Amazonka.SageMakerGeoSpatial.Types.VectorEnrichmentJobExportErrorType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.VectorEnrichmentJobExportErrorType
  ( VectorEnrichmentJobExportErrorType
      ( ..,
        VectorEnrichmentJobExportErrorType_CLIENT_ERROR,
        VectorEnrichmentJobExportErrorType_SERVER_ERROR
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype VectorEnrichmentJobExportErrorType = VectorEnrichmentJobExportErrorType'
  { fromVectorEnrichmentJobExportErrorType ::
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

pattern VectorEnrichmentJobExportErrorType_CLIENT_ERROR :: VectorEnrichmentJobExportErrorType
pattern VectorEnrichmentJobExportErrorType_CLIENT_ERROR = VectorEnrichmentJobExportErrorType' "CLIENT_ERROR"

pattern VectorEnrichmentJobExportErrorType_SERVER_ERROR :: VectorEnrichmentJobExportErrorType
pattern VectorEnrichmentJobExportErrorType_SERVER_ERROR = VectorEnrichmentJobExportErrorType' "SERVER_ERROR"

{-# COMPLETE
  VectorEnrichmentJobExportErrorType_CLIENT_ERROR,
  VectorEnrichmentJobExportErrorType_SERVER_ERROR,
  VectorEnrichmentJobExportErrorType'
  #-}
