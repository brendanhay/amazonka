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
-- Module      : Amazonka.SageMakerGeoSpatial.Types.VectorEnrichmentJobType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.VectorEnrichmentJobType
  ( VectorEnrichmentJobType
      ( ..,
        VectorEnrichmentJobType_MAP_MATCHING,
        VectorEnrichmentJobType_REVERSE_GEOCODING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype VectorEnrichmentJobType = VectorEnrichmentJobType'
  { fromVectorEnrichmentJobType ::
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

pattern VectorEnrichmentJobType_MAP_MATCHING :: VectorEnrichmentJobType
pattern VectorEnrichmentJobType_MAP_MATCHING = VectorEnrichmentJobType' "MAP_MATCHING"

pattern VectorEnrichmentJobType_REVERSE_GEOCODING :: VectorEnrichmentJobType
pattern VectorEnrichmentJobType_REVERSE_GEOCODING = VectorEnrichmentJobType' "REVERSE_GEOCODING"

{-# COMPLETE
  VectorEnrichmentJobType_MAP_MATCHING,
  VectorEnrichmentJobType_REVERSE_GEOCODING,
  VectorEnrichmentJobType'
  #-}
