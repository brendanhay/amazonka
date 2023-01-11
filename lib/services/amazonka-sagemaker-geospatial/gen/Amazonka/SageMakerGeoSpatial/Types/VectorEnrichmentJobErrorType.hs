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
-- Module      : Amazonka.SageMakerGeoSpatial.Types.VectorEnrichmentJobErrorType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.VectorEnrichmentJobErrorType
  ( VectorEnrichmentJobErrorType
      ( ..,
        VectorEnrichmentJobErrorType_CLIENT_ERROR,
        VectorEnrichmentJobErrorType_SERVER_ERROR
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype VectorEnrichmentJobErrorType = VectorEnrichmentJobErrorType'
  { fromVectorEnrichmentJobErrorType ::
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

pattern VectorEnrichmentJobErrorType_CLIENT_ERROR :: VectorEnrichmentJobErrorType
pattern VectorEnrichmentJobErrorType_CLIENT_ERROR = VectorEnrichmentJobErrorType' "CLIENT_ERROR"

pattern VectorEnrichmentJobErrorType_SERVER_ERROR :: VectorEnrichmentJobErrorType
pattern VectorEnrichmentJobErrorType_SERVER_ERROR = VectorEnrichmentJobErrorType' "SERVER_ERROR"

{-# COMPLETE
  VectorEnrichmentJobErrorType_CLIENT_ERROR,
  VectorEnrichmentJobErrorType_SERVER_ERROR,
  VectorEnrichmentJobErrorType'
  #-}
