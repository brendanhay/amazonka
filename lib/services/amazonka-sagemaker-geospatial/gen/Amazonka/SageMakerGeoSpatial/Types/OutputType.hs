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
-- Module      : Amazonka.SageMakerGeoSpatial.Types.OutputType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.OutputType
  ( OutputType
      ( ..,
        OutputType_FLOAT32,
        OutputType_FLOAT64,
        OutputType_INT16,
        OutputType_INT32,
        OutputType_UINT16
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype OutputType = OutputType'
  { fromOutputType ::
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

pattern OutputType_FLOAT32 :: OutputType
pattern OutputType_FLOAT32 = OutputType' "FLOAT32"

pattern OutputType_FLOAT64 :: OutputType
pattern OutputType_FLOAT64 = OutputType' "FLOAT64"

pattern OutputType_INT16 :: OutputType
pattern OutputType_INT16 = OutputType' "INT16"

pattern OutputType_INT32 :: OutputType
pattern OutputType_INT32 = OutputType' "INT32"

pattern OutputType_UINT16 :: OutputType
pattern OutputType_UINT16 = OutputType' "UINT16"

{-# COMPLETE
  OutputType_FLOAT32,
  OutputType_FLOAT64,
  OutputType_INT16,
  OutputType_INT32,
  OutputType_UINT16,
  OutputType'
  #-}
