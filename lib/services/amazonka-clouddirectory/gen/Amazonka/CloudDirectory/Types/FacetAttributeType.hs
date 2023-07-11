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
-- Module      : Amazonka.CloudDirectory.Types.FacetAttributeType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types.FacetAttributeType
  ( FacetAttributeType
      ( ..,
        FacetAttributeType_BINARY,
        FacetAttributeType_BOOLEAN,
        FacetAttributeType_DATETIME,
        FacetAttributeType_NUMBER,
        FacetAttributeType_STRING,
        FacetAttributeType_VARIANT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype FacetAttributeType = FacetAttributeType'
  { fromFacetAttributeType ::
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

pattern FacetAttributeType_BINARY :: FacetAttributeType
pattern FacetAttributeType_BINARY = FacetAttributeType' "BINARY"

pattern FacetAttributeType_BOOLEAN :: FacetAttributeType
pattern FacetAttributeType_BOOLEAN = FacetAttributeType' "BOOLEAN"

pattern FacetAttributeType_DATETIME :: FacetAttributeType
pattern FacetAttributeType_DATETIME = FacetAttributeType' "DATETIME"

pattern FacetAttributeType_NUMBER :: FacetAttributeType
pattern FacetAttributeType_NUMBER = FacetAttributeType' "NUMBER"

pattern FacetAttributeType_STRING :: FacetAttributeType
pattern FacetAttributeType_STRING = FacetAttributeType' "STRING"

pattern FacetAttributeType_VARIANT :: FacetAttributeType
pattern FacetAttributeType_VARIANT = FacetAttributeType' "VARIANT"

{-# COMPLETE
  FacetAttributeType_BINARY,
  FacetAttributeType_BOOLEAN,
  FacetAttributeType_DATETIME,
  FacetAttributeType_NUMBER,
  FacetAttributeType_STRING,
  FacetAttributeType_VARIANT,
  FacetAttributeType'
  #-}
