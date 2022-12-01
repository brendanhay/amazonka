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
-- Module      : Amazonka.Textract.Types.FeatureType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Textract.Types.FeatureType
  ( FeatureType
      ( ..,
        FeatureType_FORMS,
        FeatureType_QUERIES,
        FeatureType_SIGNATURES,
        FeatureType_TABLES
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype FeatureType = FeatureType'
  { fromFeatureType ::
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

pattern FeatureType_FORMS :: FeatureType
pattern FeatureType_FORMS = FeatureType' "FORMS"

pattern FeatureType_QUERIES :: FeatureType
pattern FeatureType_QUERIES = FeatureType' "QUERIES"

pattern FeatureType_SIGNATURES :: FeatureType
pattern FeatureType_SIGNATURES = FeatureType' "SIGNATURES"

pattern FeatureType_TABLES :: FeatureType
pattern FeatureType_TABLES = FeatureType' "TABLES"

{-# COMPLETE
  FeatureType_FORMS,
  FeatureType_QUERIES,
  FeatureType_SIGNATURES,
  FeatureType_TABLES,
  FeatureType'
  #-}
