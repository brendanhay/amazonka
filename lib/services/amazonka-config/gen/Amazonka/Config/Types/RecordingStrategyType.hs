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
-- Module      : Amazonka.Config.Types.RecordingStrategyType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.RecordingStrategyType
  ( RecordingStrategyType
      ( ..,
        RecordingStrategyType_ALL_SUPPORTED_RESOURCE_TYPES,
        RecordingStrategyType_EXCLUSION_BY_RESOURCE_TYPES,
        RecordingStrategyType_INCLUSION_BY_RESOURCE_TYPES
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype RecordingStrategyType = RecordingStrategyType'
  { fromRecordingStrategyType ::
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

pattern RecordingStrategyType_ALL_SUPPORTED_RESOURCE_TYPES :: RecordingStrategyType
pattern RecordingStrategyType_ALL_SUPPORTED_RESOURCE_TYPES = RecordingStrategyType' "ALL_SUPPORTED_RESOURCE_TYPES"

pattern RecordingStrategyType_EXCLUSION_BY_RESOURCE_TYPES :: RecordingStrategyType
pattern RecordingStrategyType_EXCLUSION_BY_RESOURCE_TYPES = RecordingStrategyType' "EXCLUSION_BY_RESOURCE_TYPES"

pattern RecordingStrategyType_INCLUSION_BY_RESOURCE_TYPES :: RecordingStrategyType
pattern RecordingStrategyType_INCLUSION_BY_RESOURCE_TYPES = RecordingStrategyType' "INCLUSION_BY_RESOURCE_TYPES"

{-# COMPLETE
  RecordingStrategyType_ALL_SUPPORTED_RESOURCE_TYPES,
  RecordingStrategyType_EXCLUSION_BY_RESOURCE_TYPES,
  RecordingStrategyType_INCLUSION_BY_RESOURCE_TYPES,
  RecordingStrategyType'
  #-}
