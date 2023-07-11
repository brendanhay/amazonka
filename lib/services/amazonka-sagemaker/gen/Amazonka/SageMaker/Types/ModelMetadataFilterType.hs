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
-- Module      : Amazonka.SageMaker.Types.ModelMetadataFilterType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ModelMetadataFilterType
  ( ModelMetadataFilterType
      ( ..,
        ModelMetadataFilterType_Domain,
        ModelMetadataFilterType_Framework,
        ModelMetadataFilterType_FrameworkVersion,
        ModelMetadataFilterType_Task
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ModelMetadataFilterType = ModelMetadataFilterType'
  { fromModelMetadataFilterType ::
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

pattern ModelMetadataFilterType_Domain :: ModelMetadataFilterType
pattern ModelMetadataFilterType_Domain = ModelMetadataFilterType' "Domain"

pattern ModelMetadataFilterType_Framework :: ModelMetadataFilterType
pattern ModelMetadataFilterType_Framework = ModelMetadataFilterType' "Framework"

pattern ModelMetadataFilterType_FrameworkVersion :: ModelMetadataFilterType
pattern ModelMetadataFilterType_FrameworkVersion = ModelMetadataFilterType' "FrameworkVersion"

pattern ModelMetadataFilterType_Task :: ModelMetadataFilterType
pattern ModelMetadataFilterType_Task = ModelMetadataFilterType' "Task"

{-# COMPLETE
  ModelMetadataFilterType_Domain,
  ModelMetadataFilterType_Framework,
  ModelMetadataFilterType_FrameworkVersion,
  ModelMetadataFilterType_Task,
  ModelMetadataFilterType'
  #-}
