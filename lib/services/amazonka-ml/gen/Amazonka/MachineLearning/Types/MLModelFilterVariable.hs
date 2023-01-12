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
-- Module      : Amazonka.MachineLearning.Types.MLModelFilterVariable
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MachineLearning.Types.MLModelFilterVariable
  ( MLModelFilterVariable
      ( ..,
        MLModelFilterVariable_Algorithm,
        MLModelFilterVariable_CreatedAt,
        MLModelFilterVariable_IAMUser,
        MLModelFilterVariable_LastUpdatedAt,
        MLModelFilterVariable_MLModelType,
        MLModelFilterVariable_Name,
        MLModelFilterVariable_RealtimeEndpointStatus,
        MLModelFilterVariable_Status,
        MLModelFilterVariable_TrainingDataSourceId,
        MLModelFilterVariable_TrainingDataURI
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype MLModelFilterVariable = MLModelFilterVariable'
  { fromMLModelFilterVariable ::
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

pattern MLModelFilterVariable_Algorithm :: MLModelFilterVariable
pattern MLModelFilterVariable_Algorithm = MLModelFilterVariable' "Algorithm"

pattern MLModelFilterVariable_CreatedAt :: MLModelFilterVariable
pattern MLModelFilterVariable_CreatedAt = MLModelFilterVariable' "CreatedAt"

pattern MLModelFilterVariable_IAMUser :: MLModelFilterVariable
pattern MLModelFilterVariable_IAMUser = MLModelFilterVariable' "IAMUser"

pattern MLModelFilterVariable_LastUpdatedAt :: MLModelFilterVariable
pattern MLModelFilterVariable_LastUpdatedAt = MLModelFilterVariable' "LastUpdatedAt"

pattern MLModelFilterVariable_MLModelType :: MLModelFilterVariable
pattern MLModelFilterVariable_MLModelType = MLModelFilterVariable' "MLModelType"

pattern MLModelFilterVariable_Name :: MLModelFilterVariable
pattern MLModelFilterVariable_Name = MLModelFilterVariable' "Name"

pattern MLModelFilterVariable_RealtimeEndpointStatus :: MLModelFilterVariable
pattern MLModelFilterVariable_RealtimeEndpointStatus = MLModelFilterVariable' "RealtimeEndpointStatus"

pattern MLModelFilterVariable_Status :: MLModelFilterVariable
pattern MLModelFilterVariable_Status = MLModelFilterVariable' "Status"

pattern MLModelFilterVariable_TrainingDataSourceId :: MLModelFilterVariable
pattern MLModelFilterVariable_TrainingDataSourceId = MLModelFilterVariable' "TrainingDataSourceId"

pattern MLModelFilterVariable_TrainingDataURI :: MLModelFilterVariable
pattern MLModelFilterVariable_TrainingDataURI = MLModelFilterVariable' "TrainingDataURI"

{-# COMPLETE
  MLModelFilterVariable_Algorithm,
  MLModelFilterVariable_CreatedAt,
  MLModelFilterVariable_IAMUser,
  MLModelFilterVariable_LastUpdatedAt,
  MLModelFilterVariable_MLModelType,
  MLModelFilterVariable_Name,
  MLModelFilterVariable_RealtimeEndpointStatus,
  MLModelFilterVariable_Status,
  MLModelFilterVariable_TrainingDataSourceId,
  MLModelFilterVariable_TrainingDataURI,
  MLModelFilterVariable'
  #-}
