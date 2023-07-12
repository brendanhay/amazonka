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
-- Module      : Amazonka.MachineLearning.Types.BatchPredictionFilterVariable
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MachineLearning.Types.BatchPredictionFilterVariable
  ( BatchPredictionFilterVariable
      ( ..,
        BatchPredictionFilterVariable_CreatedAt,
        BatchPredictionFilterVariable_DataSourceId,
        BatchPredictionFilterVariable_DataURI,
        BatchPredictionFilterVariable_IAMUser,
        BatchPredictionFilterVariable_LastUpdatedAt,
        BatchPredictionFilterVariable_MLModelId,
        BatchPredictionFilterVariable_Name,
        BatchPredictionFilterVariable_Status
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A list of the variables to use in searching or filtering
-- @BatchPrediction@.
--
-- -   @CreatedAt@ - Sets the search criteria to @BatchPrediction@ creation
--     date.
--
-- -   @Status@ - Sets the search criteria to @BatchPrediction@ status.
--
-- -   @Name@ - Sets the search criteria to the contents of
--     @BatchPrediction@ @Name@.
--
-- -   @IAMUser@ - Sets the search criteria to the user account that
--     invoked the @BatchPrediction@ creation.
--
-- -   @MLModelId@ - Sets the search criteria to the @MLModel@ used in the
--     @BatchPrediction@.
--
-- -   @DataSourceId@ - Sets the search criteria to the @DataSource@ used
--     in the @BatchPrediction@.
--
-- -   @DataURI@ - Sets the search criteria to the data file(s) used in the
--     @BatchPrediction@. The URL can identify either a file or an Amazon
--     Simple Storage Service (Amazon S3) bucket or directory.
newtype BatchPredictionFilterVariable = BatchPredictionFilterVariable'
  { fromBatchPredictionFilterVariable ::
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

pattern BatchPredictionFilterVariable_CreatedAt :: BatchPredictionFilterVariable
pattern BatchPredictionFilterVariable_CreatedAt = BatchPredictionFilterVariable' "CreatedAt"

pattern BatchPredictionFilterVariable_DataSourceId :: BatchPredictionFilterVariable
pattern BatchPredictionFilterVariable_DataSourceId = BatchPredictionFilterVariable' "DataSourceId"

pattern BatchPredictionFilterVariable_DataURI :: BatchPredictionFilterVariable
pattern BatchPredictionFilterVariable_DataURI = BatchPredictionFilterVariable' "DataURI"

pattern BatchPredictionFilterVariable_IAMUser :: BatchPredictionFilterVariable
pattern BatchPredictionFilterVariable_IAMUser = BatchPredictionFilterVariable' "IAMUser"

pattern BatchPredictionFilterVariable_LastUpdatedAt :: BatchPredictionFilterVariable
pattern BatchPredictionFilterVariable_LastUpdatedAt = BatchPredictionFilterVariable' "LastUpdatedAt"

pattern BatchPredictionFilterVariable_MLModelId :: BatchPredictionFilterVariable
pattern BatchPredictionFilterVariable_MLModelId = BatchPredictionFilterVariable' "MLModelId"

pattern BatchPredictionFilterVariable_Name :: BatchPredictionFilterVariable
pattern BatchPredictionFilterVariable_Name = BatchPredictionFilterVariable' "Name"

pattern BatchPredictionFilterVariable_Status :: BatchPredictionFilterVariable
pattern BatchPredictionFilterVariable_Status = BatchPredictionFilterVariable' "Status"

{-# COMPLETE
  BatchPredictionFilterVariable_CreatedAt,
  BatchPredictionFilterVariable_DataSourceId,
  BatchPredictionFilterVariable_DataURI,
  BatchPredictionFilterVariable_IAMUser,
  BatchPredictionFilterVariable_LastUpdatedAt,
  BatchPredictionFilterVariable_MLModelId,
  BatchPredictionFilterVariable_Name,
  BatchPredictionFilterVariable_Status,
  BatchPredictionFilterVariable'
  #-}
