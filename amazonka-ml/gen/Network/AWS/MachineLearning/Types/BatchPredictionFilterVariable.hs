{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.BatchPredictionFilterVariable
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Types.BatchPredictionFilterVariable
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

import qualified Network.AWS.Prelude as Prelude

-- | A list of the variables to use in searching or filtering
-- @BatchPrediction@.
--
-- -   @CreatedAt@ - Sets the search criteria to @BatchPrediction@ creation
--     date.
-- -   @Status@ - Sets the search criteria to @BatchPrediction@ status.
-- -   @Name@ - Sets the search criteria to the contents of
--     @BatchPrediction@ ____ @Name@.
-- -   @IAMUser@ - Sets the search criteria to the user account that
--     invoked the @BatchPrediction@ creation.
-- -   @MLModelId@ - Sets the search criteria to the @MLModel@ used in the
--     @BatchPrediction@.
-- -   @DataSourceId@ - Sets the search criteria to the @DataSource@ used
--     in the @BatchPrediction@.
-- -   @DataURI@ - Sets the search criteria to the data file(s) used in the
--     @BatchPrediction@. The URL can identify either a file or an Amazon
--     Simple Storage Service (Amazon S3) bucket or directory.
newtype BatchPredictionFilterVariable = BatchPredictionFilterVariable'
  { fromBatchPredictionFilterVariable ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
