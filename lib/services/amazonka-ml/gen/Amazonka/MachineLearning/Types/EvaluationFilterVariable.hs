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
-- Module      : Amazonka.MachineLearning.Types.EvaluationFilterVariable
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MachineLearning.Types.EvaluationFilterVariable
  ( EvaluationFilterVariable
      ( ..,
        EvaluationFilterVariable_CreatedAt,
        EvaluationFilterVariable_DataSourceId,
        EvaluationFilterVariable_DataURI,
        EvaluationFilterVariable_IAMUser,
        EvaluationFilterVariable_LastUpdatedAt,
        EvaluationFilterVariable_MLModelId,
        EvaluationFilterVariable_Name,
        EvaluationFilterVariable_Status
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A list of the variables to use in searching or filtering @Evaluation@.
--
-- -   @CreatedAt@ - Sets the search criteria to @Evaluation@ creation
--     date.
--
-- -   @Status@ - Sets the search criteria to @Evaluation@ status.
--
-- -   @Name@ - Sets the search criteria to the contents of @Evaluation@
--     ____ @Name@.
--
-- -   @IAMUser@ - Sets the search criteria to the user account that
--     invoked an evaluation.
--
-- -   @MLModelId@ - Sets the search criteria to the @Predictor@ that was
--     evaluated.
--
-- -   @DataSourceId@ - Sets the search criteria to the @DataSource@ used
--     in evaluation.
--
-- -   @DataUri@ - Sets the search criteria to the data file(s) used in
--     evaluation. The URL can identify either a file or an Amazon Simple
--     Storage Service (Amazon S3) bucket or directory.
newtype EvaluationFilterVariable = EvaluationFilterVariable'
  { fromEvaluationFilterVariable ::
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

pattern EvaluationFilterVariable_CreatedAt :: EvaluationFilterVariable
pattern EvaluationFilterVariable_CreatedAt = EvaluationFilterVariable' "CreatedAt"

pattern EvaluationFilterVariable_DataSourceId :: EvaluationFilterVariable
pattern EvaluationFilterVariable_DataSourceId = EvaluationFilterVariable' "DataSourceId"

pattern EvaluationFilterVariable_DataURI :: EvaluationFilterVariable
pattern EvaluationFilterVariable_DataURI = EvaluationFilterVariable' "DataURI"

pattern EvaluationFilterVariable_IAMUser :: EvaluationFilterVariable
pattern EvaluationFilterVariable_IAMUser = EvaluationFilterVariable' "IAMUser"

pattern EvaluationFilterVariable_LastUpdatedAt :: EvaluationFilterVariable
pattern EvaluationFilterVariable_LastUpdatedAt = EvaluationFilterVariable' "LastUpdatedAt"

pattern EvaluationFilterVariable_MLModelId :: EvaluationFilterVariable
pattern EvaluationFilterVariable_MLModelId = EvaluationFilterVariable' "MLModelId"

pattern EvaluationFilterVariable_Name :: EvaluationFilterVariable
pattern EvaluationFilterVariable_Name = EvaluationFilterVariable' "Name"

pattern EvaluationFilterVariable_Status :: EvaluationFilterVariable
pattern EvaluationFilterVariable_Status = EvaluationFilterVariable' "Status"

{-# COMPLETE
  EvaluationFilterVariable_CreatedAt,
  EvaluationFilterVariable_DataSourceId,
  EvaluationFilterVariable_DataURI,
  EvaluationFilterVariable_IAMUser,
  EvaluationFilterVariable_LastUpdatedAt,
  EvaluationFilterVariable_MLModelId,
  EvaluationFilterVariable_Name,
  EvaluationFilterVariable_Status,
  EvaluationFilterVariable'
  #-}
