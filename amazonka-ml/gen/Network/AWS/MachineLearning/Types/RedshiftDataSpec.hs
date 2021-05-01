{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.RedshiftDataSpec
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Types.RedshiftDataSpec where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types.RedshiftDatabase
import Network.AWS.MachineLearning.Types.RedshiftDatabaseCredentials
import qualified Network.AWS.Prelude as Prelude

-- | Describes the data specification of an Amazon Redshift @DataSource@.
--
-- /See:/ 'newRedshiftDataSpec' smart constructor.
data RedshiftDataSpec = RedshiftDataSpec'
  { -- | A JSON string that represents the splitting and rearrangement processing
    -- to be applied to a @DataSource@. If the @DataRearrangement@ parameter is
    -- not provided, all of the input data is used to create the @Datasource@.
    --
    -- There are multiple parameters that control what data is used to create a
    -- datasource:
    --
    -- -   __@percentBegin@__
    --
    --     Use @percentBegin@ to indicate the beginning of the range of the
    --     data used to create the Datasource. If you do not include
    --     @percentBegin@ and @percentEnd@, Amazon ML includes all of the data
    --     when creating the datasource.
    --
    -- -   __@percentEnd@__
    --
    --     Use @percentEnd@ to indicate the end of the range of the data used
    --     to create the Datasource. If you do not include @percentBegin@ and
    --     @percentEnd@, Amazon ML includes all of the data when creating the
    --     datasource.
    --
    -- -   __@complement@__
    --
    --     The @complement@ parameter instructs Amazon ML to use the data that
    --     is not included in the range of @percentBegin@ to @percentEnd@ to
    --     create a datasource. The @complement@ parameter is useful if you
    --     need to create complementary datasources for training and
    --     evaluation. To create a complementary datasource, use the same
    --     values for @percentBegin@ and @percentEnd@, along with the
    --     @complement@ parameter.
    --
    --     For example, the following two datasources do not share any data,
    --     and can be used to train and evaluate a model. The first datasource
    --     has 25 percent of the data, and the second one has 75 percent of the
    --     data.
    --
    --     Datasource for evaluation:
    --     @{\"splitting\":{\"percentBegin\":0, \"percentEnd\":25}}@
    --
    --     Datasource for training:
    --     @{\"splitting\":{\"percentBegin\":0, \"percentEnd\":25, \"complement\":\"true\"}}@
    --
    -- -   __@strategy@__
    --
    --     To change how Amazon ML splits the data for a datasource, use the
    --     @strategy@ parameter.
    --
    --     The default value for the @strategy@ parameter is @sequential@,
    --     meaning that Amazon ML takes all of the data records between the
    --     @percentBegin@ and @percentEnd@ parameters for the datasource, in
    --     the order that the records appear in the input data.
    --
    --     The following two @DataRearrangement@ lines are examples of
    --     sequentially ordered training and evaluation datasources:
    --
    --     Datasource for evaluation:
    --     @{\"splitting\":{\"percentBegin\":70, \"percentEnd\":100, \"strategy\":\"sequential\"}}@
    --
    --     Datasource for training:
    --     @{\"splitting\":{\"percentBegin\":70, \"percentEnd\":100, \"strategy\":\"sequential\", \"complement\":\"true\"}}@
    --
    --     To randomly split the input data into the proportions indicated by
    --     the percentBegin and percentEnd parameters, set the @strategy@
    --     parameter to @random@ and provide a string that is used as the seed
    --     value for the random data splitting (for example, you can use the S3
    --     path to your data as the random seed string). If you choose the
    --     random split strategy, Amazon ML assigns each row of data a
    --     pseudo-random number between 0 and 100, and then selects the rows
    --     that have an assigned number between @percentBegin@ and
    --     @percentEnd@. Pseudo-random numbers are assigned using both the
    --     input seed string value and the byte offset as a seed, so changing
    --     the data results in a different split. Any existing ordering is
    --     preserved. The random splitting strategy ensures that variables in
    --     the training and evaluation data are distributed similarly. It is
    --     useful in the cases where the input data may have an implicit sort
    --     order, which would otherwise result in training and evaluation
    --     datasources containing non-similar data records.
    --
    --     The following two @DataRearrangement@ lines are examples of
    --     non-sequentially ordered training and evaluation datasources:
    --
    --     Datasource for evaluation:
    --     @{\"splitting\":{\"percentBegin\":70, \"percentEnd\":100, \"strategy\":\"random\", \"randomSeed\"=\"s3:\/\/my_s3_path\/bucket\/file.csv\"}}@
    --
    --     Datasource for training:
    --     @{\"splitting\":{\"percentBegin\":70, \"percentEnd\":100, \"strategy\":\"random\", \"randomSeed\"=\"s3:\/\/my_s3_path\/bucket\/file.csv\", \"complement\":\"true\"}}@
    dataRearrangement :: Prelude.Maybe Prelude.Text,
    -- | A JSON string that represents the schema for an Amazon Redshift
    -- @DataSource@. The @DataSchema@ defines the structure of the observation
    -- data in the data file(s) referenced in the @DataSource@.
    --
    -- A @DataSchema@ is not required if you specify a @DataSchemaUri@.
    --
    -- Define your @DataSchema@ as a series of key-value pairs. @attributes@
    -- and @excludedVariableNames@ have an array of key-value pairs for their
    -- value. Use the following format to define your @DataSchema@.
    --
    -- { \"version\": \"1.0\",
    --
    -- \"recordAnnotationFieldName\": \"F1\",
    --
    -- \"recordWeightFieldName\": \"F2\",
    --
    -- \"targetFieldName\": \"F3\",
    --
    -- \"dataFormat\": \"CSV\",
    --
    -- \"dataFileContainsHeader\": true,
    --
    -- \"attributes\": [
    --
    -- { \"fieldName\": \"F1\", \"fieldType\": \"TEXT\" }, { \"fieldName\":
    -- \"F2\", \"fieldType\": \"NUMERIC\" }, { \"fieldName\": \"F3\",
    -- \"fieldType\": \"CATEGORICAL\" }, { \"fieldName\": \"F4\",
    -- \"fieldType\": \"NUMERIC\" }, { \"fieldName\": \"F5\", \"fieldType\":
    -- \"CATEGORICAL\" }, { \"fieldName\": \"F6\", \"fieldType\": \"TEXT\" }, {
    -- \"fieldName\": \"F7\", \"fieldType\": \"WEIGHTED_INT_SEQUENCE\" }, {
    -- \"fieldName\": \"F8\", \"fieldType\": \"WEIGHTED_STRING_SEQUENCE\" } ],
    --
    -- \"excludedVariableNames\": [ \"F6\" ] }
    dataSchema :: Prelude.Maybe Prelude.Text,
    -- | Describes the schema location for an Amazon Redshift @DataSource@.
    dataSchemaUri :: Prelude.Maybe Prelude.Text,
    -- | Describes the @DatabaseName@ and @ClusterIdentifier@ for an Amazon
    -- Redshift @DataSource@.
    databaseInformation :: RedshiftDatabase,
    -- | Describes the SQL Query to execute on an Amazon Redshift database for an
    -- Amazon Redshift @DataSource@.
    selectSqlQuery :: Prelude.Text,
    -- | Describes AWS Identity and Access Management (IAM) credentials that are
    -- used connect to the Amazon Redshift database.
    databaseCredentials :: RedshiftDatabaseCredentials,
    -- | Describes an Amazon S3 location to store the result set of the
    -- @SelectSqlQuery@ query.
    s3StagingLocation :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RedshiftDataSpec' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataRearrangement', 'redshiftDataSpec_dataRearrangement' - A JSON string that represents the splitting and rearrangement processing
-- to be applied to a @DataSource@. If the @DataRearrangement@ parameter is
-- not provided, all of the input data is used to create the @Datasource@.
--
-- There are multiple parameters that control what data is used to create a
-- datasource:
--
-- -   __@percentBegin@__
--
--     Use @percentBegin@ to indicate the beginning of the range of the
--     data used to create the Datasource. If you do not include
--     @percentBegin@ and @percentEnd@, Amazon ML includes all of the data
--     when creating the datasource.
--
-- -   __@percentEnd@__
--
--     Use @percentEnd@ to indicate the end of the range of the data used
--     to create the Datasource. If you do not include @percentBegin@ and
--     @percentEnd@, Amazon ML includes all of the data when creating the
--     datasource.
--
-- -   __@complement@__
--
--     The @complement@ parameter instructs Amazon ML to use the data that
--     is not included in the range of @percentBegin@ to @percentEnd@ to
--     create a datasource. The @complement@ parameter is useful if you
--     need to create complementary datasources for training and
--     evaluation. To create a complementary datasource, use the same
--     values for @percentBegin@ and @percentEnd@, along with the
--     @complement@ parameter.
--
--     For example, the following two datasources do not share any data,
--     and can be used to train and evaluate a model. The first datasource
--     has 25 percent of the data, and the second one has 75 percent of the
--     data.
--
--     Datasource for evaluation:
--     @{\"splitting\":{\"percentBegin\":0, \"percentEnd\":25}}@
--
--     Datasource for training:
--     @{\"splitting\":{\"percentBegin\":0, \"percentEnd\":25, \"complement\":\"true\"}}@
--
-- -   __@strategy@__
--
--     To change how Amazon ML splits the data for a datasource, use the
--     @strategy@ parameter.
--
--     The default value for the @strategy@ parameter is @sequential@,
--     meaning that Amazon ML takes all of the data records between the
--     @percentBegin@ and @percentEnd@ parameters for the datasource, in
--     the order that the records appear in the input data.
--
--     The following two @DataRearrangement@ lines are examples of
--     sequentially ordered training and evaluation datasources:
--
--     Datasource for evaluation:
--     @{\"splitting\":{\"percentBegin\":70, \"percentEnd\":100, \"strategy\":\"sequential\"}}@
--
--     Datasource for training:
--     @{\"splitting\":{\"percentBegin\":70, \"percentEnd\":100, \"strategy\":\"sequential\", \"complement\":\"true\"}}@
--
--     To randomly split the input data into the proportions indicated by
--     the percentBegin and percentEnd parameters, set the @strategy@
--     parameter to @random@ and provide a string that is used as the seed
--     value for the random data splitting (for example, you can use the S3
--     path to your data as the random seed string). If you choose the
--     random split strategy, Amazon ML assigns each row of data a
--     pseudo-random number between 0 and 100, and then selects the rows
--     that have an assigned number between @percentBegin@ and
--     @percentEnd@. Pseudo-random numbers are assigned using both the
--     input seed string value and the byte offset as a seed, so changing
--     the data results in a different split. Any existing ordering is
--     preserved. The random splitting strategy ensures that variables in
--     the training and evaluation data are distributed similarly. It is
--     useful in the cases where the input data may have an implicit sort
--     order, which would otherwise result in training and evaluation
--     datasources containing non-similar data records.
--
--     The following two @DataRearrangement@ lines are examples of
--     non-sequentially ordered training and evaluation datasources:
--
--     Datasource for evaluation:
--     @{\"splitting\":{\"percentBegin\":70, \"percentEnd\":100, \"strategy\":\"random\", \"randomSeed\"=\"s3:\/\/my_s3_path\/bucket\/file.csv\"}}@
--
--     Datasource for training:
--     @{\"splitting\":{\"percentBegin\":70, \"percentEnd\":100, \"strategy\":\"random\", \"randomSeed\"=\"s3:\/\/my_s3_path\/bucket\/file.csv\", \"complement\":\"true\"}}@
--
-- 'dataSchema', 'redshiftDataSpec_dataSchema' - A JSON string that represents the schema for an Amazon Redshift
-- @DataSource@. The @DataSchema@ defines the structure of the observation
-- data in the data file(s) referenced in the @DataSource@.
--
-- A @DataSchema@ is not required if you specify a @DataSchemaUri@.
--
-- Define your @DataSchema@ as a series of key-value pairs. @attributes@
-- and @excludedVariableNames@ have an array of key-value pairs for their
-- value. Use the following format to define your @DataSchema@.
--
-- { \"version\": \"1.0\",
--
-- \"recordAnnotationFieldName\": \"F1\",
--
-- \"recordWeightFieldName\": \"F2\",
--
-- \"targetFieldName\": \"F3\",
--
-- \"dataFormat\": \"CSV\",
--
-- \"dataFileContainsHeader\": true,
--
-- \"attributes\": [
--
-- { \"fieldName\": \"F1\", \"fieldType\": \"TEXT\" }, { \"fieldName\":
-- \"F2\", \"fieldType\": \"NUMERIC\" }, { \"fieldName\": \"F3\",
-- \"fieldType\": \"CATEGORICAL\" }, { \"fieldName\": \"F4\",
-- \"fieldType\": \"NUMERIC\" }, { \"fieldName\": \"F5\", \"fieldType\":
-- \"CATEGORICAL\" }, { \"fieldName\": \"F6\", \"fieldType\": \"TEXT\" }, {
-- \"fieldName\": \"F7\", \"fieldType\": \"WEIGHTED_INT_SEQUENCE\" }, {
-- \"fieldName\": \"F8\", \"fieldType\": \"WEIGHTED_STRING_SEQUENCE\" } ],
--
-- \"excludedVariableNames\": [ \"F6\" ] }
--
-- 'dataSchemaUri', 'redshiftDataSpec_dataSchemaUri' - Describes the schema location for an Amazon Redshift @DataSource@.
--
-- 'databaseInformation', 'redshiftDataSpec_databaseInformation' - Describes the @DatabaseName@ and @ClusterIdentifier@ for an Amazon
-- Redshift @DataSource@.
--
-- 'selectSqlQuery', 'redshiftDataSpec_selectSqlQuery' - Describes the SQL Query to execute on an Amazon Redshift database for an
-- Amazon Redshift @DataSource@.
--
-- 'databaseCredentials', 'redshiftDataSpec_databaseCredentials' - Describes AWS Identity and Access Management (IAM) credentials that are
-- used connect to the Amazon Redshift database.
--
-- 's3StagingLocation', 'redshiftDataSpec_s3StagingLocation' - Describes an Amazon S3 location to store the result set of the
-- @SelectSqlQuery@ query.
newRedshiftDataSpec ::
  -- | 'databaseInformation'
  RedshiftDatabase ->
  -- | 'selectSqlQuery'
  Prelude.Text ->
  -- | 'databaseCredentials'
  RedshiftDatabaseCredentials ->
  -- | 's3StagingLocation'
  Prelude.Text ->
  RedshiftDataSpec
newRedshiftDataSpec
  pDatabaseInformation_
  pSelectSqlQuery_
  pDatabaseCredentials_
  pS3StagingLocation_ =
    RedshiftDataSpec'
      { dataRearrangement =
          Prelude.Nothing,
        dataSchema = Prelude.Nothing,
        dataSchemaUri = Prelude.Nothing,
        databaseInformation = pDatabaseInformation_,
        selectSqlQuery = pSelectSqlQuery_,
        databaseCredentials = pDatabaseCredentials_,
        s3StagingLocation = pS3StagingLocation_
      }

-- | A JSON string that represents the splitting and rearrangement processing
-- to be applied to a @DataSource@. If the @DataRearrangement@ parameter is
-- not provided, all of the input data is used to create the @Datasource@.
--
-- There are multiple parameters that control what data is used to create a
-- datasource:
--
-- -   __@percentBegin@__
--
--     Use @percentBegin@ to indicate the beginning of the range of the
--     data used to create the Datasource. If you do not include
--     @percentBegin@ and @percentEnd@, Amazon ML includes all of the data
--     when creating the datasource.
--
-- -   __@percentEnd@__
--
--     Use @percentEnd@ to indicate the end of the range of the data used
--     to create the Datasource. If you do not include @percentBegin@ and
--     @percentEnd@, Amazon ML includes all of the data when creating the
--     datasource.
--
-- -   __@complement@__
--
--     The @complement@ parameter instructs Amazon ML to use the data that
--     is not included in the range of @percentBegin@ to @percentEnd@ to
--     create a datasource. The @complement@ parameter is useful if you
--     need to create complementary datasources for training and
--     evaluation. To create a complementary datasource, use the same
--     values for @percentBegin@ and @percentEnd@, along with the
--     @complement@ parameter.
--
--     For example, the following two datasources do not share any data,
--     and can be used to train and evaluate a model. The first datasource
--     has 25 percent of the data, and the second one has 75 percent of the
--     data.
--
--     Datasource for evaluation:
--     @{\"splitting\":{\"percentBegin\":0, \"percentEnd\":25}}@
--
--     Datasource for training:
--     @{\"splitting\":{\"percentBegin\":0, \"percentEnd\":25, \"complement\":\"true\"}}@
--
-- -   __@strategy@__
--
--     To change how Amazon ML splits the data for a datasource, use the
--     @strategy@ parameter.
--
--     The default value for the @strategy@ parameter is @sequential@,
--     meaning that Amazon ML takes all of the data records between the
--     @percentBegin@ and @percentEnd@ parameters for the datasource, in
--     the order that the records appear in the input data.
--
--     The following two @DataRearrangement@ lines are examples of
--     sequentially ordered training and evaluation datasources:
--
--     Datasource for evaluation:
--     @{\"splitting\":{\"percentBegin\":70, \"percentEnd\":100, \"strategy\":\"sequential\"}}@
--
--     Datasource for training:
--     @{\"splitting\":{\"percentBegin\":70, \"percentEnd\":100, \"strategy\":\"sequential\", \"complement\":\"true\"}}@
--
--     To randomly split the input data into the proportions indicated by
--     the percentBegin and percentEnd parameters, set the @strategy@
--     parameter to @random@ and provide a string that is used as the seed
--     value for the random data splitting (for example, you can use the S3
--     path to your data as the random seed string). If you choose the
--     random split strategy, Amazon ML assigns each row of data a
--     pseudo-random number between 0 and 100, and then selects the rows
--     that have an assigned number between @percentBegin@ and
--     @percentEnd@. Pseudo-random numbers are assigned using both the
--     input seed string value and the byte offset as a seed, so changing
--     the data results in a different split. Any existing ordering is
--     preserved. The random splitting strategy ensures that variables in
--     the training and evaluation data are distributed similarly. It is
--     useful in the cases where the input data may have an implicit sort
--     order, which would otherwise result in training and evaluation
--     datasources containing non-similar data records.
--
--     The following two @DataRearrangement@ lines are examples of
--     non-sequentially ordered training and evaluation datasources:
--
--     Datasource for evaluation:
--     @{\"splitting\":{\"percentBegin\":70, \"percentEnd\":100, \"strategy\":\"random\", \"randomSeed\"=\"s3:\/\/my_s3_path\/bucket\/file.csv\"}}@
--
--     Datasource for training:
--     @{\"splitting\":{\"percentBegin\":70, \"percentEnd\":100, \"strategy\":\"random\", \"randomSeed\"=\"s3:\/\/my_s3_path\/bucket\/file.csv\", \"complement\":\"true\"}}@
redshiftDataSpec_dataRearrangement :: Lens.Lens' RedshiftDataSpec (Prelude.Maybe Prelude.Text)
redshiftDataSpec_dataRearrangement = Lens.lens (\RedshiftDataSpec' {dataRearrangement} -> dataRearrangement) (\s@RedshiftDataSpec' {} a -> s {dataRearrangement = a} :: RedshiftDataSpec)

-- | A JSON string that represents the schema for an Amazon Redshift
-- @DataSource@. The @DataSchema@ defines the structure of the observation
-- data in the data file(s) referenced in the @DataSource@.
--
-- A @DataSchema@ is not required if you specify a @DataSchemaUri@.
--
-- Define your @DataSchema@ as a series of key-value pairs. @attributes@
-- and @excludedVariableNames@ have an array of key-value pairs for their
-- value. Use the following format to define your @DataSchema@.
--
-- { \"version\": \"1.0\",
--
-- \"recordAnnotationFieldName\": \"F1\",
--
-- \"recordWeightFieldName\": \"F2\",
--
-- \"targetFieldName\": \"F3\",
--
-- \"dataFormat\": \"CSV\",
--
-- \"dataFileContainsHeader\": true,
--
-- \"attributes\": [
--
-- { \"fieldName\": \"F1\", \"fieldType\": \"TEXT\" }, { \"fieldName\":
-- \"F2\", \"fieldType\": \"NUMERIC\" }, { \"fieldName\": \"F3\",
-- \"fieldType\": \"CATEGORICAL\" }, { \"fieldName\": \"F4\",
-- \"fieldType\": \"NUMERIC\" }, { \"fieldName\": \"F5\", \"fieldType\":
-- \"CATEGORICAL\" }, { \"fieldName\": \"F6\", \"fieldType\": \"TEXT\" }, {
-- \"fieldName\": \"F7\", \"fieldType\": \"WEIGHTED_INT_SEQUENCE\" }, {
-- \"fieldName\": \"F8\", \"fieldType\": \"WEIGHTED_STRING_SEQUENCE\" } ],
--
-- \"excludedVariableNames\": [ \"F6\" ] }
redshiftDataSpec_dataSchema :: Lens.Lens' RedshiftDataSpec (Prelude.Maybe Prelude.Text)
redshiftDataSpec_dataSchema = Lens.lens (\RedshiftDataSpec' {dataSchema} -> dataSchema) (\s@RedshiftDataSpec' {} a -> s {dataSchema = a} :: RedshiftDataSpec)

-- | Describes the schema location for an Amazon Redshift @DataSource@.
redshiftDataSpec_dataSchemaUri :: Lens.Lens' RedshiftDataSpec (Prelude.Maybe Prelude.Text)
redshiftDataSpec_dataSchemaUri = Lens.lens (\RedshiftDataSpec' {dataSchemaUri} -> dataSchemaUri) (\s@RedshiftDataSpec' {} a -> s {dataSchemaUri = a} :: RedshiftDataSpec)

-- | Describes the @DatabaseName@ and @ClusterIdentifier@ for an Amazon
-- Redshift @DataSource@.
redshiftDataSpec_databaseInformation :: Lens.Lens' RedshiftDataSpec RedshiftDatabase
redshiftDataSpec_databaseInformation = Lens.lens (\RedshiftDataSpec' {databaseInformation} -> databaseInformation) (\s@RedshiftDataSpec' {} a -> s {databaseInformation = a} :: RedshiftDataSpec)

-- | Describes the SQL Query to execute on an Amazon Redshift database for an
-- Amazon Redshift @DataSource@.
redshiftDataSpec_selectSqlQuery :: Lens.Lens' RedshiftDataSpec Prelude.Text
redshiftDataSpec_selectSqlQuery = Lens.lens (\RedshiftDataSpec' {selectSqlQuery} -> selectSqlQuery) (\s@RedshiftDataSpec' {} a -> s {selectSqlQuery = a} :: RedshiftDataSpec)

-- | Describes AWS Identity and Access Management (IAM) credentials that are
-- used connect to the Amazon Redshift database.
redshiftDataSpec_databaseCredentials :: Lens.Lens' RedshiftDataSpec RedshiftDatabaseCredentials
redshiftDataSpec_databaseCredentials = Lens.lens (\RedshiftDataSpec' {databaseCredentials} -> databaseCredentials) (\s@RedshiftDataSpec' {} a -> s {databaseCredentials = a} :: RedshiftDataSpec)

-- | Describes an Amazon S3 location to store the result set of the
-- @SelectSqlQuery@ query.
redshiftDataSpec_s3StagingLocation :: Lens.Lens' RedshiftDataSpec Prelude.Text
redshiftDataSpec_s3StagingLocation = Lens.lens (\RedshiftDataSpec' {s3StagingLocation} -> s3StagingLocation) (\s@RedshiftDataSpec' {} a -> s {s3StagingLocation = a} :: RedshiftDataSpec)

instance Prelude.Hashable RedshiftDataSpec

instance Prelude.NFData RedshiftDataSpec

instance Prelude.ToJSON RedshiftDataSpec where
  toJSON RedshiftDataSpec' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("DataRearrangement" Prelude..=)
              Prelude.<$> dataRearrangement,
            ("DataSchema" Prelude..=) Prelude.<$> dataSchema,
            ("DataSchemaUri" Prelude..=)
              Prelude.<$> dataSchemaUri,
            Prelude.Just
              ( "DatabaseInformation"
                  Prelude..= databaseInformation
              ),
            Prelude.Just
              ("SelectSqlQuery" Prelude..= selectSqlQuery),
            Prelude.Just
              ( "DatabaseCredentials"
                  Prelude..= databaseCredentials
              ),
            Prelude.Just
              ("S3StagingLocation" Prelude..= s3StagingLocation)
          ]
      )
