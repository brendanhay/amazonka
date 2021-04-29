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
-- Module      : Network.AWS.MachineLearning.Types.RDSDataSpec
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Types.RDSDataSpec where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types.RDSDatabase
import Network.AWS.MachineLearning.Types.RDSDatabaseCredentials
import qualified Network.AWS.Prelude as Prelude

-- | The data specification of an Amazon Relational Database Service (Amazon
-- RDS) @DataSource@.
--
-- /See:/ 'newRDSDataSpec' smart constructor.
data RDSDataSpec = RDSDataSpec'
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
    -- | A JSON string that represents the schema for an Amazon RDS @DataSource@.
    -- The @DataSchema@ defines the structure of the observation data in the
    -- data file(s) referenced in the @DataSource@.
    --
    -- A @DataSchema@ is not required if you specify a @DataSchemaUri@
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
    -- | The Amazon S3 location of the @DataSchema@.
    dataSchemaUri :: Prelude.Maybe Prelude.Text,
    -- | Describes the @DatabaseName@ and @InstanceIdentifier@ of an Amazon RDS
    -- database.
    databaseInformation :: RDSDatabase,
    -- | The query that is used to retrieve the observation data for the
    -- @DataSource@.
    selectSqlQuery :: Prelude.Text,
    -- | The AWS Identity and Access Management (IAM) credentials that are used
    -- connect to the Amazon RDS database.
    databaseCredentials :: RDSDatabaseCredentials,
    -- | The Amazon S3 location for staging Amazon RDS data. The data retrieved
    -- from Amazon RDS using @SelectSqlQuery@ is stored in this location.
    s3StagingLocation :: Prelude.Text,
    -- | The role (DataPipelineDefaultResourceRole) assumed by an Amazon Elastic
    -- Compute Cloud (Amazon EC2) instance to carry out the copy operation from
    -- Amazon RDS to an Amazon S3 task. For more information, see
    -- <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-iam-roles.html Role templates>
    -- for data pipelines.
    resourceRole :: Prelude.Text,
    -- | The role (DataPipelineDefaultRole) assumed by AWS Data Pipeline service
    -- to monitor the progress of the copy task from Amazon RDS to Amazon S3.
    -- For more information, see
    -- <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-iam-roles.html Role templates>
    -- for data pipelines.
    serviceRole :: Prelude.Text,
    -- | The subnet ID to be used to access a VPC-based RDS DB instance. This
    -- attribute is used by Data Pipeline to carry out the copy task from
    -- Amazon RDS to Amazon S3.
    subnetId :: Prelude.Text,
    -- | The security group IDs to be used to access a VPC-based RDS DB instance.
    -- Ensure that there are appropriate ingress rules set up to allow access
    -- to the RDS DB instance. This attribute is used by Data Pipeline to carry
    -- out the copy operation from Amazon RDS to an Amazon S3 task.
    securityGroupIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RDSDataSpec' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataRearrangement', 'rDSDataSpec_dataRearrangement' - A JSON string that represents the splitting and rearrangement processing
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
-- 'dataSchema', 'rDSDataSpec_dataSchema' - A JSON string that represents the schema for an Amazon RDS @DataSource@.
-- The @DataSchema@ defines the structure of the observation data in the
-- data file(s) referenced in the @DataSource@.
--
-- A @DataSchema@ is not required if you specify a @DataSchemaUri@
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
-- 'dataSchemaUri', 'rDSDataSpec_dataSchemaUri' - The Amazon S3 location of the @DataSchema@.
--
-- 'databaseInformation', 'rDSDataSpec_databaseInformation' - Describes the @DatabaseName@ and @InstanceIdentifier@ of an Amazon RDS
-- database.
--
-- 'selectSqlQuery', 'rDSDataSpec_selectSqlQuery' - The query that is used to retrieve the observation data for the
-- @DataSource@.
--
-- 'databaseCredentials', 'rDSDataSpec_databaseCredentials' - The AWS Identity and Access Management (IAM) credentials that are used
-- connect to the Amazon RDS database.
--
-- 's3StagingLocation', 'rDSDataSpec_s3StagingLocation' - The Amazon S3 location for staging Amazon RDS data. The data retrieved
-- from Amazon RDS using @SelectSqlQuery@ is stored in this location.
--
-- 'resourceRole', 'rDSDataSpec_resourceRole' - The role (DataPipelineDefaultResourceRole) assumed by an Amazon Elastic
-- Compute Cloud (Amazon EC2) instance to carry out the copy operation from
-- Amazon RDS to an Amazon S3 task. For more information, see
-- <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-iam-roles.html Role templates>
-- for data pipelines.
--
-- 'serviceRole', 'rDSDataSpec_serviceRole' - The role (DataPipelineDefaultRole) assumed by AWS Data Pipeline service
-- to monitor the progress of the copy task from Amazon RDS to Amazon S3.
-- For more information, see
-- <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-iam-roles.html Role templates>
-- for data pipelines.
--
-- 'subnetId', 'rDSDataSpec_subnetId' - The subnet ID to be used to access a VPC-based RDS DB instance. This
-- attribute is used by Data Pipeline to carry out the copy task from
-- Amazon RDS to Amazon S3.
--
-- 'securityGroupIds', 'rDSDataSpec_securityGroupIds' - The security group IDs to be used to access a VPC-based RDS DB instance.
-- Ensure that there are appropriate ingress rules set up to allow access
-- to the RDS DB instance. This attribute is used by Data Pipeline to carry
-- out the copy operation from Amazon RDS to an Amazon S3 task.
newRDSDataSpec ::
  -- | 'databaseInformation'
  RDSDatabase ->
  -- | 'selectSqlQuery'
  Prelude.Text ->
  -- | 'databaseCredentials'
  RDSDatabaseCredentials ->
  -- | 's3StagingLocation'
  Prelude.Text ->
  -- | 'resourceRole'
  Prelude.Text ->
  -- | 'serviceRole'
  Prelude.Text ->
  -- | 'subnetId'
  Prelude.Text ->
  RDSDataSpec
newRDSDataSpec
  pDatabaseInformation_
  pSelectSqlQuery_
  pDatabaseCredentials_
  pS3StagingLocation_
  pResourceRole_
  pServiceRole_
  pSubnetId_ =
    RDSDataSpec'
      { dataRearrangement = Prelude.Nothing,
        dataSchema = Prelude.Nothing,
        dataSchemaUri = Prelude.Nothing,
        databaseInformation = pDatabaseInformation_,
        selectSqlQuery = pSelectSqlQuery_,
        databaseCredentials = pDatabaseCredentials_,
        s3StagingLocation = pS3StagingLocation_,
        resourceRole = pResourceRole_,
        serviceRole = pServiceRole_,
        subnetId = pSubnetId_,
        securityGroupIds = Prelude.mempty
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
rDSDataSpec_dataRearrangement :: Lens.Lens' RDSDataSpec (Prelude.Maybe Prelude.Text)
rDSDataSpec_dataRearrangement = Lens.lens (\RDSDataSpec' {dataRearrangement} -> dataRearrangement) (\s@RDSDataSpec' {} a -> s {dataRearrangement = a} :: RDSDataSpec)

-- | A JSON string that represents the schema for an Amazon RDS @DataSource@.
-- The @DataSchema@ defines the structure of the observation data in the
-- data file(s) referenced in the @DataSource@.
--
-- A @DataSchema@ is not required if you specify a @DataSchemaUri@
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
rDSDataSpec_dataSchema :: Lens.Lens' RDSDataSpec (Prelude.Maybe Prelude.Text)
rDSDataSpec_dataSchema = Lens.lens (\RDSDataSpec' {dataSchema} -> dataSchema) (\s@RDSDataSpec' {} a -> s {dataSchema = a} :: RDSDataSpec)

-- | The Amazon S3 location of the @DataSchema@.
rDSDataSpec_dataSchemaUri :: Lens.Lens' RDSDataSpec (Prelude.Maybe Prelude.Text)
rDSDataSpec_dataSchemaUri = Lens.lens (\RDSDataSpec' {dataSchemaUri} -> dataSchemaUri) (\s@RDSDataSpec' {} a -> s {dataSchemaUri = a} :: RDSDataSpec)

-- | Describes the @DatabaseName@ and @InstanceIdentifier@ of an Amazon RDS
-- database.
rDSDataSpec_databaseInformation :: Lens.Lens' RDSDataSpec RDSDatabase
rDSDataSpec_databaseInformation = Lens.lens (\RDSDataSpec' {databaseInformation} -> databaseInformation) (\s@RDSDataSpec' {} a -> s {databaseInformation = a} :: RDSDataSpec)

-- | The query that is used to retrieve the observation data for the
-- @DataSource@.
rDSDataSpec_selectSqlQuery :: Lens.Lens' RDSDataSpec Prelude.Text
rDSDataSpec_selectSqlQuery = Lens.lens (\RDSDataSpec' {selectSqlQuery} -> selectSqlQuery) (\s@RDSDataSpec' {} a -> s {selectSqlQuery = a} :: RDSDataSpec)

-- | The AWS Identity and Access Management (IAM) credentials that are used
-- connect to the Amazon RDS database.
rDSDataSpec_databaseCredentials :: Lens.Lens' RDSDataSpec RDSDatabaseCredentials
rDSDataSpec_databaseCredentials = Lens.lens (\RDSDataSpec' {databaseCredentials} -> databaseCredentials) (\s@RDSDataSpec' {} a -> s {databaseCredentials = a} :: RDSDataSpec)

-- | The Amazon S3 location for staging Amazon RDS data. The data retrieved
-- from Amazon RDS using @SelectSqlQuery@ is stored in this location.
rDSDataSpec_s3StagingLocation :: Lens.Lens' RDSDataSpec Prelude.Text
rDSDataSpec_s3StagingLocation = Lens.lens (\RDSDataSpec' {s3StagingLocation} -> s3StagingLocation) (\s@RDSDataSpec' {} a -> s {s3StagingLocation = a} :: RDSDataSpec)

-- | The role (DataPipelineDefaultResourceRole) assumed by an Amazon Elastic
-- Compute Cloud (Amazon EC2) instance to carry out the copy operation from
-- Amazon RDS to an Amazon S3 task. For more information, see
-- <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-iam-roles.html Role templates>
-- for data pipelines.
rDSDataSpec_resourceRole :: Lens.Lens' RDSDataSpec Prelude.Text
rDSDataSpec_resourceRole = Lens.lens (\RDSDataSpec' {resourceRole} -> resourceRole) (\s@RDSDataSpec' {} a -> s {resourceRole = a} :: RDSDataSpec)

-- | The role (DataPipelineDefaultRole) assumed by AWS Data Pipeline service
-- to monitor the progress of the copy task from Amazon RDS to Amazon S3.
-- For more information, see
-- <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-iam-roles.html Role templates>
-- for data pipelines.
rDSDataSpec_serviceRole :: Lens.Lens' RDSDataSpec Prelude.Text
rDSDataSpec_serviceRole = Lens.lens (\RDSDataSpec' {serviceRole} -> serviceRole) (\s@RDSDataSpec' {} a -> s {serviceRole = a} :: RDSDataSpec)

-- | The subnet ID to be used to access a VPC-based RDS DB instance. This
-- attribute is used by Data Pipeline to carry out the copy task from
-- Amazon RDS to Amazon S3.
rDSDataSpec_subnetId :: Lens.Lens' RDSDataSpec Prelude.Text
rDSDataSpec_subnetId = Lens.lens (\RDSDataSpec' {subnetId} -> subnetId) (\s@RDSDataSpec' {} a -> s {subnetId = a} :: RDSDataSpec)

-- | The security group IDs to be used to access a VPC-based RDS DB instance.
-- Ensure that there are appropriate ingress rules set up to allow access
-- to the RDS DB instance. This attribute is used by Data Pipeline to carry
-- out the copy operation from Amazon RDS to an Amazon S3 task.
rDSDataSpec_securityGroupIds :: Lens.Lens' RDSDataSpec [Prelude.Text]
rDSDataSpec_securityGroupIds = Lens.lens (\RDSDataSpec' {securityGroupIds} -> securityGroupIds) (\s@RDSDataSpec' {} a -> s {securityGroupIds = a} :: RDSDataSpec) Prelude.. Prelude._Coerce

instance Prelude.Hashable RDSDataSpec

instance Prelude.NFData RDSDataSpec

instance Prelude.ToJSON RDSDataSpec where
  toJSON RDSDataSpec' {..} =
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
              ("S3StagingLocation" Prelude..= s3StagingLocation),
            Prelude.Just
              ("ResourceRole" Prelude..= resourceRole),
            Prelude.Just ("ServiceRole" Prelude..= serviceRole),
            Prelude.Just ("SubnetId" Prelude..= subnetId),
            Prelude.Just
              ("SecurityGroupIds" Prelude..= securityGroupIds)
          ]
      )
