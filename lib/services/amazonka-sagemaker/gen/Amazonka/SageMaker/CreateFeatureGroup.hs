{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SageMaker.CreateFeatureGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a new @FeatureGroup@. A @FeatureGroup@ is a group of @Features@
-- defined in the @FeatureStore@ to describe a @Record@.
--
-- The @FeatureGroup@ defines the schema and features contained in the
-- FeatureGroup. A @FeatureGroup@ definition is composed of a list of
-- @Features@, a @RecordIdentifierFeatureName@, an @EventTimeFeatureName@
-- and configurations for its @OnlineStore@ and @OfflineStore@. Check
-- <https://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html Amazon Web Services service quotas>
-- to see the @FeatureGroup@s quota for your Amazon Web Services account.
--
-- You must include at least one of @OnlineStoreConfig@ and
-- @OfflineStoreConfig@ to create a @FeatureGroup@.
module Amazonka.SageMaker.CreateFeatureGroup
  ( -- * Creating a Request
    CreateFeatureGroup (..),
    newCreateFeatureGroup,

    -- * Request Lenses
    createFeatureGroup_description,
    createFeatureGroup_offlineStoreConfig,
    createFeatureGroup_onlineStoreConfig,
    createFeatureGroup_roleArn,
    createFeatureGroup_tags,
    createFeatureGroup_featureGroupName,
    createFeatureGroup_recordIdentifierFeatureName,
    createFeatureGroup_eventTimeFeatureName,
    createFeatureGroup_featureDefinitions,

    -- * Destructuring the Response
    CreateFeatureGroupResponse (..),
    newCreateFeatureGroupResponse,

    -- * Response Lenses
    createFeatureGroupResponse_httpStatus,
    createFeatureGroupResponse_featureGroupArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newCreateFeatureGroup' smart constructor.
data CreateFeatureGroup = CreateFeatureGroup'
  { -- | A free-form description of a @FeatureGroup@.
    description :: Prelude.Maybe Prelude.Text,
    -- | Use this to configure an @OfflineFeatureStore@. This parameter allows
    -- you to specify:
    --
    -- -   The Amazon Simple Storage Service (Amazon S3) location of an
    --     @OfflineStore@.
    --
    -- -   A configuration for an Amazon Web Services Glue or Amazon Web
    --     Services Hive data catalog.
    --
    -- -   An KMS encryption key to encrypt the Amazon S3 location used for
    --     @OfflineStore@. If KMS encryption key is not specified, by default
    --     we encrypt all data at rest using Amazon Web Services KMS key. By
    --     defining your
    --     <https://docs.aws.amazon.com/AmazonS3/latest/userguide/bucket-key.html bucket-level key>
    --     for SSE, you can reduce Amazon Web Services KMS requests costs by up
    --     to 99 percent.
    --
    -- -   Format for the offline store table. Supported formats are Glue
    --     (Default) and <https://iceberg.apache.org/ Apache Iceberg>.
    --
    -- To learn more about this parameter, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_OfflineStoreConfig.html OfflineStoreConfig>.
    offlineStoreConfig :: Prelude.Maybe OfflineStoreConfig,
    -- | You can turn the @OnlineStore@ on or off by specifying @True@ for the
    -- @EnableOnlineStore@ flag in @OnlineStoreConfig@.
    --
    -- You can also include an Amazon Web Services KMS key ID (@KMSKeyId@) for
    -- at-rest encryption of the @OnlineStore@.
    --
    -- The default value is @False@.
    onlineStoreConfig :: Prelude.Maybe OnlineStoreConfig,
    -- | The Amazon Resource Name (ARN) of the IAM execution role used to persist
    -- data into the @OfflineStore@ if an @OfflineStoreConfig@ is provided.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | Tags used to identify @Features@ in each @FeatureGroup@.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the @FeatureGroup@. The name must be unique within an Amazon
    -- Web Services Region in an Amazon Web Services account. The name:
    --
    -- -   Must start and end with an alphanumeric character.
    --
    -- -   Can only contain alphanumeric character and hyphens. Spaces are not
    --     allowed.
    featureGroupName :: Prelude.Text,
    -- | The name of the @Feature@ whose value uniquely identifies a @Record@
    -- defined in the @FeatureStore@. Only the latest record per identifier
    -- value will be stored in the @OnlineStore@. @RecordIdentifierFeatureName@
    -- must be one of feature definitions\' names.
    --
    -- You use the @RecordIdentifierFeatureName@ to access data in a
    -- @FeatureStore@.
    --
    -- This name:
    --
    -- -   Must start and end with an alphanumeric character.
    --
    -- -   Can only contains alphanumeric characters, hyphens, underscores.
    --     Spaces are not allowed.
    recordIdentifierFeatureName :: Prelude.Text,
    -- | The name of the feature that stores the @EventTime@ of a @Record@ in a
    -- @FeatureGroup@.
    --
    -- An @EventTime@ is a point in time when a new event occurs that
    -- corresponds to the creation or update of a @Record@ in a @FeatureGroup@.
    -- All @Records@ in the @FeatureGroup@ must have a corresponding
    -- @EventTime@.
    --
    -- An @EventTime@ can be a @String@ or @Fractional@.
    --
    -- -   @Fractional@: @EventTime@ feature values must be a Unix timestamp in
    --     seconds.
    --
    -- -   @String@: @EventTime@ feature values must be an ISO-8601 string in
    --     the format. The following formats are supported
    --     @yyyy-MM-dd\'T\'HH:mm:ssZ@ and @yyyy-MM-dd\'T\'HH:mm:ss.SSSZ@ where
    --     @yyyy@, @MM@, and @dd@ represent the year, month, and day
    --     respectively and @HH@, @mm@, @ss@, and if applicable, @SSS@
    --     represent the hour, month, second and milliseconds respsectively.
    --     @\'T\'@ and @Z@ are constants.
    eventTimeFeatureName :: Prelude.Text,
    -- | A list of @Feature@ names and types. @Name@ and @Type@ is compulsory per
    -- @Feature@.
    --
    -- Valid feature @FeatureType@s are @Integral@, @Fractional@ and @String@.
    --
    -- @FeatureName@s cannot be any of the following: @is_deleted@,
    -- @write_time@, @api_invocation_time@
    --
    -- You can create up to 2,500 @FeatureDefinition@s per @FeatureGroup@.
    featureDefinitions :: Prelude.NonEmpty FeatureDefinition
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFeatureGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createFeatureGroup_description' - A free-form description of a @FeatureGroup@.
--
-- 'offlineStoreConfig', 'createFeatureGroup_offlineStoreConfig' - Use this to configure an @OfflineFeatureStore@. This parameter allows
-- you to specify:
--
-- -   The Amazon Simple Storage Service (Amazon S3) location of an
--     @OfflineStore@.
--
-- -   A configuration for an Amazon Web Services Glue or Amazon Web
--     Services Hive data catalog.
--
-- -   An KMS encryption key to encrypt the Amazon S3 location used for
--     @OfflineStore@. If KMS encryption key is not specified, by default
--     we encrypt all data at rest using Amazon Web Services KMS key. By
--     defining your
--     <https://docs.aws.amazon.com/AmazonS3/latest/userguide/bucket-key.html bucket-level key>
--     for SSE, you can reduce Amazon Web Services KMS requests costs by up
--     to 99 percent.
--
-- -   Format for the offline store table. Supported formats are Glue
--     (Default) and <https://iceberg.apache.org/ Apache Iceberg>.
--
-- To learn more about this parameter, see
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_OfflineStoreConfig.html OfflineStoreConfig>.
--
-- 'onlineStoreConfig', 'createFeatureGroup_onlineStoreConfig' - You can turn the @OnlineStore@ on or off by specifying @True@ for the
-- @EnableOnlineStore@ flag in @OnlineStoreConfig@.
--
-- You can also include an Amazon Web Services KMS key ID (@KMSKeyId@) for
-- at-rest encryption of the @OnlineStore@.
--
-- The default value is @False@.
--
-- 'roleArn', 'createFeatureGroup_roleArn' - The Amazon Resource Name (ARN) of the IAM execution role used to persist
-- data into the @OfflineStore@ if an @OfflineStoreConfig@ is provided.
--
-- 'tags', 'createFeatureGroup_tags' - Tags used to identify @Features@ in each @FeatureGroup@.
--
-- 'featureGroupName', 'createFeatureGroup_featureGroupName' - The name of the @FeatureGroup@. The name must be unique within an Amazon
-- Web Services Region in an Amazon Web Services account. The name:
--
-- -   Must start and end with an alphanumeric character.
--
-- -   Can only contain alphanumeric character and hyphens. Spaces are not
--     allowed.
--
-- 'recordIdentifierFeatureName', 'createFeatureGroup_recordIdentifierFeatureName' - The name of the @Feature@ whose value uniquely identifies a @Record@
-- defined in the @FeatureStore@. Only the latest record per identifier
-- value will be stored in the @OnlineStore@. @RecordIdentifierFeatureName@
-- must be one of feature definitions\' names.
--
-- You use the @RecordIdentifierFeatureName@ to access data in a
-- @FeatureStore@.
--
-- This name:
--
-- -   Must start and end with an alphanumeric character.
--
-- -   Can only contains alphanumeric characters, hyphens, underscores.
--     Spaces are not allowed.
--
-- 'eventTimeFeatureName', 'createFeatureGroup_eventTimeFeatureName' - The name of the feature that stores the @EventTime@ of a @Record@ in a
-- @FeatureGroup@.
--
-- An @EventTime@ is a point in time when a new event occurs that
-- corresponds to the creation or update of a @Record@ in a @FeatureGroup@.
-- All @Records@ in the @FeatureGroup@ must have a corresponding
-- @EventTime@.
--
-- An @EventTime@ can be a @String@ or @Fractional@.
--
-- -   @Fractional@: @EventTime@ feature values must be a Unix timestamp in
--     seconds.
--
-- -   @String@: @EventTime@ feature values must be an ISO-8601 string in
--     the format. The following formats are supported
--     @yyyy-MM-dd\'T\'HH:mm:ssZ@ and @yyyy-MM-dd\'T\'HH:mm:ss.SSSZ@ where
--     @yyyy@, @MM@, and @dd@ represent the year, month, and day
--     respectively and @HH@, @mm@, @ss@, and if applicable, @SSS@
--     represent the hour, month, second and milliseconds respsectively.
--     @\'T\'@ and @Z@ are constants.
--
-- 'featureDefinitions', 'createFeatureGroup_featureDefinitions' - A list of @Feature@ names and types. @Name@ and @Type@ is compulsory per
-- @Feature@.
--
-- Valid feature @FeatureType@s are @Integral@, @Fractional@ and @String@.
--
-- @FeatureName@s cannot be any of the following: @is_deleted@,
-- @write_time@, @api_invocation_time@
--
-- You can create up to 2,500 @FeatureDefinition@s per @FeatureGroup@.
newCreateFeatureGroup ::
  -- | 'featureGroupName'
  Prelude.Text ->
  -- | 'recordIdentifierFeatureName'
  Prelude.Text ->
  -- | 'eventTimeFeatureName'
  Prelude.Text ->
  -- | 'featureDefinitions'
  Prelude.NonEmpty FeatureDefinition ->
  CreateFeatureGroup
newCreateFeatureGroup
  pFeatureGroupName_
  pRecordIdentifierFeatureName_
  pEventTimeFeatureName_
  pFeatureDefinitions_ =
    CreateFeatureGroup'
      { description = Prelude.Nothing,
        offlineStoreConfig = Prelude.Nothing,
        onlineStoreConfig = Prelude.Nothing,
        roleArn = Prelude.Nothing,
        tags = Prelude.Nothing,
        featureGroupName = pFeatureGroupName_,
        recordIdentifierFeatureName =
          pRecordIdentifierFeatureName_,
        eventTimeFeatureName = pEventTimeFeatureName_,
        featureDefinitions =
          Lens.coerced Lens.# pFeatureDefinitions_
      }

-- | A free-form description of a @FeatureGroup@.
createFeatureGroup_description :: Lens.Lens' CreateFeatureGroup (Prelude.Maybe Prelude.Text)
createFeatureGroup_description = Lens.lens (\CreateFeatureGroup' {description} -> description) (\s@CreateFeatureGroup' {} a -> s {description = a} :: CreateFeatureGroup)

-- | Use this to configure an @OfflineFeatureStore@. This parameter allows
-- you to specify:
--
-- -   The Amazon Simple Storage Service (Amazon S3) location of an
--     @OfflineStore@.
--
-- -   A configuration for an Amazon Web Services Glue or Amazon Web
--     Services Hive data catalog.
--
-- -   An KMS encryption key to encrypt the Amazon S3 location used for
--     @OfflineStore@. If KMS encryption key is not specified, by default
--     we encrypt all data at rest using Amazon Web Services KMS key. By
--     defining your
--     <https://docs.aws.amazon.com/AmazonS3/latest/userguide/bucket-key.html bucket-level key>
--     for SSE, you can reduce Amazon Web Services KMS requests costs by up
--     to 99 percent.
--
-- -   Format for the offline store table. Supported formats are Glue
--     (Default) and <https://iceberg.apache.org/ Apache Iceberg>.
--
-- To learn more about this parameter, see
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_OfflineStoreConfig.html OfflineStoreConfig>.
createFeatureGroup_offlineStoreConfig :: Lens.Lens' CreateFeatureGroup (Prelude.Maybe OfflineStoreConfig)
createFeatureGroup_offlineStoreConfig = Lens.lens (\CreateFeatureGroup' {offlineStoreConfig} -> offlineStoreConfig) (\s@CreateFeatureGroup' {} a -> s {offlineStoreConfig = a} :: CreateFeatureGroup)

-- | You can turn the @OnlineStore@ on or off by specifying @True@ for the
-- @EnableOnlineStore@ flag in @OnlineStoreConfig@.
--
-- You can also include an Amazon Web Services KMS key ID (@KMSKeyId@) for
-- at-rest encryption of the @OnlineStore@.
--
-- The default value is @False@.
createFeatureGroup_onlineStoreConfig :: Lens.Lens' CreateFeatureGroup (Prelude.Maybe OnlineStoreConfig)
createFeatureGroup_onlineStoreConfig = Lens.lens (\CreateFeatureGroup' {onlineStoreConfig} -> onlineStoreConfig) (\s@CreateFeatureGroup' {} a -> s {onlineStoreConfig = a} :: CreateFeatureGroup)

-- | The Amazon Resource Name (ARN) of the IAM execution role used to persist
-- data into the @OfflineStore@ if an @OfflineStoreConfig@ is provided.
createFeatureGroup_roleArn :: Lens.Lens' CreateFeatureGroup (Prelude.Maybe Prelude.Text)
createFeatureGroup_roleArn = Lens.lens (\CreateFeatureGroup' {roleArn} -> roleArn) (\s@CreateFeatureGroup' {} a -> s {roleArn = a} :: CreateFeatureGroup)

-- | Tags used to identify @Features@ in each @FeatureGroup@.
createFeatureGroup_tags :: Lens.Lens' CreateFeatureGroup (Prelude.Maybe [Tag])
createFeatureGroup_tags = Lens.lens (\CreateFeatureGroup' {tags} -> tags) (\s@CreateFeatureGroup' {} a -> s {tags = a} :: CreateFeatureGroup) Prelude.. Lens.mapping Lens.coerced

-- | The name of the @FeatureGroup@. The name must be unique within an Amazon
-- Web Services Region in an Amazon Web Services account. The name:
--
-- -   Must start and end with an alphanumeric character.
--
-- -   Can only contain alphanumeric character and hyphens. Spaces are not
--     allowed.
createFeatureGroup_featureGroupName :: Lens.Lens' CreateFeatureGroup Prelude.Text
createFeatureGroup_featureGroupName = Lens.lens (\CreateFeatureGroup' {featureGroupName} -> featureGroupName) (\s@CreateFeatureGroup' {} a -> s {featureGroupName = a} :: CreateFeatureGroup)

-- | The name of the @Feature@ whose value uniquely identifies a @Record@
-- defined in the @FeatureStore@. Only the latest record per identifier
-- value will be stored in the @OnlineStore@. @RecordIdentifierFeatureName@
-- must be one of feature definitions\' names.
--
-- You use the @RecordIdentifierFeatureName@ to access data in a
-- @FeatureStore@.
--
-- This name:
--
-- -   Must start and end with an alphanumeric character.
--
-- -   Can only contains alphanumeric characters, hyphens, underscores.
--     Spaces are not allowed.
createFeatureGroup_recordIdentifierFeatureName :: Lens.Lens' CreateFeatureGroup Prelude.Text
createFeatureGroup_recordIdentifierFeatureName = Lens.lens (\CreateFeatureGroup' {recordIdentifierFeatureName} -> recordIdentifierFeatureName) (\s@CreateFeatureGroup' {} a -> s {recordIdentifierFeatureName = a} :: CreateFeatureGroup)

-- | The name of the feature that stores the @EventTime@ of a @Record@ in a
-- @FeatureGroup@.
--
-- An @EventTime@ is a point in time when a new event occurs that
-- corresponds to the creation or update of a @Record@ in a @FeatureGroup@.
-- All @Records@ in the @FeatureGroup@ must have a corresponding
-- @EventTime@.
--
-- An @EventTime@ can be a @String@ or @Fractional@.
--
-- -   @Fractional@: @EventTime@ feature values must be a Unix timestamp in
--     seconds.
--
-- -   @String@: @EventTime@ feature values must be an ISO-8601 string in
--     the format. The following formats are supported
--     @yyyy-MM-dd\'T\'HH:mm:ssZ@ and @yyyy-MM-dd\'T\'HH:mm:ss.SSSZ@ where
--     @yyyy@, @MM@, and @dd@ represent the year, month, and day
--     respectively and @HH@, @mm@, @ss@, and if applicable, @SSS@
--     represent the hour, month, second and milliseconds respsectively.
--     @\'T\'@ and @Z@ are constants.
createFeatureGroup_eventTimeFeatureName :: Lens.Lens' CreateFeatureGroup Prelude.Text
createFeatureGroup_eventTimeFeatureName = Lens.lens (\CreateFeatureGroup' {eventTimeFeatureName} -> eventTimeFeatureName) (\s@CreateFeatureGroup' {} a -> s {eventTimeFeatureName = a} :: CreateFeatureGroup)

-- | A list of @Feature@ names and types. @Name@ and @Type@ is compulsory per
-- @Feature@.
--
-- Valid feature @FeatureType@s are @Integral@, @Fractional@ and @String@.
--
-- @FeatureName@s cannot be any of the following: @is_deleted@,
-- @write_time@, @api_invocation_time@
--
-- You can create up to 2,500 @FeatureDefinition@s per @FeatureGroup@.
createFeatureGroup_featureDefinitions :: Lens.Lens' CreateFeatureGroup (Prelude.NonEmpty FeatureDefinition)
createFeatureGroup_featureDefinitions = Lens.lens (\CreateFeatureGroup' {featureDefinitions} -> featureDefinitions) (\s@CreateFeatureGroup' {} a -> s {featureDefinitions = a} :: CreateFeatureGroup) Prelude.. Lens.coerced

instance Core.AWSRequest CreateFeatureGroup where
  type
    AWSResponse CreateFeatureGroup =
      CreateFeatureGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateFeatureGroupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "FeatureGroupArn")
      )

instance Prelude.Hashable CreateFeatureGroup where
  hashWithSalt _salt CreateFeatureGroup' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` offlineStoreConfig
      `Prelude.hashWithSalt` onlineStoreConfig
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` featureGroupName
      `Prelude.hashWithSalt` recordIdentifierFeatureName
      `Prelude.hashWithSalt` eventTimeFeatureName
      `Prelude.hashWithSalt` featureDefinitions

instance Prelude.NFData CreateFeatureGroup where
  rnf CreateFeatureGroup' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf offlineStoreConfig
      `Prelude.seq` Prelude.rnf onlineStoreConfig
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf featureGroupName
      `Prelude.seq` Prelude.rnf recordIdentifierFeatureName
      `Prelude.seq` Prelude.rnf eventTimeFeatureName
      `Prelude.seq` Prelude.rnf featureDefinitions

instance Data.ToHeaders CreateFeatureGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.CreateFeatureGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateFeatureGroup where
  toJSON CreateFeatureGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("OfflineStoreConfig" Data..=)
              Prelude.<$> offlineStoreConfig,
            ("OnlineStoreConfig" Data..=)
              Prelude.<$> onlineStoreConfig,
            ("RoleArn" Data..=) Prelude.<$> roleArn,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("FeatureGroupName" Data..= featureGroupName),
            Prelude.Just
              ( "RecordIdentifierFeatureName"
                  Data..= recordIdentifierFeatureName
              ),
            Prelude.Just
              ( "EventTimeFeatureName"
                  Data..= eventTimeFeatureName
              ),
            Prelude.Just
              ("FeatureDefinitions" Data..= featureDefinitions)
          ]
      )

instance Data.ToPath CreateFeatureGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateFeatureGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateFeatureGroupResponse' smart constructor.
data CreateFeatureGroupResponse = CreateFeatureGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the @FeatureGroup@. This is a unique
    -- identifier for the feature group.
    featureGroupArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFeatureGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createFeatureGroupResponse_httpStatus' - The response's http status code.
--
-- 'featureGroupArn', 'createFeatureGroupResponse_featureGroupArn' - The Amazon Resource Name (ARN) of the @FeatureGroup@. This is a unique
-- identifier for the feature group.
newCreateFeatureGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'featureGroupArn'
  Prelude.Text ->
  CreateFeatureGroupResponse
newCreateFeatureGroupResponse
  pHttpStatus_
  pFeatureGroupArn_ =
    CreateFeatureGroupResponse'
      { httpStatus =
          pHttpStatus_,
        featureGroupArn = pFeatureGroupArn_
      }

-- | The response's http status code.
createFeatureGroupResponse_httpStatus :: Lens.Lens' CreateFeatureGroupResponse Prelude.Int
createFeatureGroupResponse_httpStatus = Lens.lens (\CreateFeatureGroupResponse' {httpStatus} -> httpStatus) (\s@CreateFeatureGroupResponse' {} a -> s {httpStatus = a} :: CreateFeatureGroupResponse)

-- | The Amazon Resource Name (ARN) of the @FeatureGroup@. This is a unique
-- identifier for the feature group.
createFeatureGroupResponse_featureGroupArn :: Lens.Lens' CreateFeatureGroupResponse Prelude.Text
createFeatureGroupResponse_featureGroupArn = Lens.lens (\CreateFeatureGroupResponse' {featureGroupArn} -> featureGroupArn) (\s@CreateFeatureGroupResponse' {} a -> s {featureGroupArn = a} :: CreateFeatureGroupResponse)

instance Prelude.NFData CreateFeatureGroupResponse where
  rnf CreateFeatureGroupResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf featureGroupArn
