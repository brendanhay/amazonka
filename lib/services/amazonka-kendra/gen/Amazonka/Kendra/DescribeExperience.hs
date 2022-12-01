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
-- Module      : Amazonka.Kendra.DescribeExperience
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about your Amazon Kendra experience such as a search
-- application. For more information on creating a search application
-- experience, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/deploying-search-experience-no-code.html Building a search experience with no code>.
module Amazonka.Kendra.DescribeExperience
  ( -- * Creating a Request
    DescribeExperience (..),
    newDescribeExperience,

    -- * Request Lenses
    describeExperience_id,
    describeExperience_indexId,

    -- * Destructuring the Response
    DescribeExperienceResponse (..),
    newDescribeExperienceResponse,

    -- * Response Lenses
    describeExperienceResponse_indexId,
    describeExperienceResponse_name,
    describeExperienceResponse_roleArn,
    describeExperienceResponse_errorMessage,
    describeExperienceResponse_configuration,
    describeExperienceResponse_status,
    describeExperienceResponse_endpoints,
    describeExperienceResponse_id,
    describeExperienceResponse_description,
    describeExperienceResponse_createdAt,
    describeExperienceResponse_updatedAt,
    describeExperienceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Kendra.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeExperience' smart constructor.
data DescribeExperience = DescribeExperience'
  { -- | The identifier of your Amazon Kendra experience you want to get
    -- information on.
    id :: Prelude.Text,
    -- | The identifier of the index for your Amazon Kendra experience.
    indexId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeExperience' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'describeExperience_id' - The identifier of your Amazon Kendra experience you want to get
-- information on.
--
-- 'indexId', 'describeExperience_indexId' - The identifier of the index for your Amazon Kendra experience.
newDescribeExperience ::
  -- | 'id'
  Prelude.Text ->
  -- | 'indexId'
  Prelude.Text ->
  DescribeExperience
newDescribeExperience pId_ pIndexId_ =
  DescribeExperience' {id = pId_, indexId = pIndexId_}

-- | The identifier of your Amazon Kendra experience you want to get
-- information on.
describeExperience_id :: Lens.Lens' DescribeExperience Prelude.Text
describeExperience_id = Lens.lens (\DescribeExperience' {id} -> id) (\s@DescribeExperience' {} a -> s {id = a} :: DescribeExperience)

-- | The identifier of the index for your Amazon Kendra experience.
describeExperience_indexId :: Lens.Lens' DescribeExperience Prelude.Text
describeExperience_indexId = Lens.lens (\DescribeExperience' {indexId} -> indexId) (\s@DescribeExperience' {} a -> s {indexId = a} :: DescribeExperience)

instance Core.AWSRequest DescribeExperience where
  type
    AWSResponse DescribeExperience =
      DescribeExperienceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeExperienceResponse'
            Prelude.<$> (x Core..?> "IndexId")
            Prelude.<*> (x Core..?> "Name")
            Prelude.<*> (x Core..?> "RoleArn")
            Prelude.<*> (x Core..?> "ErrorMessage")
            Prelude.<*> (x Core..?> "Configuration")
            Prelude.<*> (x Core..?> "Status")
            Prelude.<*> (x Core..?> "Endpoints")
            Prelude.<*> (x Core..?> "Id")
            Prelude.<*> (x Core..?> "Description")
            Prelude.<*> (x Core..?> "CreatedAt")
            Prelude.<*> (x Core..?> "UpdatedAt")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeExperience where
  hashWithSalt _salt DescribeExperience' {..} =
    _salt `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` indexId

instance Prelude.NFData DescribeExperience where
  rnf DescribeExperience' {..} =
    Prelude.rnf id `Prelude.seq` Prelude.rnf indexId

instance Core.ToHeaders DescribeExperience where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSKendraFrontendService.DescribeExperience" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeExperience where
  toJSON DescribeExperience' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Id" Core..= id),
            Prelude.Just ("IndexId" Core..= indexId)
          ]
      )

instance Core.ToPath DescribeExperience where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeExperience where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeExperienceResponse' smart constructor.
data DescribeExperienceResponse = DescribeExperienceResponse'
  { -- | Shows the identifier of the index for your Amazon Kendra experience.
    indexId :: Prelude.Maybe Prelude.Text,
    -- | Shows the name of your Amazon Kendra experience.
    name :: Prelude.Maybe Prelude.Text,
    -- | Shows the Amazon Resource Name (ARN) of a role with permission to access
    -- @Query@ API, @QuerySuggestions@ API, @SubmitFeedback@ API, and IAM
    -- Identity Center that stores your user and group information.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The reason your Amazon Kendra experience could not properly process.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | Shows the configuration information for your Amazon Kendra experience.
    -- This includes @ContentSourceConfiguration@, which specifies the data
    -- source IDs and\/or FAQ IDs, and @UserIdentityConfiguration@, which
    -- specifies the user or group information to grant access to your Amazon
    -- Kendra experience.
    configuration :: Prelude.Maybe ExperienceConfiguration,
    -- | The current processing status of your Amazon Kendra experience. When the
    -- status is @ACTIVE@, your Amazon Kendra experience is ready to use. When
    -- the status is @FAILED@, the @ErrorMessage@ field contains the reason
    -- that this failed.
    status :: Prelude.Maybe ExperienceStatus,
    -- | Shows the endpoint URLs for your Amazon Kendra experiences. The URLs are
    -- unique and fully hosted by Amazon Web Services.
    endpoints :: Prelude.Maybe (Prelude.NonEmpty ExperienceEndpoint),
    -- | Shows the identifier of your Amazon Kendra experience.
    id :: Prelude.Maybe Prelude.Text,
    -- | Shows the description for your Amazon Kendra experience.
    description :: Prelude.Maybe Prelude.Text,
    -- | Shows the date-time your Amazon Kendra experience was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | Shows the date-time your Amazon Kendra experience was last updated.
    updatedAt :: Prelude.Maybe Core.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeExperienceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'indexId', 'describeExperienceResponse_indexId' - Shows the identifier of the index for your Amazon Kendra experience.
--
-- 'name', 'describeExperienceResponse_name' - Shows the name of your Amazon Kendra experience.
--
-- 'roleArn', 'describeExperienceResponse_roleArn' - Shows the Amazon Resource Name (ARN) of a role with permission to access
-- @Query@ API, @QuerySuggestions@ API, @SubmitFeedback@ API, and IAM
-- Identity Center that stores your user and group information.
--
-- 'errorMessage', 'describeExperienceResponse_errorMessage' - The reason your Amazon Kendra experience could not properly process.
--
-- 'configuration', 'describeExperienceResponse_configuration' - Shows the configuration information for your Amazon Kendra experience.
-- This includes @ContentSourceConfiguration@, which specifies the data
-- source IDs and\/or FAQ IDs, and @UserIdentityConfiguration@, which
-- specifies the user or group information to grant access to your Amazon
-- Kendra experience.
--
-- 'status', 'describeExperienceResponse_status' - The current processing status of your Amazon Kendra experience. When the
-- status is @ACTIVE@, your Amazon Kendra experience is ready to use. When
-- the status is @FAILED@, the @ErrorMessage@ field contains the reason
-- that this failed.
--
-- 'endpoints', 'describeExperienceResponse_endpoints' - Shows the endpoint URLs for your Amazon Kendra experiences. The URLs are
-- unique and fully hosted by Amazon Web Services.
--
-- 'id', 'describeExperienceResponse_id' - Shows the identifier of your Amazon Kendra experience.
--
-- 'description', 'describeExperienceResponse_description' - Shows the description for your Amazon Kendra experience.
--
-- 'createdAt', 'describeExperienceResponse_createdAt' - Shows the date-time your Amazon Kendra experience was created.
--
-- 'updatedAt', 'describeExperienceResponse_updatedAt' - Shows the date-time your Amazon Kendra experience was last updated.
--
-- 'httpStatus', 'describeExperienceResponse_httpStatus' - The response's http status code.
newDescribeExperienceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeExperienceResponse
newDescribeExperienceResponse pHttpStatus_ =
  DescribeExperienceResponse'
    { indexId =
        Prelude.Nothing,
      name = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      configuration = Prelude.Nothing,
      status = Prelude.Nothing,
      endpoints = Prelude.Nothing,
      id = Prelude.Nothing,
      description = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      updatedAt = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Shows the identifier of the index for your Amazon Kendra experience.
describeExperienceResponse_indexId :: Lens.Lens' DescribeExperienceResponse (Prelude.Maybe Prelude.Text)
describeExperienceResponse_indexId = Lens.lens (\DescribeExperienceResponse' {indexId} -> indexId) (\s@DescribeExperienceResponse' {} a -> s {indexId = a} :: DescribeExperienceResponse)

-- | Shows the name of your Amazon Kendra experience.
describeExperienceResponse_name :: Lens.Lens' DescribeExperienceResponse (Prelude.Maybe Prelude.Text)
describeExperienceResponse_name = Lens.lens (\DescribeExperienceResponse' {name} -> name) (\s@DescribeExperienceResponse' {} a -> s {name = a} :: DescribeExperienceResponse)

-- | Shows the Amazon Resource Name (ARN) of a role with permission to access
-- @Query@ API, @QuerySuggestions@ API, @SubmitFeedback@ API, and IAM
-- Identity Center that stores your user and group information.
describeExperienceResponse_roleArn :: Lens.Lens' DescribeExperienceResponse (Prelude.Maybe Prelude.Text)
describeExperienceResponse_roleArn = Lens.lens (\DescribeExperienceResponse' {roleArn} -> roleArn) (\s@DescribeExperienceResponse' {} a -> s {roleArn = a} :: DescribeExperienceResponse)

-- | The reason your Amazon Kendra experience could not properly process.
describeExperienceResponse_errorMessage :: Lens.Lens' DescribeExperienceResponse (Prelude.Maybe Prelude.Text)
describeExperienceResponse_errorMessage = Lens.lens (\DescribeExperienceResponse' {errorMessage} -> errorMessage) (\s@DescribeExperienceResponse' {} a -> s {errorMessage = a} :: DescribeExperienceResponse)

-- | Shows the configuration information for your Amazon Kendra experience.
-- This includes @ContentSourceConfiguration@, which specifies the data
-- source IDs and\/or FAQ IDs, and @UserIdentityConfiguration@, which
-- specifies the user or group information to grant access to your Amazon
-- Kendra experience.
describeExperienceResponse_configuration :: Lens.Lens' DescribeExperienceResponse (Prelude.Maybe ExperienceConfiguration)
describeExperienceResponse_configuration = Lens.lens (\DescribeExperienceResponse' {configuration} -> configuration) (\s@DescribeExperienceResponse' {} a -> s {configuration = a} :: DescribeExperienceResponse)

-- | The current processing status of your Amazon Kendra experience. When the
-- status is @ACTIVE@, your Amazon Kendra experience is ready to use. When
-- the status is @FAILED@, the @ErrorMessage@ field contains the reason
-- that this failed.
describeExperienceResponse_status :: Lens.Lens' DescribeExperienceResponse (Prelude.Maybe ExperienceStatus)
describeExperienceResponse_status = Lens.lens (\DescribeExperienceResponse' {status} -> status) (\s@DescribeExperienceResponse' {} a -> s {status = a} :: DescribeExperienceResponse)

-- | Shows the endpoint URLs for your Amazon Kendra experiences. The URLs are
-- unique and fully hosted by Amazon Web Services.
describeExperienceResponse_endpoints :: Lens.Lens' DescribeExperienceResponse (Prelude.Maybe (Prelude.NonEmpty ExperienceEndpoint))
describeExperienceResponse_endpoints = Lens.lens (\DescribeExperienceResponse' {endpoints} -> endpoints) (\s@DescribeExperienceResponse' {} a -> s {endpoints = a} :: DescribeExperienceResponse) Prelude.. Lens.mapping Lens.coerced

-- | Shows the identifier of your Amazon Kendra experience.
describeExperienceResponse_id :: Lens.Lens' DescribeExperienceResponse (Prelude.Maybe Prelude.Text)
describeExperienceResponse_id = Lens.lens (\DescribeExperienceResponse' {id} -> id) (\s@DescribeExperienceResponse' {} a -> s {id = a} :: DescribeExperienceResponse)

-- | Shows the description for your Amazon Kendra experience.
describeExperienceResponse_description :: Lens.Lens' DescribeExperienceResponse (Prelude.Maybe Prelude.Text)
describeExperienceResponse_description = Lens.lens (\DescribeExperienceResponse' {description} -> description) (\s@DescribeExperienceResponse' {} a -> s {description = a} :: DescribeExperienceResponse)

-- | Shows the date-time your Amazon Kendra experience was created.
describeExperienceResponse_createdAt :: Lens.Lens' DescribeExperienceResponse (Prelude.Maybe Prelude.UTCTime)
describeExperienceResponse_createdAt = Lens.lens (\DescribeExperienceResponse' {createdAt} -> createdAt) (\s@DescribeExperienceResponse' {} a -> s {createdAt = a} :: DescribeExperienceResponse) Prelude.. Lens.mapping Core._Time

-- | Shows the date-time your Amazon Kendra experience was last updated.
describeExperienceResponse_updatedAt :: Lens.Lens' DescribeExperienceResponse (Prelude.Maybe Prelude.UTCTime)
describeExperienceResponse_updatedAt = Lens.lens (\DescribeExperienceResponse' {updatedAt} -> updatedAt) (\s@DescribeExperienceResponse' {} a -> s {updatedAt = a} :: DescribeExperienceResponse) Prelude.. Lens.mapping Core._Time

-- | The response's http status code.
describeExperienceResponse_httpStatus :: Lens.Lens' DescribeExperienceResponse Prelude.Int
describeExperienceResponse_httpStatus = Lens.lens (\DescribeExperienceResponse' {httpStatus} -> httpStatus) (\s@DescribeExperienceResponse' {} a -> s {httpStatus = a} :: DescribeExperienceResponse)

instance Prelude.NFData DescribeExperienceResponse where
  rnf DescribeExperienceResponse' {..} =
    Prelude.rnf indexId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf configuration
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf endpoints
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf updatedAt
      `Prelude.seq` Prelude.rnf httpStatus
