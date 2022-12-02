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
-- Module      : Amazonka.Kendra.UpdateExperience
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates your Amazon Kendra experience such as a search application. For
-- more information on creating a search application experience, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/deploying-search-experience-no-code.html Building a search experience with no code>.
module Amazonka.Kendra.UpdateExperience
  ( -- * Creating a Request
    UpdateExperience (..),
    newUpdateExperience,

    -- * Request Lenses
    updateExperience_name,
    updateExperience_roleArn,
    updateExperience_configuration,
    updateExperience_description,
    updateExperience_id,
    updateExperience_indexId,

    -- * Destructuring the Response
    UpdateExperienceResponse (..),
    newUpdateExperienceResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateExperience' smart constructor.
data UpdateExperience = UpdateExperience'
  { -- | A new name for your Amazon Kendra experience.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of a role with permission to access
    -- @Query@ API, @QuerySuggestions@ API, @SubmitFeedback@ API, and IAM
    -- Identity Center that stores your user and group information. For more
    -- information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/iam-roles.html IAM roles for Amazon Kendra>.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | Configuration information you want to update for your Amazon Kendra
    -- experience.
    configuration :: Prelude.Maybe ExperienceConfiguration,
    -- | A new description for your Amazon Kendra experience.
    description :: Prelude.Maybe Prelude.Text,
    -- | The identifier of your Amazon Kendra experience you want to update.
    id :: Prelude.Text,
    -- | The identifier of the index for your Amazon Kendra experience.
    indexId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateExperience' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateExperience_name' - A new name for your Amazon Kendra experience.
--
-- 'roleArn', 'updateExperience_roleArn' - The Amazon Resource Name (ARN) of a role with permission to access
-- @Query@ API, @QuerySuggestions@ API, @SubmitFeedback@ API, and IAM
-- Identity Center that stores your user and group information. For more
-- information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/iam-roles.html IAM roles for Amazon Kendra>.
--
-- 'configuration', 'updateExperience_configuration' - Configuration information you want to update for your Amazon Kendra
-- experience.
--
-- 'description', 'updateExperience_description' - A new description for your Amazon Kendra experience.
--
-- 'id', 'updateExperience_id' - The identifier of your Amazon Kendra experience you want to update.
--
-- 'indexId', 'updateExperience_indexId' - The identifier of the index for your Amazon Kendra experience.
newUpdateExperience ::
  -- | 'id'
  Prelude.Text ->
  -- | 'indexId'
  Prelude.Text ->
  UpdateExperience
newUpdateExperience pId_ pIndexId_ =
  UpdateExperience'
    { name = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      configuration = Prelude.Nothing,
      description = Prelude.Nothing,
      id = pId_,
      indexId = pIndexId_
    }

-- | A new name for your Amazon Kendra experience.
updateExperience_name :: Lens.Lens' UpdateExperience (Prelude.Maybe Prelude.Text)
updateExperience_name = Lens.lens (\UpdateExperience' {name} -> name) (\s@UpdateExperience' {} a -> s {name = a} :: UpdateExperience)

-- | The Amazon Resource Name (ARN) of a role with permission to access
-- @Query@ API, @QuerySuggestions@ API, @SubmitFeedback@ API, and IAM
-- Identity Center that stores your user and group information. For more
-- information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/iam-roles.html IAM roles for Amazon Kendra>.
updateExperience_roleArn :: Lens.Lens' UpdateExperience (Prelude.Maybe Prelude.Text)
updateExperience_roleArn = Lens.lens (\UpdateExperience' {roleArn} -> roleArn) (\s@UpdateExperience' {} a -> s {roleArn = a} :: UpdateExperience)

-- | Configuration information you want to update for your Amazon Kendra
-- experience.
updateExperience_configuration :: Lens.Lens' UpdateExperience (Prelude.Maybe ExperienceConfiguration)
updateExperience_configuration = Lens.lens (\UpdateExperience' {configuration} -> configuration) (\s@UpdateExperience' {} a -> s {configuration = a} :: UpdateExperience)

-- | A new description for your Amazon Kendra experience.
updateExperience_description :: Lens.Lens' UpdateExperience (Prelude.Maybe Prelude.Text)
updateExperience_description = Lens.lens (\UpdateExperience' {description} -> description) (\s@UpdateExperience' {} a -> s {description = a} :: UpdateExperience)

-- | The identifier of your Amazon Kendra experience you want to update.
updateExperience_id :: Lens.Lens' UpdateExperience Prelude.Text
updateExperience_id = Lens.lens (\UpdateExperience' {id} -> id) (\s@UpdateExperience' {} a -> s {id = a} :: UpdateExperience)

-- | The identifier of the index for your Amazon Kendra experience.
updateExperience_indexId :: Lens.Lens' UpdateExperience Prelude.Text
updateExperience_indexId = Lens.lens (\UpdateExperience' {indexId} -> indexId) (\s@UpdateExperience' {} a -> s {indexId = a} :: UpdateExperience)

instance Core.AWSRequest UpdateExperience where
  type
    AWSResponse UpdateExperience =
      UpdateExperienceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull UpdateExperienceResponse'

instance Prelude.Hashable UpdateExperience where
  hashWithSalt _salt UpdateExperience' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` configuration
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` indexId

instance Prelude.NFData UpdateExperience where
  rnf UpdateExperience' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf configuration
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf indexId

instance Data.ToHeaders UpdateExperience where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSKendraFrontendService.UpdateExperience" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateExperience where
  toJSON UpdateExperience' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Name" Data..=) Prelude.<$> name,
            ("RoleArn" Data..=) Prelude.<$> roleArn,
            ("Configuration" Data..=) Prelude.<$> configuration,
            ("Description" Data..=) Prelude.<$> description,
            Prelude.Just ("Id" Data..= id),
            Prelude.Just ("IndexId" Data..= indexId)
          ]
      )

instance Data.ToPath UpdateExperience where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateExperience where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateExperienceResponse' smart constructor.
data UpdateExperienceResponse = UpdateExperienceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateExperienceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateExperienceResponse ::
  UpdateExperienceResponse
newUpdateExperienceResponse =
  UpdateExperienceResponse'

instance Prelude.NFData UpdateExperienceResponse where
  rnf _ = ()
