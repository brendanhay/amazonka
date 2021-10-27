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
-- Module      : Network.AWS.Kendra.UpdateDataSource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing Amazon Kendra data source.
module Network.AWS.Kendra.UpdateDataSource
  ( -- * Creating a Request
    UpdateDataSource (..),
    newUpdateDataSource,

    -- * Request Lenses
    updateDataSource_languageCode,
    updateDataSource_schedule,
    updateDataSource_name,
    updateDataSource_configuration,
    updateDataSource_description,
    updateDataSource_roleArn,
    updateDataSource_id,
    updateDataSource_indexId,

    -- * Destructuring the Response
    UpdateDataSourceResponse (..),
    newUpdateDataSourceResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Kendra.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateDataSource' smart constructor.
data UpdateDataSource = UpdateDataSource'
  { -- | The code for a language. This allows you to support a language for all
    -- documents when updating the data source. English is supported by
    -- default. For more information on supported languages, including their
    -- codes, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/in-adding-languages.html Adding documents in languages other than English>.
    languageCode :: Prelude.Maybe Prelude.Text,
    -- | The new update schedule for the data source.
    schedule :: Prelude.Maybe Prelude.Text,
    -- | The name of the data source to update. The name of the data source
    -- can\'t be updated. To rename a data source you must delete the data
    -- source and re-create it.
    name :: Prelude.Maybe Prelude.Text,
    configuration :: Prelude.Maybe DataSourceConfiguration,
    -- | The new description for the data source.
    description :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the new role to use when the data
    -- source is accessing resources on your behalf.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the data source to update.
    id :: Prelude.Text,
    -- | The identifier of the index that contains the data source to update.
    indexId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDataSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'languageCode', 'updateDataSource_languageCode' - The code for a language. This allows you to support a language for all
-- documents when updating the data source. English is supported by
-- default. For more information on supported languages, including their
-- codes, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/in-adding-languages.html Adding documents in languages other than English>.
--
-- 'schedule', 'updateDataSource_schedule' - The new update schedule for the data source.
--
-- 'name', 'updateDataSource_name' - The name of the data source to update. The name of the data source
-- can\'t be updated. To rename a data source you must delete the data
-- source and re-create it.
--
-- 'configuration', 'updateDataSource_configuration' - Undocumented member.
--
-- 'description', 'updateDataSource_description' - The new description for the data source.
--
-- 'roleArn', 'updateDataSource_roleArn' - The Amazon Resource Name (ARN) of the new role to use when the data
-- source is accessing resources on your behalf.
--
-- 'id', 'updateDataSource_id' - The unique identifier of the data source to update.
--
-- 'indexId', 'updateDataSource_indexId' - The identifier of the index that contains the data source to update.
newUpdateDataSource ::
  -- | 'id'
  Prelude.Text ->
  -- | 'indexId'
  Prelude.Text ->
  UpdateDataSource
newUpdateDataSource pId_ pIndexId_ =
  UpdateDataSource'
    { languageCode = Prelude.Nothing,
      schedule = Prelude.Nothing,
      name = Prelude.Nothing,
      configuration = Prelude.Nothing,
      description = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      id = pId_,
      indexId = pIndexId_
    }

-- | The code for a language. This allows you to support a language for all
-- documents when updating the data source. English is supported by
-- default. For more information on supported languages, including their
-- codes, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/in-adding-languages.html Adding documents in languages other than English>.
updateDataSource_languageCode :: Lens.Lens' UpdateDataSource (Prelude.Maybe Prelude.Text)
updateDataSource_languageCode = Lens.lens (\UpdateDataSource' {languageCode} -> languageCode) (\s@UpdateDataSource' {} a -> s {languageCode = a} :: UpdateDataSource)

-- | The new update schedule for the data source.
updateDataSource_schedule :: Lens.Lens' UpdateDataSource (Prelude.Maybe Prelude.Text)
updateDataSource_schedule = Lens.lens (\UpdateDataSource' {schedule} -> schedule) (\s@UpdateDataSource' {} a -> s {schedule = a} :: UpdateDataSource)

-- | The name of the data source to update. The name of the data source
-- can\'t be updated. To rename a data source you must delete the data
-- source and re-create it.
updateDataSource_name :: Lens.Lens' UpdateDataSource (Prelude.Maybe Prelude.Text)
updateDataSource_name = Lens.lens (\UpdateDataSource' {name} -> name) (\s@UpdateDataSource' {} a -> s {name = a} :: UpdateDataSource)

-- | Undocumented member.
updateDataSource_configuration :: Lens.Lens' UpdateDataSource (Prelude.Maybe DataSourceConfiguration)
updateDataSource_configuration = Lens.lens (\UpdateDataSource' {configuration} -> configuration) (\s@UpdateDataSource' {} a -> s {configuration = a} :: UpdateDataSource)

-- | The new description for the data source.
updateDataSource_description :: Lens.Lens' UpdateDataSource (Prelude.Maybe Prelude.Text)
updateDataSource_description = Lens.lens (\UpdateDataSource' {description} -> description) (\s@UpdateDataSource' {} a -> s {description = a} :: UpdateDataSource)

-- | The Amazon Resource Name (ARN) of the new role to use when the data
-- source is accessing resources on your behalf.
updateDataSource_roleArn :: Lens.Lens' UpdateDataSource (Prelude.Maybe Prelude.Text)
updateDataSource_roleArn = Lens.lens (\UpdateDataSource' {roleArn} -> roleArn) (\s@UpdateDataSource' {} a -> s {roleArn = a} :: UpdateDataSource)

-- | The unique identifier of the data source to update.
updateDataSource_id :: Lens.Lens' UpdateDataSource Prelude.Text
updateDataSource_id = Lens.lens (\UpdateDataSource' {id} -> id) (\s@UpdateDataSource' {} a -> s {id = a} :: UpdateDataSource)

-- | The identifier of the index that contains the data source to update.
updateDataSource_indexId :: Lens.Lens' UpdateDataSource Prelude.Text
updateDataSource_indexId = Lens.lens (\UpdateDataSource' {indexId} -> indexId) (\s@UpdateDataSource' {} a -> s {indexId = a} :: UpdateDataSource)

instance Core.AWSRequest UpdateDataSource where
  type
    AWSResponse UpdateDataSource =
      UpdateDataSourceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull UpdateDataSourceResponse'

instance Prelude.Hashable UpdateDataSource

instance Prelude.NFData UpdateDataSource

instance Core.ToHeaders UpdateDataSource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSKendraFrontendService.UpdateDataSource" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateDataSource where
  toJSON UpdateDataSource' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("LanguageCode" Core..=) Prelude.<$> languageCode,
            ("Schedule" Core..=) Prelude.<$> schedule,
            ("Name" Core..=) Prelude.<$> name,
            ("Configuration" Core..=) Prelude.<$> configuration,
            ("Description" Core..=) Prelude.<$> description,
            ("RoleArn" Core..=) Prelude.<$> roleArn,
            Prelude.Just ("Id" Core..= id),
            Prelude.Just ("IndexId" Core..= indexId)
          ]
      )

instance Core.ToPath UpdateDataSource where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateDataSource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDataSourceResponse' smart constructor.
data UpdateDataSourceResponse = UpdateDataSourceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDataSourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateDataSourceResponse ::
  UpdateDataSourceResponse
newUpdateDataSourceResponse =
  UpdateDataSourceResponse'

instance Prelude.NFData UpdateDataSourceResponse
