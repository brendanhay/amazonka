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
-- Module      : Amazonka.DataBrew.UpdateDataset
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the definition of an existing DataBrew dataset.
module Amazonka.DataBrew.UpdateDataset
  ( -- * Creating a Request
    UpdateDataset (..),
    newUpdateDataset,

    -- * Request Lenses
    updateDataset_pathOptions,
    updateDataset_format,
    updateDataset_formatOptions,
    updateDataset_name,
    updateDataset_input,

    -- * Destructuring the Response
    UpdateDatasetResponse (..),
    newUpdateDatasetResponse,

    -- * Response Lenses
    updateDatasetResponse_httpStatus,
    updateDatasetResponse_name,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataBrew.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateDataset' smart constructor.
data UpdateDataset = UpdateDataset'
  { -- | A set of options that defines how DataBrew interprets an Amazon S3 path
    -- of the dataset.
    pathOptions :: Prelude.Maybe PathOptions,
    -- | The file format of a dataset that is created from an Amazon S3 file or
    -- folder.
    format :: Prelude.Maybe InputFormat,
    formatOptions :: Prelude.Maybe FormatOptions,
    -- | The name of the dataset to be updated.
    name :: Prelude.Text,
    input :: Input
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDataset' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pathOptions', 'updateDataset_pathOptions' - A set of options that defines how DataBrew interprets an Amazon S3 path
-- of the dataset.
--
-- 'format', 'updateDataset_format' - The file format of a dataset that is created from an Amazon S3 file or
-- folder.
--
-- 'formatOptions', 'updateDataset_formatOptions' - Undocumented member.
--
-- 'name', 'updateDataset_name' - The name of the dataset to be updated.
--
-- 'input', 'updateDataset_input' - Undocumented member.
newUpdateDataset ::
  -- | 'name'
  Prelude.Text ->
  -- | 'input'
  Input ->
  UpdateDataset
newUpdateDataset pName_ pInput_ =
  UpdateDataset'
    { pathOptions = Prelude.Nothing,
      format = Prelude.Nothing,
      formatOptions = Prelude.Nothing,
      name = pName_,
      input = pInput_
    }

-- | A set of options that defines how DataBrew interprets an Amazon S3 path
-- of the dataset.
updateDataset_pathOptions :: Lens.Lens' UpdateDataset (Prelude.Maybe PathOptions)
updateDataset_pathOptions = Lens.lens (\UpdateDataset' {pathOptions} -> pathOptions) (\s@UpdateDataset' {} a -> s {pathOptions = a} :: UpdateDataset)

-- | The file format of a dataset that is created from an Amazon S3 file or
-- folder.
updateDataset_format :: Lens.Lens' UpdateDataset (Prelude.Maybe InputFormat)
updateDataset_format = Lens.lens (\UpdateDataset' {format} -> format) (\s@UpdateDataset' {} a -> s {format = a} :: UpdateDataset)

-- | Undocumented member.
updateDataset_formatOptions :: Lens.Lens' UpdateDataset (Prelude.Maybe FormatOptions)
updateDataset_formatOptions = Lens.lens (\UpdateDataset' {formatOptions} -> formatOptions) (\s@UpdateDataset' {} a -> s {formatOptions = a} :: UpdateDataset)

-- | The name of the dataset to be updated.
updateDataset_name :: Lens.Lens' UpdateDataset Prelude.Text
updateDataset_name = Lens.lens (\UpdateDataset' {name} -> name) (\s@UpdateDataset' {} a -> s {name = a} :: UpdateDataset)

-- | Undocumented member.
updateDataset_input :: Lens.Lens' UpdateDataset Input
updateDataset_input = Lens.lens (\UpdateDataset' {input} -> input) (\s@UpdateDataset' {} a -> s {input = a} :: UpdateDataset)

instance Core.AWSRequest UpdateDataset where
  type
    AWSResponse UpdateDataset =
      UpdateDatasetResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateDatasetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "Name")
      )

instance Prelude.Hashable UpdateDataset where
  hashWithSalt _salt UpdateDataset' {..} =
    _salt `Prelude.hashWithSalt` pathOptions
      `Prelude.hashWithSalt` format
      `Prelude.hashWithSalt` formatOptions
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` input

instance Prelude.NFData UpdateDataset where
  rnf UpdateDataset' {..} =
    Prelude.rnf pathOptions
      `Prelude.seq` Prelude.rnf format
      `Prelude.seq` Prelude.rnf formatOptions
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf input

instance Data.ToHeaders UpdateDataset where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateDataset where
  toJSON UpdateDataset' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("PathOptions" Data..=) Prelude.<$> pathOptions,
            ("Format" Data..=) Prelude.<$> format,
            ("FormatOptions" Data..=) Prelude.<$> formatOptions,
            Prelude.Just ("Input" Data..= input)
          ]
      )

instance Data.ToPath UpdateDataset where
  toPath UpdateDataset' {..} =
    Prelude.mconcat ["/datasets/", Data.toBS name]

instance Data.ToQuery UpdateDataset where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDatasetResponse' smart constructor.
data UpdateDatasetResponse = UpdateDatasetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name of the dataset that you updated.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDatasetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateDatasetResponse_httpStatus' - The response's http status code.
--
-- 'name', 'updateDatasetResponse_name' - The name of the dataset that you updated.
newUpdateDatasetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'name'
  Prelude.Text ->
  UpdateDatasetResponse
newUpdateDatasetResponse pHttpStatus_ pName_ =
  UpdateDatasetResponse'
    { httpStatus = pHttpStatus_,
      name = pName_
    }

-- | The response's http status code.
updateDatasetResponse_httpStatus :: Lens.Lens' UpdateDatasetResponse Prelude.Int
updateDatasetResponse_httpStatus = Lens.lens (\UpdateDatasetResponse' {httpStatus} -> httpStatus) (\s@UpdateDatasetResponse' {} a -> s {httpStatus = a} :: UpdateDatasetResponse)

-- | The name of the dataset that you updated.
updateDatasetResponse_name :: Lens.Lens' UpdateDatasetResponse Prelude.Text
updateDatasetResponse_name = Lens.lens (\UpdateDatasetResponse' {name} -> name) (\s@UpdateDatasetResponse' {} a -> s {name = a} :: UpdateDatasetResponse)

instance Prelude.NFData UpdateDatasetResponse where
  rnf UpdateDatasetResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf name
