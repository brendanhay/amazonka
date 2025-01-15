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
-- Module      : Amazonka.IoTFleetWise.ImportSignalCatalog
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a signal catalog using your existing VSS formatted content from
-- your local device.
module Amazonka.IoTFleetWise.ImportSignalCatalog
  ( -- * Creating a Request
    ImportSignalCatalog (..),
    newImportSignalCatalog,

    -- * Request Lenses
    importSignalCatalog_description,
    importSignalCatalog_tags,
    importSignalCatalog_vss,
    importSignalCatalog_name,

    -- * Destructuring the Response
    ImportSignalCatalogResponse (..),
    newImportSignalCatalogResponse,

    -- * Response Lenses
    importSignalCatalogResponse_httpStatus,
    importSignalCatalogResponse_name,
    importSignalCatalogResponse_arn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTFleetWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newImportSignalCatalog' smart constructor.
data ImportSignalCatalog = ImportSignalCatalog'
  { -- | A brief description of the signal catalog.
    description :: Prelude.Maybe Prelude.Text,
    -- | Metadata that can be used to manage the signal catalog.
    tags :: Prelude.Maybe [Tag],
    -- | The contents of the Vehicle Signal Specification (VSS) configuration.
    -- VSS is a precise language used to describe and model signals in vehicle
    -- networks.
    vss :: Prelude.Maybe FormattedVss,
    -- | The name of the signal catalog to import.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportSignalCatalog' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'importSignalCatalog_description' - A brief description of the signal catalog.
--
-- 'tags', 'importSignalCatalog_tags' - Metadata that can be used to manage the signal catalog.
--
-- 'vss', 'importSignalCatalog_vss' - The contents of the Vehicle Signal Specification (VSS) configuration.
-- VSS is a precise language used to describe and model signals in vehicle
-- networks.
--
-- 'name', 'importSignalCatalog_name' - The name of the signal catalog to import.
newImportSignalCatalog ::
  -- | 'name'
  Prelude.Text ->
  ImportSignalCatalog
newImportSignalCatalog pName_ =
  ImportSignalCatalog'
    { description = Prelude.Nothing,
      tags = Prelude.Nothing,
      vss = Prelude.Nothing,
      name = pName_
    }

-- | A brief description of the signal catalog.
importSignalCatalog_description :: Lens.Lens' ImportSignalCatalog (Prelude.Maybe Prelude.Text)
importSignalCatalog_description = Lens.lens (\ImportSignalCatalog' {description} -> description) (\s@ImportSignalCatalog' {} a -> s {description = a} :: ImportSignalCatalog)

-- | Metadata that can be used to manage the signal catalog.
importSignalCatalog_tags :: Lens.Lens' ImportSignalCatalog (Prelude.Maybe [Tag])
importSignalCatalog_tags = Lens.lens (\ImportSignalCatalog' {tags} -> tags) (\s@ImportSignalCatalog' {} a -> s {tags = a} :: ImportSignalCatalog) Prelude.. Lens.mapping Lens.coerced

-- | The contents of the Vehicle Signal Specification (VSS) configuration.
-- VSS is a precise language used to describe and model signals in vehicle
-- networks.
importSignalCatalog_vss :: Lens.Lens' ImportSignalCatalog (Prelude.Maybe FormattedVss)
importSignalCatalog_vss = Lens.lens (\ImportSignalCatalog' {vss} -> vss) (\s@ImportSignalCatalog' {} a -> s {vss = a} :: ImportSignalCatalog)

-- | The name of the signal catalog to import.
importSignalCatalog_name :: Lens.Lens' ImportSignalCatalog Prelude.Text
importSignalCatalog_name = Lens.lens (\ImportSignalCatalog' {name} -> name) (\s@ImportSignalCatalog' {} a -> s {name = a} :: ImportSignalCatalog)

instance Core.AWSRequest ImportSignalCatalog where
  type
    AWSResponse ImportSignalCatalog =
      ImportSignalCatalogResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ImportSignalCatalogResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "name")
            Prelude.<*> (x Data..:> "arn")
      )

instance Prelude.Hashable ImportSignalCatalog where
  hashWithSalt _salt ImportSignalCatalog' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` vss
      `Prelude.hashWithSalt` name

instance Prelude.NFData ImportSignalCatalog where
  rnf ImportSignalCatalog' {..} =
    Prelude.rnf description `Prelude.seq`
      Prelude.rnf tags `Prelude.seq`
        Prelude.rnf vss `Prelude.seq`
          Prelude.rnf name

instance Data.ToHeaders ImportSignalCatalog where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "IoTAutobahnControlPlane.ImportSignalCatalog" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ImportSignalCatalog where
  toJSON ImportSignalCatalog' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("tags" Data..=) Prelude.<$> tags,
            ("vss" Data..=) Prelude.<$> vss,
            Prelude.Just ("name" Data..= name)
          ]
      )

instance Data.ToPath ImportSignalCatalog where
  toPath = Prelude.const "/"

instance Data.ToQuery ImportSignalCatalog where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newImportSignalCatalogResponse' smart constructor.
data ImportSignalCatalogResponse = ImportSignalCatalogResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name of the imported signal catalog.
    name :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the imported signal catalog.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportSignalCatalogResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'importSignalCatalogResponse_httpStatus' - The response's http status code.
--
-- 'name', 'importSignalCatalogResponse_name' - The name of the imported signal catalog.
--
-- 'arn', 'importSignalCatalogResponse_arn' - The Amazon Resource Name (ARN) of the imported signal catalog.
newImportSignalCatalogResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'name'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  ImportSignalCatalogResponse
newImportSignalCatalogResponse
  pHttpStatus_
  pName_
  pArn_ =
    ImportSignalCatalogResponse'
      { httpStatus =
          pHttpStatus_,
        name = pName_,
        arn = pArn_
      }

-- | The response's http status code.
importSignalCatalogResponse_httpStatus :: Lens.Lens' ImportSignalCatalogResponse Prelude.Int
importSignalCatalogResponse_httpStatus = Lens.lens (\ImportSignalCatalogResponse' {httpStatus} -> httpStatus) (\s@ImportSignalCatalogResponse' {} a -> s {httpStatus = a} :: ImportSignalCatalogResponse)

-- | The name of the imported signal catalog.
importSignalCatalogResponse_name :: Lens.Lens' ImportSignalCatalogResponse Prelude.Text
importSignalCatalogResponse_name = Lens.lens (\ImportSignalCatalogResponse' {name} -> name) (\s@ImportSignalCatalogResponse' {} a -> s {name = a} :: ImportSignalCatalogResponse)

-- | The Amazon Resource Name (ARN) of the imported signal catalog.
importSignalCatalogResponse_arn :: Lens.Lens' ImportSignalCatalogResponse Prelude.Text
importSignalCatalogResponse_arn = Lens.lens (\ImportSignalCatalogResponse' {arn} -> arn) (\s@ImportSignalCatalogResponse' {} a -> s {arn = a} :: ImportSignalCatalogResponse)

instance Prelude.NFData ImportSignalCatalogResponse where
  rnf ImportSignalCatalogResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf name `Prelude.seq`
        Prelude.rnf arn
