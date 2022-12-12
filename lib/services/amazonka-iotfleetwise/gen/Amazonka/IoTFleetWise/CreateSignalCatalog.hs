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
-- Module      : Amazonka.IoTFleetWise.CreateSignalCatalog
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a collection of standardized signals that can be reused to
-- create vehicle models.
module Amazonka.IoTFleetWise.CreateSignalCatalog
  ( -- * Creating a Request
    CreateSignalCatalog (..),
    newCreateSignalCatalog,

    -- * Request Lenses
    createSignalCatalog_description,
    createSignalCatalog_nodes,
    createSignalCatalog_tags,
    createSignalCatalog_name,

    -- * Destructuring the Response
    CreateSignalCatalogResponse (..),
    newCreateSignalCatalogResponse,

    -- * Response Lenses
    createSignalCatalogResponse_httpStatus,
    createSignalCatalogResponse_name,
    createSignalCatalogResponse_arn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTFleetWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateSignalCatalog' smart constructor.
data CreateSignalCatalog = CreateSignalCatalog'
  { -- | A brief description of the signal catalog.
    description :: Prelude.Maybe Prelude.Text,
    -- | A list of information about nodes, which are a general abstraction of
    -- signals. For more information, see the API data type.
    nodes :: Prelude.Maybe [Node],
    -- | Metadata that can be used to manage the signal catalog.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the signal catalog to create.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSignalCatalog' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createSignalCatalog_description' - A brief description of the signal catalog.
--
-- 'nodes', 'createSignalCatalog_nodes' - A list of information about nodes, which are a general abstraction of
-- signals. For more information, see the API data type.
--
-- 'tags', 'createSignalCatalog_tags' - Metadata that can be used to manage the signal catalog.
--
-- 'name', 'createSignalCatalog_name' - The name of the signal catalog to create.
newCreateSignalCatalog ::
  -- | 'name'
  Prelude.Text ->
  CreateSignalCatalog
newCreateSignalCatalog pName_ =
  CreateSignalCatalog'
    { description = Prelude.Nothing,
      nodes = Prelude.Nothing,
      tags = Prelude.Nothing,
      name = pName_
    }

-- | A brief description of the signal catalog.
createSignalCatalog_description :: Lens.Lens' CreateSignalCatalog (Prelude.Maybe Prelude.Text)
createSignalCatalog_description = Lens.lens (\CreateSignalCatalog' {description} -> description) (\s@CreateSignalCatalog' {} a -> s {description = a} :: CreateSignalCatalog)

-- | A list of information about nodes, which are a general abstraction of
-- signals. For more information, see the API data type.
createSignalCatalog_nodes :: Lens.Lens' CreateSignalCatalog (Prelude.Maybe [Node])
createSignalCatalog_nodes = Lens.lens (\CreateSignalCatalog' {nodes} -> nodes) (\s@CreateSignalCatalog' {} a -> s {nodes = a} :: CreateSignalCatalog) Prelude.. Lens.mapping Lens.coerced

-- | Metadata that can be used to manage the signal catalog.
createSignalCatalog_tags :: Lens.Lens' CreateSignalCatalog (Prelude.Maybe [Tag])
createSignalCatalog_tags = Lens.lens (\CreateSignalCatalog' {tags} -> tags) (\s@CreateSignalCatalog' {} a -> s {tags = a} :: CreateSignalCatalog) Prelude.. Lens.mapping Lens.coerced

-- | The name of the signal catalog to create.
createSignalCatalog_name :: Lens.Lens' CreateSignalCatalog Prelude.Text
createSignalCatalog_name = Lens.lens (\CreateSignalCatalog' {name} -> name) (\s@CreateSignalCatalog' {} a -> s {name = a} :: CreateSignalCatalog)

instance Core.AWSRequest CreateSignalCatalog where
  type
    AWSResponse CreateSignalCatalog =
      CreateSignalCatalogResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSignalCatalogResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "name")
            Prelude.<*> (x Data..:> "arn")
      )

instance Prelude.Hashable CreateSignalCatalog where
  hashWithSalt _salt CreateSignalCatalog' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` nodes
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateSignalCatalog where
  rnf CreateSignalCatalog' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf nodes
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders CreateSignalCatalog where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "IoTAutobahnControlPlane.CreateSignalCatalog" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateSignalCatalog where
  toJSON CreateSignalCatalog' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("nodes" Data..=) Prelude.<$> nodes,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("name" Data..= name)
          ]
      )

instance Data.ToPath CreateSignalCatalog where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateSignalCatalog where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSignalCatalogResponse' smart constructor.
data CreateSignalCatalogResponse = CreateSignalCatalogResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name of the created signal catalog.
    name :: Prelude.Text,
    -- | The ARN of the created signal catalog.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSignalCatalogResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createSignalCatalogResponse_httpStatus' - The response's http status code.
--
-- 'name', 'createSignalCatalogResponse_name' - The name of the created signal catalog.
--
-- 'arn', 'createSignalCatalogResponse_arn' - The ARN of the created signal catalog.
newCreateSignalCatalogResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'name'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  CreateSignalCatalogResponse
newCreateSignalCatalogResponse
  pHttpStatus_
  pName_
  pArn_ =
    CreateSignalCatalogResponse'
      { httpStatus =
          pHttpStatus_,
        name = pName_,
        arn = pArn_
      }

-- | The response's http status code.
createSignalCatalogResponse_httpStatus :: Lens.Lens' CreateSignalCatalogResponse Prelude.Int
createSignalCatalogResponse_httpStatus = Lens.lens (\CreateSignalCatalogResponse' {httpStatus} -> httpStatus) (\s@CreateSignalCatalogResponse' {} a -> s {httpStatus = a} :: CreateSignalCatalogResponse)

-- | The name of the created signal catalog.
createSignalCatalogResponse_name :: Lens.Lens' CreateSignalCatalogResponse Prelude.Text
createSignalCatalogResponse_name = Lens.lens (\CreateSignalCatalogResponse' {name} -> name) (\s@CreateSignalCatalogResponse' {} a -> s {name = a} :: CreateSignalCatalogResponse)

-- | The ARN of the created signal catalog.
createSignalCatalogResponse_arn :: Lens.Lens' CreateSignalCatalogResponse Prelude.Text
createSignalCatalogResponse_arn = Lens.lens (\CreateSignalCatalogResponse' {arn} -> arn) (\s@CreateSignalCatalogResponse' {} a -> s {arn = a} :: CreateSignalCatalogResponse)

instance Prelude.NFData CreateSignalCatalogResponse where
  rnf CreateSignalCatalogResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf arn
