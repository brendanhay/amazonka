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
-- Module      : Amazonka.Schemas.CreateRegistry
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a registry.
module Amazonka.Schemas.CreateRegistry
  ( -- * Creating a Request
    CreateRegistry (..),
    newCreateRegistry,

    -- * Request Lenses
    createRegistry_tags,
    createRegistry_description,
    createRegistry_registryName,

    -- * Destructuring the Response
    CreateRegistryResponse (..),
    newCreateRegistryResponse,

    -- * Response Lenses
    createRegistryResponse_tags,
    createRegistryResponse_registryName,
    createRegistryResponse_description,
    createRegistryResponse_registryArn,
    createRegistryResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Schemas.Types

-- | /See:/ 'newCreateRegistry' smart constructor.
data CreateRegistry = CreateRegistry'
  { -- | Tags to associate with the registry.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A description of the registry to be created.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the registry.
    registryName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRegistry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createRegistry_tags' - Tags to associate with the registry.
--
-- 'description', 'createRegistry_description' - A description of the registry to be created.
--
-- 'registryName', 'createRegistry_registryName' - The name of the registry.
newCreateRegistry ::
  -- | 'registryName'
  Prelude.Text ->
  CreateRegistry
newCreateRegistry pRegistryName_ =
  CreateRegistry'
    { tags = Prelude.Nothing,
      description = Prelude.Nothing,
      registryName = pRegistryName_
    }

-- | Tags to associate with the registry.
createRegistry_tags :: Lens.Lens' CreateRegistry (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createRegistry_tags = Lens.lens (\CreateRegistry' {tags} -> tags) (\s@CreateRegistry' {} a -> s {tags = a} :: CreateRegistry) Prelude.. Lens.mapping Lens.coerced

-- | A description of the registry to be created.
createRegistry_description :: Lens.Lens' CreateRegistry (Prelude.Maybe Prelude.Text)
createRegistry_description = Lens.lens (\CreateRegistry' {description} -> description) (\s@CreateRegistry' {} a -> s {description = a} :: CreateRegistry)

-- | The name of the registry.
createRegistry_registryName :: Lens.Lens' CreateRegistry Prelude.Text
createRegistry_registryName = Lens.lens (\CreateRegistry' {registryName} -> registryName) (\s@CreateRegistry' {} a -> s {registryName = a} :: CreateRegistry)

instance Core.AWSRequest CreateRegistry where
  type
    AWSResponse CreateRegistry =
      CreateRegistryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateRegistryResponse'
            Prelude.<$> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "RegistryName")
            Prelude.<*> (x Core..?> "Description")
            Prelude.<*> (x Core..?> "RegistryArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateRegistry where
  hashWithSalt _salt CreateRegistry' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` registryName

instance Prelude.NFData CreateRegistry where
  rnf CreateRegistry' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf registryName

instance Core.ToHeaders CreateRegistry where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateRegistry where
  toJSON CreateRegistry' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("tags" Core..=) Prelude.<$> tags,
            ("Description" Core..=) Prelude.<$> description
          ]
      )

instance Core.ToPath CreateRegistry where
  toPath CreateRegistry' {..} =
    Prelude.mconcat
      ["/v1/registries/name/", Core.toBS registryName]

instance Core.ToQuery CreateRegistry where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateRegistryResponse' smart constructor.
data CreateRegistryResponse = CreateRegistryResponse'
  { -- | Tags associated with the registry.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the registry.
    registryName :: Prelude.Maybe Prelude.Text,
    -- | The description of the registry.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the registry.
    registryArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRegistryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createRegistryResponse_tags' - Tags associated with the registry.
--
-- 'registryName', 'createRegistryResponse_registryName' - The name of the registry.
--
-- 'description', 'createRegistryResponse_description' - The description of the registry.
--
-- 'registryArn', 'createRegistryResponse_registryArn' - The ARN of the registry.
--
-- 'httpStatus', 'createRegistryResponse_httpStatus' - The response's http status code.
newCreateRegistryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateRegistryResponse
newCreateRegistryResponse pHttpStatus_ =
  CreateRegistryResponse'
    { tags = Prelude.Nothing,
      registryName = Prelude.Nothing,
      description = Prelude.Nothing,
      registryArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Tags associated with the registry.
createRegistryResponse_tags :: Lens.Lens' CreateRegistryResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createRegistryResponse_tags = Lens.lens (\CreateRegistryResponse' {tags} -> tags) (\s@CreateRegistryResponse' {} a -> s {tags = a} :: CreateRegistryResponse) Prelude.. Lens.mapping Lens.coerced

-- | The name of the registry.
createRegistryResponse_registryName :: Lens.Lens' CreateRegistryResponse (Prelude.Maybe Prelude.Text)
createRegistryResponse_registryName = Lens.lens (\CreateRegistryResponse' {registryName} -> registryName) (\s@CreateRegistryResponse' {} a -> s {registryName = a} :: CreateRegistryResponse)

-- | The description of the registry.
createRegistryResponse_description :: Lens.Lens' CreateRegistryResponse (Prelude.Maybe Prelude.Text)
createRegistryResponse_description = Lens.lens (\CreateRegistryResponse' {description} -> description) (\s@CreateRegistryResponse' {} a -> s {description = a} :: CreateRegistryResponse)

-- | The ARN of the registry.
createRegistryResponse_registryArn :: Lens.Lens' CreateRegistryResponse (Prelude.Maybe Prelude.Text)
createRegistryResponse_registryArn = Lens.lens (\CreateRegistryResponse' {registryArn} -> registryArn) (\s@CreateRegistryResponse' {} a -> s {registryArn = a} :: CreateRegistryResponse)

-- | The response's http status code.
createRegistryResponse_httpStatus :: Lens.Lens' CreateRegistryResponse Prelude.Int
createRegistryResponse_httpStatus = Lens.lens (\CreateRegistryResponse' {httpStatus} -> httpStatus) (\s@CreateRegistryResponse' {} a -> s {httpStatus = a} :: CreateRegistryResponse)

instance Prelude.NFData CreateRegistryResponse where
  rnf CreateRegistryResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf registryName
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf registryArn
      `Prelude.seq` Prelude.rnf httpStatus
