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
-- Module      : Network.AWS.Glue.CreateRegistry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new registry which may be used to hold a collection of
-- schemas.
module Network.AWS.Glue.CreateRegistry
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
    createRegistryResponse_registryName,
    createRegistryResponse_tags,
    createRegistryResponse_description,
    createRegistryResponse_registryArn,
    createRegistryResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateRegistry' smart constructor.
data CreateRegistry = CreateRegistry'
  { -- | AWS tags that contain a key value pair and may be searched by console,
    -- command line, or API.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | A description of the registry. If description is not provided, there
    -- will not be any default value for this.
    description :: Core.Maybe Core.Text,
    -- | Name of the registry to be created of max length of 255, and may only
    -- contain letters, numbers, hyphen, underscore, dollar sign, or hash mark.
    -- No whitespace.
    registryName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateRegistry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createRegistry_tags' - AWS tags that contain a key value pair and may be searched by console,
-- command line, or API.
--
-- 'description', 'createRegistry_description' - A description of the registry. If description is not provided, there
-- will not be any default value for this.
--
-- 'registryName', 'createRegistry_registryName' - Name of the registry to be created of max length of 255, and may only
-- contain letters, numbers, hyphen, underscore, dollar sign, or hash mark.
-- No whitespace.
newCreateRegistry ::
  -- | 'registryName'
  Core.Text ->
  CreateRegistry
newCreateRegistry pRegistryName_ =
  CreateRegistry'
    { tags = Core.Nothing,
      description = Core.Nothing,
      registryName = pRegistryName_
    }

-- | AWS tags that contain a key value pair and may be searched by console,
-- command line, or API.
createRegistry_tags :: Lens.Lens' CreateRegistry (Core.Maybe (Core.HashMap Core.Text Core.Text))
createRegistry_tags = Lens.lens (\CreateRegistry' {tags} -> tags) (\s@CreateRegistry' {} a -> s {tags = a} :: CreateRegistry) Core.. Lens.mapping Lens._Coerce

-- | A description of the registry. If description is not provided, there
-- will not be any default value for this.
createRegistry_description :: Lens.Lens' CreateRegistry (Core.Maybe Core.Text)
createRegistry_description = Lens.lens (\CreateRegistry' {description} -> description) (\s@CreateRegistry' {} a -> s {description = a} :: CreateRegistry)

-- | Name of the registry to be created of max length of 255, and may only
-- contain letters, numbers, hyphen, underscore, dollar sign, or hash mark.
-- No whitespace.
createRegistry_registryName :: Lens.Lens' CreateRegistry Core.Text
createRegistry_registryName = Lens.lens (\CreateRegistry' {registryName} -> registryName) (\s@CreateRegistry' {} a -> s {registryName = a} :: CreateRegistry)

instance Core.AWSRequest CreateRegistry where
  type
    AWSResponse CreateRegistry =
      CreateRegistryResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateRegistryResponse'
            Core.<$> (x Core..?> "RegistryName")
            Core.<*> (x Core..?> "Tags" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "Description")
            Core.<*> (x Core..?> "RegistryArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateRegistry

instance Core.NFData CreateRegistry

instance Core.ToHeaders CreateRegistry where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.CreateRegistry" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateRegistry where
  toJSON CreateRegistry' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Tags" Core..=) Core.<$> tags,
            ("Description" Core..=) Core.<$> description,
            Core.Just ("RegistryName" Core..= registryName)
          ]
      )

instance Core.ToPath CreateRegistry where
  toPath = Core.const "/"

instance Core.ToQuery CreateRegistry where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateRegistryResponse' smart constructor.
data CreateRegistryResponse = CreateRegistryResponse'
  { -- | The name of the registry.
    registryName :: Core.Maybe Core.Text,
    -- | The tags for the registry.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | A description of the registry.
    description :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the newly created registry.
    registryArn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateRegistryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'registryName', 'createRegistryResponse_registryName' - The name of the registry.
--
-- 'tags', 'createRegistryResponse_tags' - The tags for the registry.
--
-- 'description', 'createRegistryResponse_description' - A description of the registry.
--
-- 'registryArn', 'createRegistryResponse_registryArn' - The Amazon Resource Name (ARN) of the newly created registry.
--
-- 'httpStatus', 'createRegistryResponse_httpStatus' - The response's http status code.
newCreateRegistryResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateRegistryResponse
newCreateRegistryResponse pHttpStatus_ =
  CreateRegistryResponse'
    { registryName =
        Core.Nothing,
      tags = Core.Nothing,
      description = Core.Nothing,
      registryArn = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the registry.
createRegistryResponse_registryName :: Lens.Lens' CreateRegistryResponse (Core.Maybe Core.Text)
createRegistryResponse_registryName = Lens.lens (\CreateRegistryResponse' {registryName} -> registryName) (\s@CreateRegistryResponse' {} a -> s {registryName = a} :: CreateRegistryResponse)

-- | The tags for the registry.
createRegistryResponse_tags :: Lens.Lens' CreateRegistryResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
createRegistryResponse_tags = Lens.lens (\CreateRegistryResponse' {tags} -> tags) (\s@CreateRegistryResponse' {} a -> s {tags = a} :: CreateRegistryResponse) Core.. Lens.mapping Lens._Coerce

-- | A description of the registry.
createRegistryResponse_description :: Lens.Lens' CreateRegistryResponse (Core.Maybe Core.Text)
createRegistryResponse_description = Lens.lens (\CreateRegistryResponse' {description} -> description) (\s@CreateRegistryResponse' {} a -> s {description = a} :: CreateRegistryResponse)

-- | The Amazon Resource Name (ARN) of the newly created registry.
createRegistryResponse_registryArn :: Lens.Lens' CreateRegistryResponse (Core.Maybe Core.Text)
createRegistryResponse_registryArn = Lens.lens (\CreateRegistryResponse' {registryArn} -> registryArn) (\s@CreateRegistryResponse' {} a -> s {registryArn = a} :: CreateRegistryResponse)

-- | The response's http status code.
createRegistryResponse_httpStatus :: Lens.Lens' CreateRegistryResponse Core.Int
createRegistryResponse_httpStatus = Lens.lens (\CreateRegistryResponse' {httpStatus} -> httpStatus) (\s@CreateRegistryResponse' {} a -> s {httpStatus = a} :: CreateRegistryResponse)

instance Core.NFData CreateRegistryResponse
