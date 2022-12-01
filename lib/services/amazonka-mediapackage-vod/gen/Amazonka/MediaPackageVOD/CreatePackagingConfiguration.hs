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
-- Module      : Amazonka.MediaPackageVOD.CreatePackagingConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new MediaPackage VOD PackagingConfiguration resource.
module Amazonka.MediaPackageVOD.CreatePackagingConfiguration
  ( -- * Creating a Request
    CreatePackagingConfiguration (..),
    newCreatePackagingConfiguration,

    -- * Request Lenses
    createPackagingConfiguration_tags,
    createPackagingConfiguration_mssPackage,
    createPackagingConfiguration_dashPackage,
    createPackagingConfiguration_cmafPackage,
    createPackagingConfiguration_hlsPackage,
    createPackagingConfiguration_id,
    createPackagingConfiguration_packagingGroupId,

    -- * Destructuring the Response
    CreatePackagingConfigurationResponse (..),
    newCreatePackagingConfigurationResponse,

    -- * Response Lenses
    createPackagingConfigurationResponse_tags,
    createPackagingConfigurationResponse_mssPackage,
    createPackagingConfigurationResponse_packagingGroupId,
    createPackagingConfigurationResponse_arn,
    createPackagingConfigurationResponse_id,
    createPackagingConfigurationResponse_dashPackage,
    createPackagingConfigurationResponse_cmafPackage,
    createPackagingConfigurationResponse_hlsPackage,
    createPackagingConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaPackageVOD.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | A new MediaPackage VOD PackagingConfiguration resource configuration.
--
-- /See:/ 'newCreatePackagingConfiguration' smart constructor.
data CreatePackagingConfiguration = CreatePackagingConfiguration'
  { tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    mssPackage :: Prelude.Maybe MssPackage,
    dashPackage :: Prelude.Maybe DashPackage,
    cmafPackage :: Prelude.Maybe CmafPackage,
    hlsPackage :: Prelude.Maybe HlsPackage,
    -- | The ID of the PackagingConfiguration.
    id :: Prelude.Text,
    -- | The ID of a PackagingGroup.
    packagingGroupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePackagingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createPackagingConfiguration_tags' - Undocumented member.
--
-- 'mssPackage', 'createPackagingConfiguration_mssPackage' - Undocumented member.
--
-- 'dashPackage', 'createPackagingConfiguration_dashPackage' - Undocumented member.
--
-- 'cmafPackage', 'createPackagingConfiguration_cmafPackage' - Undocumented member.
--
-- 'hlsPackage', 'createPackagingConfiguration_hlsPackage' - Undocumented member.
--
-- 'id', 'createPackagingConfiguration_id' - The ID of the PackagingConfiguration.
--
-- 'packagingGroupId', 'createPackagingConfiguration_packagingGroupId' - The ID of a PackagingGroup.
newCreatePackagingConfiguration ::
  -- | 'id'
  Prelude.Text ->
  -- | 'packagingGroupId'
  Prelude.Text ->
  CreatePackagingConfiguration
newCreatePackagingConfiguration
  pId_
  pPackagingGroupId_ =
    CreatePackagingConfiguration'
      { tags =
          Prelude.Nothing,
        mssPackage = Prelude.Nothing,
        dashPackage = Prelude.Nothing,
        cmafPackage = Prelude.Nothing,
        hlsPackage = Prelude.Nothing,
        id = pId_,
        packagingGroupId = pPackagingGroupId_
      }

-- | Undocumented member.
createPackagingConfiguration_tags :: Lens.Lens' CreatePackagingConfiguration (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createPackagingConfiguration_tags = Lens.lens (\CreatePackagingConfiguration' {tags} -> tags) (\s@CreatePackagingConfiguration' {} a -> s {tags = a} :: CreatePackagingConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
createPackagingConfiguration_mssPackage :: Lens.Lens' CreatePackagingConfiguration (Prelude.Maybe MssPackage)
createPackagingConfiguration_mssPackage = Lens.lens (\CreatePackagingConfiguration' {mssPackage} -> mssPackage) (\s@CreatePackagingConfiguration' {} a -> s {mssPackage = a} :: CreatePackagingConfiguration)

-- | Undocumented member.
createPackagingConfiguration_dashPackage :: Lens.Lens' CreatePackagingConfiguration (Prelude.Maybe DashPackage)
createPackagingConfiguration_dashPackage = Lens.lens (\CreatePackagingConfiguration' {dashPackage} -> dashPackage) (\s@CreatePackagingConfiguration' {} a -> s {dashPackage = a} :: CreatePackagingConfiguration)

-- | Undocumented member.
createPackagingConfiguration_cmafPackage :: Lens.Lens' CreatePackagingConfiguration (Prelude.Maybe CmafPackage)
createPackagingConfiguration_cmafPackage = Lens.lens (\CreatePackagingConfiguration' {cmafPackage} -> cmafPackage) (\s@CreatePackagingConfiguration' {} a -> s {cmafPackage = a} :: CreatePackagingConfiguration)

-- | Undocumented member.
createPackagingConfiguration_hlsPackage :: Lens.Lens' CreatePackagingConfiguration (Prelude.Maybe HlsPackage)
createPackagingConfiguration_hlsPackage = Lens.lens (\CreatePackagingConfiguration' {hlsPackage} -> hlsPackage) (\s@CreatePackagingConfiguration' {} a -> s {hlsPackage = a} :: CreatePackagingConfiguration)

-- | The ID of the PackagingConfiguration.
createPackagingConfiguration_id :: Lens.Lens' CreatePackagingConfiguration Prelude.Text
createPackagingConfiguration_id = Lens.lens (\CreatePackagingConfiguration' {id} -> id) (\s@CreatePackagingConfiguration' {} a -> s {id = a} :: CreatePackagingConfiguration)

-- | The ID of a PackagingGroup.
createPackagingConfiguration_packagingGroupId :: Lens.Lens' CreatePackagingConfiguration Prelude.Text
createPackagingConfiguration_packagingGroupId = Lens.lens (\CreatePackagingConfiguration' {packagingGroupId} -> packagingGroupId) (\s@CreatePackagingConfiguration' {} a -> s {packagingGroupId = a} :: CreatePackagingConfiguration)

instance Core.AWSRequest CreatePackagingConfiguration where
  type
    AWSResponse CreatePackagingConfiguration =
      CreatePackagingConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePackagingConfigurationResponse'
            Prelude.<$> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "mssPackage")
            Prelude.<*> (x Core..?> "packagingGroupId")
            Prelude.<*> (x Core..?> "arn")
            Prelude.<*> (x Core..?> "id")
            Prelude.<*> (x Core..?> "dashPackage")
            Prelude.<*> (x Core..?> "cmafPackage")
            Prelude.<*> (x Core..?> "hlsPackage")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreatePackagingConfiguration
  where
  hashWithSalt _salt CreatePackagingConfiguration' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` mssPackage
      `Prelude.hashWithSalt` dashPackage
      `Prelude.hashWithSalt` cmafPackage
      `Prelude.hashWithSalt` hlsPackage
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` packagingGroupId

instance Prelude.NFData CreatePackagingConfiguration where
  rnf CreatePackagingConfiguration' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf mssPackage
      `Prelude.seq` Prelude.rnf dashPackage
      `Prelude.seq` Prelude.rnf cmafPackage
      `Prelude.seq` Prelude.rnf hlsPackage
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf packagingGroupId

instance Core.ToHeaders CreatePackagingConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreatePackagingConfiguration where
  toJSON CreatePackagingConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("tags" Core..=) Prelude.<$> tags,
            ("mssPackage" Core..=) Prelude.<$> mssPackage,
            ("dashPackage" Core..=) Prelude.<$> dashPackage,
            ("cmafPackage" Core..=) Prelude.<$> cmafPackage,
            ("hlsPackage" Core..=) Prelude.<$> hlsPackage,
            Prelude.Just ("id" Core..= id),
            Prelude.Just
              ("packagingGroupId" Core..= packagingGroupId)
          ]
      )

instance Core.ToPath CreatePackagingConfiguration where
  toPath = Prelude.const "/packaging_configurations"

instance Core.ToQuery CreatePackagingConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreatePackagingConfigurationResponse' smart constructor.
data CreatePackagingConfigurationResponse = CreatePackagingConfigurationResponse'
  { tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    mssPackage :: Prelude.Maybe MssPackage,
    -- | The ID of a PackagingGroup.
    packagingGroupId :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the PackagingConfiguration.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the PackagingConfiguration.
    id :: Prelude.Maybe Prelude.Text,
    dashPackage :: Prelude.Maybe DashPackage,
    cmafPackage :: Prelude.Maybe CmafPackage,
    hlsPackage :: Prelude.Maybe HlsPackage,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePackagingConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createPackagingConfigurationResponse_tags' - Undocumented member.
--
-- 'mssPackage', 'createPackagingConfigurationResponse_mssPackage' - Undocumented member.
--
-- 'packagingGroupId', 'createPackagingConfigurationResponse_packagingGroupId' - The ID of a PackagingGroup.
--
-- 'arn', 'createPackagingConfigurationResponse_arn' - The ARN of the PackagingConfiguration.
--
-- 'id', 'createPackagingConfigurationResponse_id' - The ID of the PackagingConfiguration.
--
-- 'dashPackage', 'createPackagingConfigurationResponse_dashPackage' - Undocumented member.
--
-- 'cmafPackage', 'createPackagingConfigurationResponse_cmafPackage' - Undocumented member.
--
-- 'hlsPackage', 'createPackagingConfigurationResponse_hlsPackage' - Undocumented member.
--
-- 'httpStatus', 'createPackagingConfigurationResponse_httpStatus' - The response's http status code.
newCreatePackagingConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreatePackagingConfigurationResponse
newCreatePackagingConfigurationResponse pHttpStatus_ =
  CreatePackagingConfigurationResponse'
    { tags =
        Prelude.Nothing,
      mssPackage = Prelude.Nothing,
      packagingGroupId = Prelude.Nothing,
      arn = Prelude.Nothing,
      id = Prelude.Nothing,
      dashPackage = Prelude.Nothing,
      cmafPackage = Prelude.Nothing,
      hlsPackage = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createPackagingConfigurationResponse_tags :: Lens.Lens' CreatePackagingConfigurationResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createPackagingConfigurationResponse_tags = Lens.lens (\CreatePackagingConfigurationResponse' {tags} -> tags) (\s@CreatePackagingConfigurationResponse' {} a -> s {tags = a} :: CreatePackagingConfigurationResponse) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
createPackagingConfigurationResponse_mssPackage :: Lens.Lens' CreatePackagingConfigurationResponse (Prelude.Maybe MssPackage)
createPackagingConfigurationResponse_mssPackage = Lens.lens (\CreatePackagingConfigurationResponse' {mssPackage} -> mssPackage) (\s@CreatePackagingConfigurationResponse' {} a -> s {mssPackage = a} :: CreatePackagingConfigurationResponse)

-- | The ID of a PackagingGroup.
createPackagingConfigurationResponse_packagingGroupId :: Lens.Lens' CreatePackagingConfigurationResponse (Prelude.Maybe Prelude.Text)
createPackagingConfigurationResponse_packagingGroupId = Lens.lens (\CreatePackagingConfigurationResponse' {packagingGroupId} -> packagingGroupId) (\s@CreatePackagingConfigurationResponse' {} a -> s {packagingGroupId = a} :: CreatePackagingConfigurationResponse)

-- | The ARN of the PackagingConfiguration.
createPackagingConfigurationResponse_arn :: Lens.Lens' CreatePackagingConfigurationResponse (Prelude.Maybe Prelude.Text)
createPackagingConfigurationResponse_arn = Lens.lens (\CreatePackagingConfigurationResponse' {arn} -> arn) (\s@CreatePackagingConfigurationResponse' {} a -> s {arn = a} :: CreatePackagingConfigurationResponse)

-- | The ID of the PackagingConfiguration.
createPackagingConfigurationResponse_id :: Lens.Lens' CreatePackagingConfigurationResponse (Prelude.Maybe Prelude.Text)
createPackagingConfigurationResponse_id = Lens.lens (\CreatePackagingConfigurationResponse' {id} -> id) (\s@CreatePackagingConfigurationResponse' {} a -> s {id = a} :: CreatePackagingConfigurationResponse)

-- | Undocumented member.
createPackagingConfigurationResponse_dashPackage :: Lens.Lens' CreatePackagingConfigurationResponse (Prelude.Maybe DashPackage)
createPackagingConfigurationResponse_dashPackage = Lens.lens (\CreatePackagingConfigurationResponse' {dashPackage} -> dashPackage) (\s@CreatePackagingConfigurationResponse' {} a -> s {dashPackage = a} :: CreatePackagingConfigurationResponse)

-- | Undocumented member.
createPackagingConfigurationResponse_cmafPackage :: Lens.Lens' CreatePackagingConfigurationResponse (Prelude.Maybe CmafPackage)
createPackagingConfigurationResponse_cmafPackage = Lens.lens (\CreatePackagingConfigurationResponse' {cmafPackage} -> cmafPackage) (\s@CreatePackagingConfigurationResponse' {} a -> s {cmafPackage = a} :: CreatePackagingConfigurationResponse)

-- | Undocumented member.
createPackagingConfigurationResponse_hlsPackage :: Lens.Lens' CreatePackagingConfigurationResponse (Prelude.Maybe HlsPackage)
createPackagingConfigurationResponse_hlsPackage = Lens.lens (\CreatePackagingConfigurationResponse' {hlsPackage} -> hlsPackage) (\s@CreatePackagingConfigurationResponse' {} a -> s {hlsPackage = a} :: CreatePackagingConfigurationResponse)

-- | The response's http status code.
createPackagingConfigurationResponse_httpStatus :: Lens.Lens' CreatePackagingConfigurationResponse Prelude.Int
createPackagingConfigurationResponse_httpStatus = Lens.lens (\CreatePackagingConfigurationResponse' {httpStatus} -> httpStatus) (\s@CreatePackagingConfigurationResponse' {} a -> s {httpStatus = a} :: CreatePackagingConfigurationResponse)

instance
  Prelude.NFData
    CreatePackagingConfigurationResponse
  where
  rnf CreatePackagingConfigurationResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf mssPackage
      `Prelude.seq` Prelude.rnf packagingGroupId
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf dashPackage
      `Prelude.seq` Prelude.rnf cmafPackage
      `Prelude.seq` Prelude.rnf hlsPackage
      `Prelude.seq` Prelude.rnf httpStatus
