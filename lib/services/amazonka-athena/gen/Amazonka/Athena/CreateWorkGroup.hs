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
-- Module      : Amazonka.Athena.CreateWorkGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a workgroup with the specified name.
module Amazonka.Athena.CreateWorkGroup
  ( -- * Creating a Request
    CreateWorkGroup (..),
    newCreateWorkGroup,

    -- * Request Lenses
    createWorkGroup_tags,
    createWorkGroup_configuration,
    createWorkGroup_description,
    createWorkGroup_name,

    -- * Destructuring the Response
    CreateWorkGroupResponse (..),
    newCreateWorkGroupResponse,

    -- * Response Lenses
    createWorkGroupResponse_httpStatus,
  )
where

import Amazonka.Athena.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateWorkGroup' smart constructor.
data CreateWorkGroup = CreateWorkGroup'
  { -- | A list of comma separated tags to add to the workgroup that is created.
    tags :: Prelude.Maybe [Tag],
    -- | The configuration for the workgroup, which includes the location in
    -- Amazon S3 where query results are stored, the encryption configuration,
    -- if any, used for encrypting query results, whether the Amazon CloudWatch
    -- Metrics are enabled for the workgroup, the limit for the amount of bytes
    -- scanned (cutoff) per query, if it is specified, and whether workgroup\'s
    -- settings (specified with @EnforceWorkGroupConfiguration@) in the
    -- @WorkGroupConfiguration@ override client-side settings. See
    -- WorkGroupConfiguration$EnforceWorkGroupConfiguration.
    configuration :: Prelude.Maybe WorkGroupConfiguration,
    -- | The workgroup description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The workgroup name.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateWorkGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createWorkGroup_tags' - A list of comma separated tags to add to the workgroup that is created.
--
-- 'configuration', 'createWorkGroup_configuration' - The configuration for the workgroup, which includes the location in
-- Amazon S3 where query results are stored, the encryption configuration,
-- if any, used for encrypting query results, whether the Amazon CloudWatch
-- Metrics are enabled for the workgroup, the limit for the amount of bytes
-- scanned (cutoff) per query, if it is specified, and whether workgroup\'s
-- settings (specified with @EnforceWorkGroupConfiguration@) in the
-- @WorkGroupConfiguration@ override client-side settings. See
-- WorkGroupConfiguration$EnforceWorkGroupConfiguration.
--
-- 'description', 'createWorkGroup_description' - The workgroup description.
--
-- 'name', 'createWorkGroup_name' - The workgroup name.
newCreateWorkGroup ::
  -- | 'name'
  Prelude.Text ->
  CreateWorkGroup
newCreateWorkGroup pName_ =
  CreateWorkGroup'
    { tags = Prelude.Nothing,
      configuration = Prelude.Nothing,
      description = Prelude.Nothing,
      name = pName_
    }

-- | A list of comma separated tags to add to the workgroup that is created.
createWorkGroup_tags :: Lens.Lens' CreateWorkGroup (Prelude.Maybe [Tag])
createWorkGroup_tags = Lens.lens (\CreateWorkGroup' {tags} -> tags) (\s@CreateWorkGroup' {} a -> s {tags = a} :: CreateWorkGroup) Prelude.. Lens.mapping Lens.coerced

-- | The configuration for the workgroup, which includes the location in
-- Amazon S3 where query results are stored, the encryption configuration,
-- if any, used for encrypting query results, whether the Amazon CloudWatch
-- Metrics are enabled for the workgroup, the limit for the amount of bytes
-- scanned (cutoff) per query, if it is specified, and whether workgroup\'s
-- settings (specified with @EnforceWorkGroupConfiguration@) in the
-- @WorkGroupConfiguration@ override client-side settings. See
-- WorkGroupConfiguration$EnforceWorkGroupConfiguration.
createWorkGroup_configuration :: Lens.Lens' CreateWorkGroup (Prelude.Maybe WorkGroupConfiguration)
createWorkGroup_configuration = Lens.lens (\CreateWorkGroup' {configuration} -> configuration) (\s@CreateWorkGroup' {} a -> s {configuration = a} :: CreateWorkGroup)

-- | The workgroup description.
createWorkGroup_description :: Lens.Lens' CreateWorkGroup (Prelude.Maybe Prelude.Text)
createWorkGroup_description = Lens.lens (\CreateWorkGroup' {description} -> description) (\s@CreateWorkGroup' {} a -> s {description = a} :: CreateWorkGroup)

-- | The workgroup name.
createWorkGroup_name :: Lens.Lens' CreateWorkGroup Prelude.Text
createWorkGroup_name = Lens.lens (\CreateWorkGroup' {name} -> name) (\s@CreateWorkGroup' {} a -> s {name = a} :: CreateWorkGroup)

instance Core.AWSRequest CreateWorkGroup where
  type
    AWSResponse CreateWorkGroup =
      CreateWorkGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateWorkGroupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateWorkGroup where
  hashWithSalt _salt CreateWorkGroup' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` configuration
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateWorkGroup where
  rnf CreateWorkGroup' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf configuration
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf name

instance Core.ToHeaders CreateWorkGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonAthena.CreateWorkGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateWorkGroup where
  toJSON CreateWorkGroup' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Tags" Core..=) Prelude.<$> tags,
            ("Configuration" Core..=) Prelude.<$> configuration,
            ("Description" Core..=) Prelude.<$> description,
            Prelude.Just ("Name" Core..= name)
          ]
      )

instance Core.ToPath CreateWorkGroup where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateWorkGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateWorkGroupResponse' smart constructor.
data CreateWorkGroupResponse = CreateWorkGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateWorkGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createWorkGroupResponse_httpStatus' - The response's http status code.
newCreateWorkGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateWorkGroupResponse
newCreateWorkGroupResponse pHttpStatus_ =
  CreateWorkGroupResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
createWorkGroupResponse_httpStatus :: Lens.Lens' CreateWorkGroupResponse Prelude.Int
createWorkGroupResponse_httpStatus = Lens.lens (\CreateWorkGroupResponse' {httpStatus} -> httpStatus) (\s@CreateWorkGroupResponse' {} a -> s {httpStatus = a} :: CreateWorkGroupResponse)

instance Prelude.NFData CreateWorkGroupResponse where
  rnf CreateWorkGroupResponse' {..} =
    Prelude.rnf httpStatus
