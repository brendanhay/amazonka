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
-- Module      : Amazonka.MediaPackageVOD.CreatePackagingGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new MediaPackage VOD PackagingGroup resource.
module Amazonka.MediaPackageVOD.CreatePackagingGroup
  ( -- * Creating a Request
    CreatePackagingGroup (..),
    newCreatePackagingGroup,

    -- * Request Lenses
    createPackagingGroup_authorization,
    createPackagingGroup_egressAccessLogs,
    createPackagingGroup_tags,
    createPackagingGroup_id,

    -- * Destructuring the Response
    CreatePackagingGroupResponse (..),
    newCreatePackagingGroupResponse,

    -- * Response Lenses
    createPackagingGroupResponse_arn,
    createPackagingGroupResponse_authorization,
    createPackagingGroupResponse_domainName,
    createPackagingGroupResponse_egressAccessLogs,
    createPackagingGroupResponse_id,
    createPackagingGroupResponse_tags,
    createPackagingGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaPackageVOD.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | A new MediaPackage VOD PackagingGroup resource configuration.
--
-- /See:/ 'newCreatePackagingGroup' smart constructor.
data CreatePackagingGroup = CreatePackagingGroup'
  { authorization :: Prelude.Maybe Authorization,
    egressAccessLogs :: Prelude.Maybe EgressAccessLogs,
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The ID of the PackagingGroup.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePackagingGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authorization', 'createPackagingGroup_authorization' - Undocumented member.
--
-- 'egressAccessLogs', 'createPackagingGroup_egressAccessLogs' - Undocumented member.
--
-- 'tags', 'createPackagingGroup_tags' - Undocumented member.
--
-- 'id', 'createPackagingGroup_id' - The ID of the PackagingGroup.
newCreatePackagingGroup ::
  -- | 'id'
  Prelude.Text ->
  CreatePackagingGroup
newCreatePackagingGroup pId_ =
  CreatePackagingGroup'
    { authorization =
        Prelude.Nothing,
      egressAccessLogs = Prelude.Nothing,
      tags = Prelude.Nothing,
      id = pId_
    }

-- | Undocumented member.
createPackagingGroup_authorization :: Lens.Lens' CreatePackagingGroup (Prelude.Maybe Authorization)
createPackagingGroup_authorization = Lens.lens (\CreatePackagingGroup' {authorization} -> authorization) (\s@CreatePackagingGroup' {} a -> s {authorization = a} :: CreatePackagingGroup)

-- | Undocumented member.
createPackagingGroup_egressAccessLogs :: Lens.Lens' CreatePackagingGroup (Prelude.Maybe EgressAccessLogs)
createPackagingGroup_egressAccessLogs = Lens.lens (\CreatePackagingGroup' {egressAccessLogs} -> egressAccessLogs) (\s@CreatePackagingGroup' {} a -> s {egressAccessLogs = a} :: CreatePackagingGroup)

-- | Undocumented member.
createPackagingGroup_tags :: Lens.Lens' CreatePackagingGroup (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createPackagingGroup_tags = Lens.lens (\CreatePackagingGroup' {tags} -> tags) (\s@CreatePackagingGroup' {} a -> s {tags = a} :: CreatePackagingGroup) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the PackagingGroup.
createPackagingGroup_id :: Lens.Lens' CreatePackagingGroup Prelude.Text
createPackagingGroup_id = Lens.lens (\CreatePackagingGroup' {id} -> id) (\s@CreatePackagingGroup' {} a -> s {id = a} :: CreatePackagingGroup)

instance Core.AWSRequest CreatePackagingGroup where
  type
    AWSResponse CreatePackagingGroup =
      CreatePackagingGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePackagingGroupResponse'
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "authorization")
            Prelude.<*> (x Data..?> "domainName")
            Prelude.<*> (x Data..?> "egressAccessLogs")
            Prelude.<*> (x Data..?> "id")
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreatePackagingGroup where
  hashWithSalt _salt CreatePackagingGroup' {..} =
    _salt
      `Prelude.hashWithSalt` authorization
      `Prelude.hashWithSalt` egressAccessLogs
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` id

instance Prelude.NFData CreatePackagingGroup where
  rnf CreatePackagingGroup' {..} =
    Prelude.rnf authorization
      `Prelude.seq` Prelude.rnf egressAccessLogs
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf id

instance Data.ToHeaders CreatePackagingGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreatePackagingGroup where
  toJSON CreatePackagingGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("authorization" Data..=) Prelude.<$> authorization,
            ("egressAccessLogs" Data..=)
              Prelude.<$> egressAccessLogs,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("id" Data..= id)
          ]
      )

instance Data.ToPath CreatePackagingGroup where
  toPath = Prelude.const "/packaging_groups"

instance Data.ToQuery CreatePackagingGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreatePackagingGroupResponse' smart constructor.
data CreatePackagingGroupResponse = CreatePackagingGroupResponse'
  { -- | The ARN of the PackagingGroup.
    arn :: Prelude.Maybe Prelude.Text,
    authorization :: Prelude.Maybe Authorization,
    -- | The fully qualified domain name for Assets in the PackagingGroup.
    domainName :: Prelude.Maybe Prelude.Text,
    egressAccessLogs :: Prelude.Maybe EgressAccessLogs,
    -- | The ID of the PackagingGroup.
    id :: Prelude.Maybe Prelude.Text,
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePackagingGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'createPackagingGroupResponse_arn' - The ARN of the PackagingGroup.
--
-- 'authorization', 'createPackagingGroupResponse_authorization' - Undocumented member.
--
-- 'domainName', 'createPackagingGroupResponse_domainName' - The fully qualified domain name for Assets in the PackagingGroup.
--
-- 'egressAccessLogs', 'createPackagingGroupResponse_egressAccessLogs' - Undocumented member.
--
-- 'id', 'createPackagingGroupResponse_id' - The ID of the PackagingGroup.
--
-- 'tags', 'createPackagingGroupResponse_tags' - Undocumented member.
--
-- 'httpStatus', 'createPackagingGroupResponse_httpStatus' - The response's http status code.
newCreatePackagingGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreatePackagingGroupResponse
newCreatePackagingGroupResponse pHttpStatus_ =
  CreatePackagingGroupResponse'
    { arn =
        Prelude.Nothing,
      authorization = Prelude.Nothing,
      domainName = Prelude.Nothing,
      egressAccessLogs = Prelude.Nothing,
      id = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the PackagingGroup.
createPackagingGroupResponse_arn :: Lens.Lens' CreatePackagingGroupResponse (Prelude.Maybe Prelude.Text)
createPackagingGroupResponse_arn = Lens.lens (\CreatePackagingGroupResponse' {arn} -> arn) (\s@CreatePackagingGroupResponse' {} a -> s {arn = a} :: CreatePackagingGroupResponse)

-- | Undocumented member.
createPackagingGroupResponse_authorization :: Lens.Lens' CreatePackagingGroupResponse (Prelude.Maybe Authorization)
createPackagingGroupResponse_authorization = Lens.lens (\CreatePackagingGroupResponse' {authorization} -> authorization) (\s@CreatePackagingGroupResponse' {} a -> s {authorization = a} :: CreatePackagingGroupResponse)

-- | The fully qualified domain name for Assets in the PackagingGroup.
createPackagingGroupResponse_domainName :: Lens.Lens' CreatePackagingGroupResponse (Prelude.Maybe Prelude.Text)
createPackagingGroupResponse_domainName = Lens.lens (\CreatePackagingGroupResponse' {domainName} -> domainName) (\s@CreatePackagingGroupResponse' {} a -> s {domainName = a} :: CreatePackagingGroupResponse)

-- | Undocumented member.
createPackagingGroupResponse_egressAccessLogs :: Lens.Lens' CreatePackagingGroupResponse (Prelude.Maybe EgressAccessLogs)
createPackagingGroupResponse_egressAccessLogs = Lens.lens (\CreatePackagingGroupResponse' {egressAccessLogs} -> egressAccessLogs) (\s@CreatePackagingGroupResponse' {} a -> s {egressAccessLogs = a} :: CreatePackagingGroupResponse)

-- | The ID of the PackagingGroup.
createPackagingGroupResponse_id :: Lens.Lens' CreatePackagingGroupResponse (Prelude.Maybe Prelude.Text)
createPackagingGroupResponse_id = Lens.lens (\CreatePackagingGroupResponse' {id} -> id) (\s@CreatePackagingGroupResponse' {} a -> s {id = a} :: CreatePackagingGroupResponse)

-- | Undocumented member.
createPackagingGroupResponse_tags :: Lens.Lens' CreatePackagingGroupResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createPackagingGroupResponse_tags = Lens.lens (\CreatePackagingGroupResponse' {tags} -> tags) (\s@CreatePackagingGroupResponse' {} a -> s {tags = a} :: CreatePackagingGroupResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createPackagingGroupResponse_httpStatus :: Lens.Lens' CreatePackagingGroupResponse Prelude.Int
createPackagingGroupResponse_httpStatus = Lens.lens (\CreatePackagingGroupResponse' {httpStatus} -> httpStatus) (\s@CreatePackagingGroupResponse' {} a -> s {httpStatus = a} :: CreatePackagingGroupResponse)

instance Prelude.NFData CreatePackagingGroupResponse where
  rnf CreatePackagingGroupResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf authorization
      `Prelude.seq` Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf egressAccessLogs
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
