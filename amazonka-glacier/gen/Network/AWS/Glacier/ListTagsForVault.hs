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
-- Module      : Network.AWS.Glacier.ListTagsForVault
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation lists all the tags attached to a vault. The operation
-- returns an empty map if there are no tags. For more information about
-- tags, see
-- <https://docs.aws.amazon.com/amazonglacier/latest/dev/tagging.html Tagging Amazon S3 Glacier Resources>.
module Network.AWS.Glacier.ListTagsForVault
  ( -- * Creating a Request
    ListTagsForVault (..),
    newListTagsForVault,

    -- * Request Lenses
    listTagsForVault_accountId,
    listTagsForVault_vaultName,

    -- * Destructuring the Response
    ListTagsForVaultResponse (..),
    newListTagsForVaultResponse,

    -- * Response Lenses
    listTagsForVaultResponse_tags,
    listTagsForVaultResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glacier.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input value for @ListTagsForVaultInput@.
--
-- /See:/ 'newListTagsForVault' smart constructor.
data ListTagsForVault = ListTagsForVault'
  { -- | The @AccountId@ value is the AWS account ID of the account that owns the
    -- vault. You can either specify an AWS account ID or optionally a single
    -- \'@-@\' (hyphen), in which case Amazon S3 Glacier uses the AWS account
    -- ID associated with the credentials used to sign the request. If you use
    -- an account ID, do not include any hyphens (\'-\') in the ID.
    accountId :: Core.Text,
    -- | The name of the vault.
    vaultName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListTagsForVault' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'listTagsForVault_accountId' - The @AccountId@ value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- \'@-@\' (hyphen), in which case Amazon S3 Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (\'-\') in the ID.
--
-- 'vaultName', 'listTagsForVault_vaultName' - The name of the vault.
newListTagsForVault ::
  -- | 'accountId'
  Core.Text ->
  -- | 'vaultName'
  Core.Text ->
  ListTagsForVault
newListTagsForVault pAccountId_ pVaultName_ =
  ListTagsForVault'
    { accountId = pAccountId_,
      vaultName = pVaultName_
    }

-- | The @AccountId@ value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- \'@-@\' (hyphen), in which case Amazon S3 Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (\'-\') in the ID.
listTagsForVault_accountId :: Lens.Lens' ListTagsForVault Core.Text
listTagsForVault_accountId = Lens.lens (\ListTagsForVault' {accountId} -> accountId) (\s@ListTagsForVault' {} a -> s {accountId = a} :: ListTagsForVault)

-- | The name of the vault.
listTagsForVault_vaultName :: Lens.Lens' ListTagsForVault Core.Text
listTagsForVault_vaultName = Lens.lens (\ListTagsForVault' {vaultName} -> vaultName) (\s@ListTagsForVault' {} a -> s {vaultName = a} :: ListTagsForVault)

instance Core.AWSRequest ListTagsForVault where
  type
    AWSResponse ListTagsForVault =
      ListTagsForVaultResponse
  request =
    Request.glacierVersionHeader (Core._serviceVersion defaultService)
      Core.. Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTagsForVaultResponse'
            Core.<$> (x Core..?> "Tags" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListTagsForVault

instance Core.NFData ListTagsForVault

instance Core.ToHeaders ListTagsForVault where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListTagsForVault where
  toPath ListTagsForVault' {..} =
    Core.mconcat
      [ "/",
        Core.toBS accountId,
        "/vaults/",
        Core.toBS vaultName,
        "/tags"
      ]

instance Core.ToQuery ListTagsForVault where
  toQuery = Core.const Core.mempty

-- | Contains the Amazon S3 Glacier response to your request.
--
-- /See:/ 'newListTagsForVaultResponse' smart constructor.
data ListTagsForVaultResponse = ListTagsForVaultResponse'
  { -- | The tags attached to the vault. Each tag is composed of a key and a
    -- value.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListTagsForVaultResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'listTagsForVaultResponse_tags' - The tags attached to the vault. Each tag is composed of a key and a
-- value.
--
-- 'httpStatus', 'listTagsForVaultResponse_httpStatus' - The response's http status code.
newListTagsForVaultResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListTagsForVaultResponse
newListTagsForVaultResponse pHttpStatus_ =
  ListTagsForVaultResponse'
    { tags = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The tags attached to the vault. Each tag is composed of a key and a
-- value.
listTagsForVaultResponse_tags :: Lens.Lens' ListTagsForVaultResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
listTagsForVaultResponse_tags = Lens.lens (\ListTagsForVaultResponse' {tags} -> tags) (\s@ListTagsForVaultResponse' {} a -> s {tags = a} :: ListTagsForVaultResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listTagsForVaultResponse_httpStatus :: Lens.Lens' ListTagsForVaultResponse Core.Int
listTagsForVaultResponse_httpStatus = Lens.lens (\ListTagsForVaultResponse' {httpStatus} -> httpStatus) (\s@ListTagsForVaultResponse' {} a -> s {httpStatus = a} :: ListTagsForVaultResponse)

instance Core.NFData ListTagsForVaultResponse
