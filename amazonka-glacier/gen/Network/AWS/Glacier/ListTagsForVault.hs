{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.Glacier.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
    accountId :: Prelude.Text,
    -- | The name of the vault.
    vaultName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'vaultName'
  Prelude.Text ->
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
listTagsForVault_accountId :: Lens.Lens' ListTagsForVault Prelude.Text
listTagsForVault_accountId = Lens.lens (\ListTagsForVault' {accountId} -> accountId) (\s@ListTagsForVault' {} a -> s {accountId = a} :: ListTagsForVault)

-- | The name of the vault.
listTagsForVault_vaultName :: Lens.Lens' ListTagsForVault Prelude.Text
listTagsForVault_vaultName = Lens.lens (\ListTagsForVault' {vaultName} -> vaultName) (\s@ListTagsForVault' {} a -> s {vaultName = a} :: ListTagsForVault)

instance Prelude.AWSRequest ListTagsForVault where
  type Rs ListTagsForVault = ListTagsForVaultResponse
  request =
    Request.glacierVersionHeader (Prelude._svcVersion defaultService)
      Prelude.. Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTagsForVaultResponse'
            Prelude.<$> (x Prelude..?> "Tags" Prelude..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTagsForVault

instance Prelude.NFData ListTagsForVault

instance Prelude.ToHeaders ListTagsForVault where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath ListTagsForVault where
  toPath ListTagsForVault' {..} =
    Prelude.mconcat
      [ "/",
        Prelude.toBS accountId,
        "/vaults/",
        Prelude.toBS vaultName,
        "/tags"
      ]

instance Prelude.ToQuery ListTagsForVault where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the Amazon S3 Glacier response to your request.
--
-- /See:/ 'newListTagsForVaultResponse' smart constructor.
data ListTagsForVaultResponse = ListTagsForVaultResponse'
  { -- | The tags attached to the vault. Each tag is composed of a key and a
    -- value.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  ListTagsForVaultResponse
newListTagsForVaultResponse pHttpStatus_ =
  ListTagsForVaultResponse'
    { tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The tags attached to the vault. Each tag is composed of a key and a
-- value.
listTagsForVaultResponse_tags :: Lens.Lens' ListTagsForVaultResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
listTagsForVaultResponse_tags = Lens.lens (\ListTagsForVaultResponse' {tags} -> tags) (\s@ListTagsForVaultResponse' {} a -> s {tags = a} :: ListTagsForVaultResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listTagsForVaultResponse_httpStatus :: Lens.Lens' ListTagsForVaultResponse Prelude.Int
listTagsForVaultResponse_httpStatus = Lens.lens (\ListTagsForVaultResponse' {httpStatus} -> httpStatus) (\s@ListTagsForVaultResponse' {} a -> s {httpStatus = a} :: ListTagsForVaultResponse)

instance Prelude.NFData ListTagsForVaultResponse
