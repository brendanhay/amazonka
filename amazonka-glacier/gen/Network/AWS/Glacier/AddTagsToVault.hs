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
-- Module      : Network.AWS.Glacier.AddTagsToVault
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation adds the specified tags to a vault. Each tag is composed
-- of a key and a value. Each vault can have up to 10 tags. If your request
-- would cause the tag limit for the vault to be exceeded, the operation
-- throws the @LimitExceededException@ error. If a tag already exists on
-- the vault under a specified key, the existing key value will be
-- overwritten. For more information about tags, see
-- <https://docs.aws.amazon.com/amazonglacier/latest/dev/tagging.html Tagging Amazon S3 Glacier Resources>.
module Network.AWS.Glacier.AddTagsToVault
  ( -- * Creating a Request
    AddTagsToVault (..),
    newAddTagsToVault,

    -- * Request Lenses
    addTagsToVault_tags,
    addTagsToVault_accountId,
    addTagsToVault_vaultName,

    -- * Destructuring the Response
    AddTagsToVaultResponse (..),
    newAddTagsToVaultResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glacier.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input values for @AddTagsToVault@.
--
-- /See:/ 'newAddTagsToVault' smart constructor.
data AddTagsToVault = AddTagsToVault'
  { -- | The tags to add to the vault. Each tag is composed of a key and a value.
    -- The value can be an empty string.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The @AccountId@ value is the AWS account ID of the account that owns the
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
-- Create a value of 'AddTagsToVault' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'addTagsToVault_tags' - The tags to add to the vault. Each tag is composed of a key and a value.
-- The value can be an empty string.
--
-- 'accountId', 'addTagsToVault_accountId' - The @AccountId@ value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- \'@-@\' (hyphen), in which case Amazon S3 Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (\'-\') in the ID.
--
-- 'vaultName', 'addTagsToVault_vaultName' - The name of the vault.
newAddTagsToVault ::
  -- | 'accountId'
  Core.Text ->
  -- | 'vaultName'
  Core.Text ->
  AddTagsToVault
newAddTagsToVault pAccountId_ pVaultName_ =
  AddTagsToVault'
    { tags = Core.Nothing,
      accountId = pAccountId_,
      vaultName = pVaultName_
    }

-- | The tags to add to the vault. Each tag is composed of a key and a value.
-- The value can be an empty string.
addTagsToVault_tags :: Lens.Lens' AddTagsToVault (Core.Maybe (Core.HashMap Core.Text Core.Text))
addTagsToVault_tags = Lens.lens (\AddTagsToVault' {tags} -> tags) (\s@AddTagsToVault' {} a -> s {tags = a} :: AddTagsToVault) Core.. Lens.mapping Lens._Coerce

-- | The @AccountId@ value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- \'@-@\' (hyphen), in which case Amazon S3 Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (\'-\') in the ID.
addTagsToVault_accountId :: Lens.Lens' AddTagsToVault Core.Text
addTagsToVault_accountId = Lens.lens (\AddTagsToVault' {accountId} -> accountId) (\s@AddTagsToVault' {} a -> s {accountId = a} :: AddTagsToVault)

-- | The name of the vault.
addTagsToVault_vaultName :: Lens.Lens' AddTagsToVault Core.Text
addTagsToVault_vaultName = Lens.lens (\AddTagsToVault' {vaultName} -> vaultName) (\s@AddTagsToVault' {} a -> s {vaultName = a} :: AddTagsToVault)

instance Core.AWSRequest AddTagsToVault where
  type
    AWSResponse AddTagsToVault =
      AddTagsToVaultResponse
  request =
    Request.glacierVersionHeader (Core._serviceVersion defaultService)
      Core.. Request.postJSON defaultService
  response =
    Response.receiveNull AddTagsToVaultResponse'

instance Core.Hashable AddTagsToVault

instance Core.NFData AddTagsToVault

instance Core.ToHeaders AddTagsToVault where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON AddTagsToVault where
  toJSON AddTagsToVault' {..} =
    Core.object
      (Core.catMaybes [("Tags" Core..=) Core.<$> tags])

instance Core.ToPath AddTagsToVault where
  toPath AddTagsToVault' {..} =
    Core.mconcat
      [ "/",
        Core.toBS accountId,
        "/vaults/",
        Core.toBS vaultName,
        "/tags"
      ]

instance Core.ToQuery AddTagsToVault where
  toQuery = Core.const (Core.mconcat ["operation=add"])

-- | /See:/ 'newAddTagsToVaultResponse' smart constructor.
data AddTagsToVaultResponse = AddTagsToVaultResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AddTagsToVaultResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAddTagsToVaultResponse ::
  AddTagsToVaultResponse
newAddTagsToVaultResponse = AddTagsToVaultResponse'

instance Core.NFData AddTagsToVaultResponse
