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
-- Module      : Amazonka.Glacier.AddTagsToVault
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.Glacier.AddTagsToVault
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glacier.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The input values for @AddTagsToVault@.
--
-- /See:/ 'newAddTagsToVault' smart constructor.
data AddTagsToVault = AddTagsToVault'
  { -- | The tags to add to the vault. Each tag is composed of a key and a value.
    -- The value can be an empty string.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The @AccountId@ value is the AWS account ID of the account that owns the
    -- vault. You can either specify an AWS account ID or optionally a single
    -- \'@-@\' (hyphen), in which case Amazon S3 Glacier uses the AWS account
    -- ID associated with the credentials used to sign the request. If you use
    -- an account ID, do not include any hyphens (\'-\') in the ID.
    accountId :: Prelude.Text,
    -- | The name of the vault.
    vaultName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'vaultName'
  Prelude.Text ->
  AddTagsToVault
newAddTagsToVault pAccountId_ pVaultName_ =
  AddTagsToVault'
    { tags = Prelude.Nothing,
      accountId = pAccountId_,
      vaultName = pVaultName_
    }

-- | The tags to add to the vault. Each tag is composed of a key and a value.
-- The value can be an empty string.
addTagsToVault_tags :: Lens.Lens' AddTagsToVault (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
addTagsToVault_tags = Lens.lens (\AddTagsToVault' {tags} -> tags) (\s@AddTagsToVault' {} a -> s {tags = a} :: AddTagsToVault) Prelude.. Lens.mapping Lens.coerced

-- | The @AccountId@ value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- \'@-@\' (hyphen), in which case Amazon S3 Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (\'-\') in the ID.
addTagsToVault_accountId :: Lens.Lens' AddTagsToVault Prelude.Text
addTagsToVault_accountId = Lens.lens (\AddTagsToVault' {accountId} -> accountId) (\s@AddTagsToVault' {} a -> s {accountId = a} :: AddTagsToVault)

-- | The name of the vault.
addTagsToVault_vaultName :: Lens.Lens' AddTagsToVault Prelude.Text
addTagsToVault_vaultName = Lens.lens (\AddTagsToVault' {vaultName} -> vaultName) (\s@AddTagsToVault' {} a -> s {vaultName = a} :: AddTagsToVault)

instance Core.AWSRequest AddTagsToVault where
  type
    AWSResponse AddTagsToVault =
      AddTagsToVaultResponse
  request overrides =
    Request.glacierVersionHeader (Core.version defaultService)
      Prelude.. Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull AddTagsToVaultResponse'

instance Prelude.Hashable AddTagsToVault where
  hashWithSalt _salt AddTagsToVault' {..} =
    _salt
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` vaultName

instance Prelude.NFData AddTagsToVault where
  rnf AddTagsToVault' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf vaultName

instance Data.ToHeaders AddTagsToVault where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON AddTagsToVault where
  toJSON AddTagsToVault' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Tags" Data..=) Prelude.<$> tags]
      )

instance Data.ToPath AddTagsToVault where
  toPath AddTagsToVault' {..} =
    Prelude.mconcat
      [ "/",
        Data.toBS accountId,
        "/vaults/",
        Data.toBS vaultName,
        "/tags"
      ]

instance Data.ToQuery AddTagsToVault where
  toQuery =
    Prelude.const (Prelude.mconcat ["operation=add"])

-- | /See:/ 'newAddTagsToVaultResponse' smart constructor.
data AddTagsToVaultResponse = AddTagsToVaultResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddTagsToVaultResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAddTagsToVaultResponse ::
  AddTagsToVaultResponse
newAddTagsToVaultResponse = AddTagsToVaultResponse'

instance Prelude.NFData AddTagsToVaultResponse where
  rnf _ = ()
