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
-- Module      : Network.AWS.Glacier.RemoveTagsFromVault
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation removes one or more tags from the set of tags attached to
-- a vault. For more information about tags, see
-- <https://docs.aws.amazon.com/amazonglacier/latest/dev/tagging.html Tagging Amazon S3 Glacier Resources>.
-- This operation is idempotent. The operation will be successful, even if
-- there are no tags attached to the vault.
module Network.AWS.Glacier.RemoveTagsFromVault
  ( -- * Creating a Request
    RemoveTagsFromVault (..),
    newRemoveTagsFromVault,

    -- * Request Lenses
    removeTagsFromVault_tagKeys,
    removeTagsFromVault_accountId,
    removeTagsFromVault_vaultName,

    -- * Destructuring the Response
    RemoveTagsFromVaultResponse (..),
    newRemoveTagsFromVaultResponse,
  )
where

import Network.AWS.Glacier.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input value for @RemoveTagsFromVaultInput@.
--
-- /See:/ 'newRemoveTagsFromVault' smart constructor.
data RemoveTagsFromVault = RemoveTagsFromVault'
  { -- | A list of tag keys. Each corresponding tag is removed from the vault.
    tagKeys :: Prelude.Maybe [Prelude.Text],
    -- | The @AccountId@ value is the AWS account ID of the account that owns the
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
-- Create a value of 'RemoveTagsFromVault' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagKeys', 'removeTagsFromVault_tagKeys' - A list of tag keys. Each corresponding tag is removed from the vault.
--
-- 'accountId', 'removeTagsFromVault_accountId' - The @AccountId@ value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- \'@-@\' (hyphen), in which case Amazon S3 Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (\'-\') in the ID.
--
-- 'vaultName', 'removeTagsFromVault_vaultName' - The name of the vault.
newRemoveTagsFromVault ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'vaultName'
  Prelude.Text ->
  RemoveTagsFromVault
newRemoveTagsFromVault pAccountId_ pVaultName_ =
  RemoveTagsFromVault'
    { tagKeys = Prelude.Nothing,
      accountId = pAccountId_,
      vaultName = pVaultName_
    }

-- | A list of tag keys. Each corresponding tag is removed from the vault.
removeTagsFromVault_tagKeys :: Lens.Lens' RemoveTagsFromVault (Prelude.Maybe [Prelude.Text])
removeTagsFromVault_tagKeys = Lens.lens (\RemoveTagsFromVault' {tagKeys} -> tagKeys) (\s@RemoveTagsFromVault' {} a -> s {tagKeys = a} :: RemoveTagsFromVault) Prelude.. Lens.mapping Prelude._Coerce

-- | The @AccountId@ value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- \'@-@\' (hyphen), in which case Amazon S3 Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (\'-\') in the ID.
removeTagsFromVault_accountId :: Lens.Lens' RemoveTagsFromVault Prelude.Text
removeTagsFromVault_accountId = Lens.lens (\RemoveTagsFromVault' {accountId} -> accountId) (\s@RemoveTagsFromVault' {} a -> s {accountId = a} :: RemoveTagsFromVault)

-- | The name of the vault.
removeTagsFromVault_vaultName :: Lens.Lens' RemoveTagsFromVault Prelude.Text
removeTagsFromVault_vaultName = Lens.lens (\RemoveTagsFromVault' {vaultName} -> vaultName) (\s@RemoveTagsFromVault' {} a -> s {vaultName = a} :: RemoveTagsFromVault)

instance Prelude.AWSRequest RemoveTagsFromVault where
  type
    Rs RemoveTagsFromVault =
      RemoveTagsFromVaultResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull RemoveTagsFromVaultResponse'

instance Prelude.Hashable RemoveTagsFromVault

instance Prelude.NFData RemoveTagsFromVault

instance Prelude.ToHeaders RemoveTagsFromVault where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToJSON RemoveTagsFromVault where
  toJSON RemoveTagsFromVault' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("TagKeys" Prelude..=) Prelude.<$> tagKeys]
      )

instance Prelude.ToPath RemoveTagsFromVault where
  toPath RemoveTagsFromVault' {..} =
    Prelude.mconcat
      [ "/",
        Prelude.toBS accountId,
        "/vaults/",
        Prelude.toBS vaultName,
        "/tags"
      ]

instance Prelude.ToQuery RemoveTagsFromVault where
  toQuery =
    Prelude.const
      (Prelude.mconcat ["operation=remove"])

-- | /See:/ 'newRemoveTagsFromVaultResponse' smart constructor.
data RemoveTagsFromVaultResponse = RemoveTagsFromVaultResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RemoveTagsFromVaultResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newRemoveTagsFromVaultResponse ::
  RemoveTagsFromVaultResponse
newRemoveTagsFromVaultResponse =
  RemoveTagsFromVaultResponse'

instance Prelude.NFData RemoveTagsFromVaultResponse
