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
-- Module      : Amazonka.Glacier.ListVaults
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation lists all vaults owned by the calling user\'s account.
-- The list returned in the response is ASCII-sorted by vault name.
--
-- By default, this operation returns up to 10 items. If there are more
-- vaults to list, the response @marker@ field contains the vault Amazon
-- Resource Name (ARN) at which to continue the list with a new List Vaults
-- request; otherwise, the @marker@ field is @null@. To return a list of
-- vaults that begins at a specific vault, set the @marker@ request
-- parameter to the vault ARN you obtained from a previous List Vaults
-- request. You can also limit the number of vaults returned in the
-- response by specifying the @limit@ parameter in the request.
--
-- An AWS account has full permission to perform all operations (actions).
-- However, AWS Identity and Access Management (IAM) users don\'t have any
-- permissions by default. You must grant them explicit permission to
-- perform specific actions. For more information, see
-- <https://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identity and Access Management (IAM)>.
--
-- For conceptual information and underlying REST API, see
-- <https://docs.aws.amazon.com/amazonglacier/latest/dev/retrieving-vault-info.html Retrieving Vault Metadata in Amazon S3 Glacier>
-- and
-- <https://docs.aws.amazon.com/amazonglacier/latest/dev/api-vaults-get.html List Vaults>
-- in the /Amazon Glacier Developer Guide/.
--
-- This operation returns paginated results.
module Amazonka.Glacier.ListVaults
  ( -- * Creating a Request
    ListVaults (..),
    newListVaults,

    -- * Request Lenses
    listVaults_marker,
    listVaults_limit,
    listVaults_accountId,

    -- * Destructuring the Response
    ListVaultsResponse (..),
    newListVaultsResponse,

    -- * Response Lenses
    listVaultsResponse_marker,
    listVaultsResponse_vaultList,
    listVaultsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Glacier.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Provides options to retrieve the vault list owned by the calling user\'s
-- account. The list provides metadata information for each vault.
--
-- /See:/ 'newListVaults' smart constructor.
data ListVaults = ListVaults'
  { -- | A string used for pagination. The marker specifies the vault ARN after
    -- which the listing of vaults should begin.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of vaults to be returned. The default limit is 10.
    -- The number of vaults returned might be fewer than the specified limit,
    -- but the number of returned vaults never exceeds the limit.
    limit :: Prelude.Maybe Prelude.Text,
    -- | The @AccountId@ value is the AWS account ID. This value must match the
    -- AWS account ID associated with the credentials used to sign the request.
    -- You can either specify an AWS account ID or optionally a single \'@-@\'
    -- (hyphen), in which case Amazon Glacier uses the AWS account ID
    -- associated with the credentials used to sign the request. If you specify
    -- your account ID, do not include any hyphens (\'-\') in the ID.
    accountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListVaults' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listVaults_marker' - A string used for pagination. The marker specifies the vault ARN after
-- which the listing of vaults should begin.
--
-- 'limit', 'listVaults_limit' - The maximum number of vaults to be returned. The default limit is 10.
-- The number of vaults returned might be fewer than the specified limit,
-- but the number of returned vaults never exceeds the limit.
--
-- 'accountId', 'listVaults_accountId' - The @AccountId@ value is the AWS account ID. This value must match the
-- AWS account ID associated with the credentials used to sign the request.
-- You can either specify an AWS account ID or optionally a single \'@-@\'
-- (hyphen), in which case Amazon Glacier uses the AWS account ID
-- associated with the credentials used to sign the request. If you specify
-- your account ID, do not include any hyphens (\'-\') in the ID.
newListVaults ::
  -- | 'accountId'
  Prelude.Text ->
  ListVaults
newListVaults pAccountId_ =
  ListVaults'
    { marker = Prelude.Nothing,
      limit = Prelude.Nothing,
      accountId = pAccountId_
    }

-- | A string used for pagination. The marker specifies the vault ARN after
-- which the listing of vaults should begin.
listVaults_marker :: Lens.Lens' ListVaults (Prelude.Maybe Prelude.Text)
listVaults_marker = Lens.lens (\ListVaults' {marker} -> marker) (\s@ListVaults' {} a -> s {marker = a} :: ListVaults)

-- | The maximum number of vaults to be returned. The default limit is 10.
-- The number of vaults returned might be fewer than the specified limit,
-- but the number of returned vaults never exceeds the limit.
listVaults_limit :: Lens.Lens' ListVaults (Prelude.Maybe Prelude.Text)
listVaults_limit = Lens.lens (\ListVaults' {limit} -> limit) (\s@ListVaults' {} a -> s {limit = a} :: ListVaults)

-- | The @AccountId@ value is the AWS account ID. This value must match the
-- AWS account ID associated with the credentials used to sign the request.
-- You can either specify an AWS account ID or optionally a single \'@-@\'
-- (hyphen), in which case Amazon Glacier uses the AWS account ID
-- associated with the credentials used to sign the request. If you specify
-- your account ID, do not include any hyphens (\'-\') in the ID.
listVaults_accountId :: Lens.Lens' ListVaults Prelude.Text
listVaults_accountId = Lens.lens (\ListVaults' {accountId} -> accountId) (\s@ListVaults' {} a -> s {accountId = a} :: ListVaults)

instance Core.AWSPager ListVaults where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listVaultsResponse_marker Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listVaultsResponse_vaultList Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listVaults_marker
          Lens..~ rs
          Lens.^? listVaultsResponse_marker Prelude.. Lens._Just

instance Core.AWSRequest ListVaults where
  type AWSResponse ListVaults = ListVaultsResponse
  request overrides =
    Request.glacierVersionHeader (Core.version defaultService)
      Prelude.. Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListVaultsResponse'
            Prelude.<$> (x Core..?> "Marker")
            Prelude.<*> (x Core..?> "VaultList" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListVaults where
  hashWithSalt _salt ListVaults' {..} =
    _salt `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` accountId

instance Prelude.NFData ListVaults where
  rnf ListVaults' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf accountId

instance Core.ToHeaders ListVaults where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListVaults where
  toPath ListVaults' {..} =
    Prelude.mconcat
      ["/", Core.toBS accountId, "/vaults"]

instance Core.ToQuery ListVaults where
  toQuery ListVaults' {..} =
    Prelude.mconcat
      ["marker" Core.=: marker, "limit" Core.=: limit]

-- | Contains the Amazon S3 Glacier response to your request.
--
-- /See:/ 'newListVaultsResponse' smart constructor.
data ListVaultsResponse = ListVaultsResponse'
  { -- | The vault ARN at which to continue pagination of the results. You use
    -- the marker in another List Vaults request to obtain more vaults in the
    -- list.
    marker :: Prelude.Maybe Prelude.Text,
    -- | List of vaults.
    vaultList :: Prelude.Maybe [DescribeVaultOutput],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListVaultsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listVaultsResponse_marker' - The vault ARN at which to continue pagination of the results. You use
-- the marker in another List Vaults request to obtain more vaults in the
-- list.
--
-- 'vaultList', 'listVaultsResponse_vaultList' - List of vaults.
--
-- 'httpStatus', 'listVaultsResponse_httpStatus' - The response's http status code.
newListVaultsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListVaultsResponse
newListVaultsResponse pHttpStatus_ =
  ListVaultsResponse'
    { marker = Prelude.Nothing,
      vaultList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The vault ARN at which to continue pagination of the results. You use
-- the marker in another List Vaults request to obtain more vaults in the
-- list.
listVaultsResponse_marker :: Lens.Lens' ListVaultsResponse (Prelude.Maybe Prelude.Text)
listVaultsResponse_marker = Lens.lens (\ListVaultsResponse' {marker} -> marker) (\s@ListVaultsResponse' {} a -> s {marker = a} :: ListVaultsResponse)

-- | List of vaults.
listVaultsResponse_vaultList :: Lens.Lens' ListVaultsResponse (Prelude.Maybe [DescribeVaultOutput])
listVaultsResponse_vaultList = Lens.lens (\ListVaultsResponse' {vaultList} -> vaultList) (\s@ListVaultsResponse' {} a -> s {vaultList = a} :: ListVaultsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listVaultsResponse_httpStatus :: Lens.Lens' ListVaultsResponse Prelude.Int
listVaultsResponse_httpStatus = Lens.lens (\ListVaultsResponse' {httpStatus} -> httpStatus) (\s@ListVaultsResponse' {} a -> s {httpStatus = a} :: ListVaultsResponse)

instance Prelude.NFData ListVaultsResponse where
  rnf ListVaultsResponse' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf vaultList
      `Prelude.seq` Prelude.rnf httpStatus
