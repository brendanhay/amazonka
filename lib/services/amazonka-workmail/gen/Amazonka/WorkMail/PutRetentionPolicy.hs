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
-- Module      : Amazonka.WorkMail.PutRetentionPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Puts a retention policy to the specified organization.
module Amazonka.WorkMail.PutRetentionPolicy
  ( -- * Creating a Request
    PutRetentionPolicy (..),
    newPutRetentionPolicy,

    -- * Request Lenses
    putRetentionPolicy_description,
    putRetentionPolicy_id,
    putRetentionPolicy_organizationId,
    putRetentionPolicy_name,
    putRetentionPolicy_folderConfigurations,

    -- * Destructuring the Response
    PutRetentionPolicyResponse (..),
    newPutRetentionPolicyResponse,

    -- * Response Lenses
    putRetentionPolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkMail.Types

-- | /See:/ 'newPutRetentionPolicy' smart constructor.
data PutRetentionPolicy = PutRetentionPolicy'
  { -- | The retention policy description.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The retention policy ID.
    id :: Prelude.Maybe Prelude.Text,
    -- | The organization ID.
    organizationId :: Prelude.Text,
    -- | The retention policy name.
    name :: Prelude.Text,
    -- | The retention policy folder configurations.
    folderConfigurations :: [FolderConfiguration]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutRetentionPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'putRetentionPolicy_description' - The retention policy description.
--
-- 'id', 'putRetentionPolicy_id' - The retention policy ID.
--
-- 'organizationId', 'putRetentionPolicy_organizationId' - The organization ID.
--
-- 'name', 'putRetentionPolicy_name' - The retention policy name.
--
-- 'folderConfigurations', 'putRetentionPolicy_folderConfigurations' - The retention policy folder configurations.
newPutRetentionPolicy ::
  -- | 'organizationId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  PutRetentionPolicy
newPutRetentionPolicy pOrganizationId_ pName_ =
  PutRetentionPolicy'
    { description = Prelude.Nothing,
      id = Prelude.Nothing,
      organizationId = pOrganizationId_,
      name = pName_,
      folderConfigurations = Prelude.mempty
    }

-- | The retention policy description.
putRetentionPolicy_description :: Lens.Lens' PutRetentionPolicy (Prelude.Maybe Prelude.Text)
putRetentionPolicy_description = Lens.lens (\PutRetentionPolicy' {description} -> description) (\s@PutRetentionPolicy' {} a -> s {description = a} :: PutRetentionPolicy) Prelude.. Lens.mapping Data._Sensitive

-- | The retention policy ID.
putRetentionPolicy_id :: Lens.Lens' PutRetentionPolicy (Prelude.Maybe Prelude.Text)
putRetentionPolicy_id = Lens.lens (\PutRetentionPolicy' {id} -> id) (\s@PutRetentionPolicy' {} a -> s {id = a} :: PutRetentionPolicy)

-- | The organization ID.
putRetentionPolicy_organizationId :: Lens.Lens' PutRetentionPolicy Prelude.Text
putRetentionPolicy_organizationId = Lens.lens (\PutRetentionPolicy' {organizationId} -> organizationId) (\s@PutRetentionPolicy' {} a -> s {organizationId = a} :: PutRetentionPolicy)

-- | The retention policy name.
putRetentionPolicy_name :: Lens.Lens' PutRetentionPolicy Prelude.Text
putRetentionPolicy_name = Lens.lens (\PutRetentionPolicy' {name} -> name) (\s@PutRetentionPolicy' {} a -> s {name = a} :: PutRetentionPolicy)

-- | The retention policy folder configurations.
putRetentionPolicy_folderConfigurations :: Lens.Lens' PutRetentionPolicy [FolderConfiguration]
putRetentionPolicy_folderConfigurations = Lens.lens (\PutRetentionPolicy' {folderConfigurations} -> folderConfigurations) (\s@PutRetentionPolicy' {} a -> s {folderConfigurations = a} :: PutRetentionPolicy) Prelude.. Lens.coerced

instance Core.AWSRequest PutRetentionPolicy where
  type
    AWSResponse PutRetentionPolicy =
      PutRetentionPolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutRetentionPolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutRetentionPolicy where
  hashWithSalt _salt PutRetentionPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` organizationId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` folderConfigurations

instance Prelude.NFData PutRetentionPolicy where
  rnf PutRetentionPolicy' {..} =
    Prelude.rnf description `Prelude.seq`
      Prelude.rnf id `Prelude.seq`
        Prelude.rnf organizationId `Prelude.seq`
          Prelude.rnf name `Prelude.seq`
            Prelude.rnf folderConfigurations

instance Data.ToHeaders PutRetentionPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkMailService.PutRetentionPolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutRetentionPolicy where
  toJSON PutRetentionPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("Id" Data..=) Prelude.<$> id,
            Prelude.Just
              ("OrganizationId" Data..= organizationId),
            Prelude.Just ("Name" Data..= name),
            Prelude.Just
              ( "FolderConfigurations"
                  Data..= folderConfigurations
              )
          ]
      )

instance Data.ToPath PutRetentionPolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery PutRetentionPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutRetentionPolicyResponse' smart constructor.
data PutRetentionPolicyResponse = PutRetentionPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutRetentionPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putRetentionPolicyResponse_httpStatus' - The response's http status code.
newPutRetentionPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutRetentionPolicyResponse
newPutRetentionPolicyResponse pHttpStatus_ =
  PutRetentionPolicyResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
putRetentionPolicyResponse_httpStatus :: Lens.Lens' PutRetentionPolicyResponse Prelude.Int
putRetentionPolicyResponse_httpStatus = Lens.lens (\PutRetentionPolicyResponse' {httpStatus} -> httpStatus) (\s@PutRetentionPolicyResponse' {} a -> s {httpStatus = a} :: PutRetentionPolicyResponse)

instance Prelude.NFData PutRetentionPolicyResponse where
  rnf PutRetentionPolicyResponse' {..} =
    Prelude.rnf httpStatus
