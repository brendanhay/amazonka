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
-- Module      : Amazonka.MarketplaceCatalog.DeleteResourcePolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a resource-based policy on an Entity that is identified by its
-- resource ARN.
module Amazonka.MarketplaceCatalog.DeleteResourcePolicy
  ( -- * Creating a Request
    DeleteResourcePolicy (..),
    newDeleteResourcePolicy,

    -- * Request Lenses
    deleteResourcePolicy_resourceArn,

    -- * Destructuring the Response
    DeleteResourcePolicyResponse (..),
    newDeleteResourcePolicyResponse,

    -- * Response Lenses
    deleteResourcePolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MarketplaceCatalog.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteResourcePolicy' smart constructor.
data DeleteResourcePolicy = DeleteResourcePolicy'
  { -- | The Amazon Resource Name (ARN) of the Entity resource that is associated
    -- with the resource policy.
    resourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteResourcePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArn', 'deleteResourcePolicy_resourceArn' - The Amazon Resource Name (ARN) of the Entity resource that is associated
-- with the resource policy.
newDeleteResourcePolicy ::
  -- | 'resourceArn'
  Prelude.Text ->
  DeleteResourcePolicy
newDeleteResourcePolicy pResourceArn_ =
  DeleteResourcePolicy' {resourceArn = pResourceArn_}

-- | The Amazon Resource Name (ARN) of the Entity resource that is associated
-- with the resource policy.
deleteResourcePolicy_resourceArn :: Lens.Lens' DeleteResourcePolicy Prelude.Text
deleteResourcePolicy_resourceArn = Lens.lens (\DeleteResourcePolicy' {resourceArn} -> resourceArn) (\s@DeleteResourcePolicy' {} a -> s {resourceArn = a} :: DeleteResourcePolicy)

instance Core.AWSRequest DeleteResourcePolicy where
  type
    AWSResponse DeleteResourcePolicy =
      DeleteResourcePolicyResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteResourcePolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteResourcePolicy where
  hashWithSalt _salt DeleteResourcePolicy' {..} =
    _salt `Prelude.hashWithSalt` resourceArn

instance Prelude.NFData DeleteResourcePolicy where
  rnf DeleteResourcePolicy' {..} =
    Prelude.rnf resourceArn

instance Data.ToHeaders DeleteResourcePolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteResourcePolicy where
  toPath = Prelude.const "/DeleteResourcePolicy"

instance Data.ToQuery DeleteResourcePolicy where
  toQuery DeleteResourcePolicy' {..} =
    Prelude.mconcat ["resourceArn" Data.=: resourceArn]

-- | /See:/ 'newDeleteResourcePolicyResponse' smart constructor.
data DeleteResourcePolicyResponse = DeleteResourcePolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteResourcePolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteResourcePolicyResponse_httpStatus' - The response's http status code.
newDeleteResourcePolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteResourcePolicyResponse
newDeleteResourcePolicyResponse pHttpStatus_ =
  DeleteResourcePolicyResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteResourcePolicyResponse_httpStatus :: Lens.Lens' DeleteResourcePolicyResponse Prelude.Int
deleteResourcePolicyResponse_httpStatus = Lens.lens (\DeleteResourcePolicyResponse' {httpStatus} -> httpStatus) (\s@DeleteResourcePolicyResponse' {} a -> s {httpStatus = a} :: DeleteResourcePolicyResponse)

instance Prelude.NFData DeleteResourcePolicyResponse where
  rnf DeleteResourcePolicyResponse' {..} =
    Prelude.rnf httpStatus
