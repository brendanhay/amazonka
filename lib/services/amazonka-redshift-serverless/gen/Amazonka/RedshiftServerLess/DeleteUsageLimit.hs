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
-- Module      : Amazonka.RedshiftServerLess.DeleteUsageLimit
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a usage limit from Amazon Redshift Serverless.
module Amazonka.RedshiftServerLess.DeleteUsageLimit
  ( -- * Creating a Request
    DeleteUsageLimit (..),
    newDeleteUsageLimit,

    -- * Request Lenses
    deleteUsageLimit_usageLimitId,

    -- * Destructuring the Response
    DeleteUsageLimitResponse (..),
    newDeleteUsageLimitResponse,

    -- * Response Lenses
    deleteUsageLimitResponse_usageLimit,
    deleteUsageLimitResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RedshiftServerLess.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteUsageLimit' smart constructor.
data DeleteUsageLimit = DeleteUsageLimit'
  { -- | The unique identifier of the usage limit to delete.
    usageLimitId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteUsageLimit' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'usageLimitId', 'deleteUsageLimit_usageLimitId' - The unique identifier of the usage limit to delete.
newDeleteUsageLimit ::
  -- | 'usageLimitId'
  Prelude.Text ->
  DeleteUsageLimit
newDeleteUsageLimit pUsageLimitId_ =
  DeleteUsageLimit' {usageLimitId = pUsageLimitId_}

-- | The unique identifier of the usage limit to delete.
deleteUsageLimit_usageLimitId :: Lens.Lens' DeleteUsageLimit Prelude.Text
deleteUsageLimit_usageLimitId = Lens.lens (\DeleteUsageLimit' {usageLimitId} -> usageLimitId) (\s@DeleteUsageLimit' {} a -> s {usageLimitId = a} :: DeleteUsageLimit)

instance Core.AWSRequest DeleteUsageLimit where
  type
    AWSResponse DeleteUsageLimit =
      DeleteUsageLimitResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteUsageLimitResponse'
            Prelude.<$> (x Core..?> "usageLimit")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteUsageLimit where
  hashWithSalt _salt DeleteUsageLimit' {..} =
    _salt `Prelude.hashWithSalt` usageLimitId

instance Prelude.NFData DeleteUsageLimit where
  rnf DeleteUsageLimit' {..} = Prelude.rnf usageLimitId

instance Core.ToHeaders DeleteUsageLimit where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "RedshiftServerless.DeleteUsageLimit" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteUsageLimit where
  toJSON DeleteUsageLimit' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("usageLimitId" Core..= usageLimitId)]
      )

instance Core.ToPath DeleteUsageLimit where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteUsageLimit where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteUsageLimitResponse' smart constructor.
data DeleteUsageLimitResponse = DeleteUsageLimitResponse'
  { -- | The deleted usage limit object.
    usageLimit :: Prelude.Maybe UsageLimit,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteUsageLimitResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'usageLimit', 'deleteUsageLimitResponse_usageLimit' - The deleted usage limit object.
--
-- 'httpStatus', 'deleteUsageLimitResponse_httpStatus' - The response's http status code.
newDeleteUsageLimitResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteUsageLimitResponse
newDeleteUsageLimitResponse pHttpStatus_ =
  DeleteUsageLimitResponse'
    { usageLimit =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The deleted usage limit object.
deleteUsageLimitResponse_usageLimit :: Lens.Lens' DeleteUsageLimitResponse (Prelude.Maybe UsageLimit)
deleteUsageLimitResponse_usageLimit = Lens.lens (\DeleteUsageLimitResponse' {usageLimit} -> usageLimit) (\s@DeleteUsageLimitResponse' {} a -> s {usageLimit = a} :: DeleteUsageLimitResponse)

-- | The response's http status code.
deleteUsageLimitResponse_httpStatus :: Lens.Lens' DeleteUsageLimitResponse Prelude.Int
deleteUsageLimitResponse_httpStatus = Lens.lens (\DeleteUsageLimitResponse' {httpStatus} -> httpStatus) (\s@DeleteUsageLimitResponse' {} a -> s {httpStatus = a} :: DeleteUsageLimitResponse)

instance Prelude.NFData DeleteUsageLimitResponse where
  rnf DeleteUsageLimitResponse' {..} =
    Prelude.rnf usageLimit
      `Prelude.seq` Prelude.rnf httpStatus
