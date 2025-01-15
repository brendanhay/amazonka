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
-- Module      : Amazonka.Inspector2.DeleteFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a filter resource.
module Amazonka.Inspector2.DeleteFilter
  ( -- * Creating a Request
    DeleteFilter (..),
    newDeleteFilter,

    -- * Request Lenses
    deleteFilter_arn,

    -- * Destructuring the Response
    DeleteFilterResponse (..),
    newDeleteFilterResponse,

    -- * Response Lenses
    deleteFilterResponse_httpStatus,
    deleteFilterResponse_arn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteFilter' smart constructor.
data DeleteFilter = DeleteFilter'
  { -- | The Amazon Resource Number (ARN) of the filter to be deleted.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'deleteFilter_arn' - The Amazon Resource Number (ARN) of the filter to be deleted.
newDeleteFilter ::
  -- | 'arn'
  Prelude.Text ->
  DeleteFilter
newDeleteFilter pArn_ = DeleteFilter' {arn = pArn_}

-- | The Amazon Resource Number (ARN) of the filter to be deleted.
deleteFilter_arn :: Lens.Lens' DeleteFilter Prelude.Text
deleteFilter_arn = Lens.lens (\DeleteFilter' {arn} -> arn) (\s@DeleteFilter' {} a -> s {arn = a} :: DeleteFilter)

instance Core.AWSRequest DeleteFilter where
  type AWSResponse DeleteFilter = DeleteFilterResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteFilterResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "arn")
      )

instance Prelude.Hashable DeleteFilter where
  hashWithSalt _salt DeleteFilter' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData DeleteFilter where
  rnf DeleteFilter' {..} = Prelude.rnf arn

instance Data.ToHeaders DeleteFilter where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteFilter where
  toJSON DeleteFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("arn" Data..= arn)]
      )

instance Data.ToPath DeleteFilter where
  toPath = Prelude.const "/filters/delete"

instance Data.ToQuery DeleteFilter where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteFilterResponse' smart constructor.
data DeleteFilterResponse = DeleteFilterResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Number (ARN) of the filter that has been deleted.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFilterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteFilterResponse_httpStatus' - The response's http status code.
--
-- 'arn', 'deleteFilterResponse_arn' - The Amazon Resource Number (ARN) of the filter that has been deleted.
newDeleteFilterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'arn'
  Prelude.Text ->
  DeleteFilterResponse
newDeleteFilterResponse pHttpStatus_ pArn_ =
  DeleteFilterResponse'
    { httpStatus = pHttpStatus_,
      arn = pArn_
    }

-- | The response's http status code.
deleteFilterResponse_httpStatus :: Lens.Lens' DeleteFilterResponse Prelude.Int
deleteFilterResponse_httpStatus = Lens.lens (\DeleteFilterResponse' {httpStatus} -> httpStatus) (\s@DeleteFilterResponse' {} a -> s {httpStatus = a} :: DeleteFilterResponse)

-- | The Amazon Resource Number (ARN) of the filter that has been deleted.
deleteFilterResponse_arn :: Lens.Lens' DeleteFilterResponse Prelude.Text
deleteFilterResponse_arn = Lens.lens (\DeleteFilterResponse' {arn} -> arn) (\s@DeleteFilterResponse' {} a -> s {arn = a} :: DeleteFilterResponse)

instance Prelude.NFData DeleteFilterResponse where
  rnf DeleteFilterResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf arn
