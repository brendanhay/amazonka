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
-- Module      : Amazonka.CloudWatch.DeleteMetricStream
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes the metric stream that you specify.
module Amazonka.CloudWatch.DeleteMetricStream
  ( -- * Creating a Request
    DeleteMetricStream (..),
    newDeleteMetricStream,

    -- * Request Lenses
    deleteMetricStream_name,

    -- * Destructuring the Response
    DeleteMetricStreamResponse (..),
    newDeleteMetricStreamResponse,

    -- * Response Lenses
    deleteMetricStreamResponse_httpStatus,
  )
where

import Amazonka.CloudWatch.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteMetricStream' smart constructor.
data DeleteMetricStream = DeleteMetricStream'
  { -- | The name of the metric stream to delete.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteMetricStream' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteMetricStream_name' - The name of the metric stream to delete.
newDeleteMetricStream ::
  -- | 'name'
  Prelude.Text ->
  DeleteMetricStream
newDeleteMetricStream pName_ =
  DeleteMetricStream' {name = pName_}

-- | The name of the metric stream to delete.
deleteMetricStream_name :: Lens.Lens' DeleteMetricStream Prelude.Text
deleteMetricStream_name = Lens.lens (\DeleteMetricStream' {name} -> name) (\s@DeleteMetricStream' {} a -> s {name = a} :: DeleteMetricStream)

instance Core.AWSRequest DeleteMetricStream where
  type
    AWSResponse DeleteMetricStream =
      DeleteMetricStreamResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DeleteMetricStreamResult"
      ( \s h x ->
          DeleteMetricStreamResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteMetricStream where
  hashWithSalt _salt DeleteMetricStream' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData DeleteMetricStream where
  rnf DeleteMetricStream' {..} = Prelude.rnf name

instance Core.ToHeaders DeleteMetricStream where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DeleteMetricStream where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteMetricStream where
  toQuery DeleteMetricStream' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DeleteMetricStream" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-08-01" :: Prelude.ByteString),
        "Name" Core.=: name
      ]

-- | /See:/ 'newDeleteMetricStreamResponse' smart constructor.
data DeleteMetricStreamResponse = DeleteMetricStreamResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteMetricStreamResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteMetricStreamResponse_httpStatus' - The response's http status code.
newDeleteMetricStreamResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteMetricStreamResponse
newDeleteMetricStreamResponse pHttpStatus_ =
  DeleteMetricStreamResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteMetricStreamResponse_httpStatus :: Lens.Lens' DeleteMetricStreamResponse Prelude.Int
deleteMetricStreamResponse_httpStatus = Lens.lens (\DeleteMetricStreamResponse' {httpStatus} -> httpStatus) (\s@DeleteMetricStreamResponse' {} a -> s {httpStatus = a} :: DeleteMetricStreamResponse)

instance Prelude.NFData DeleteMetricStreamResponse where
  rnf DeleteMetricStreamResponse' {..} =
    Prelude.rnf httpStatus
