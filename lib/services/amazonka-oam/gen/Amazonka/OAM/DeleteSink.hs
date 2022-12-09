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
-- Module      : Amazonka.OAM.DeleteSink
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a sink. You must delete all links to a sink before you can
-- delete that sink.
module Amazonka.OAM.DeleteSink
  ( -- * Creating a Request
    DeleteSink (..),
    newDeleteSink,

    -- * Request Lenses
    deleteSink_identifier,

    -- * Destructuring the Response
    DeleteSinkResponse (..),
    newDeleteSinkResponse,

    -- * Response Lenses
    deleteSinkResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteSink' smart constructor.
data DeleteSink = DeleteSink'
  { -- | The ARN of the sink to delete.
    identifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSink' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identifier', 'deleteSink_identifier' - The ARN of the sink to delete.
newDeleteSink ::
  -- | 'identifier'
  Prelude.Text ->
  DeleteSink
newDeleteSink pIdentifier_ =
  DeleteSink' {identifier = pIdentifier_}

-- | The ARN of the sink to delete.
deleteSink_identifier :: Lens.Lens' DeleteSink Prelude.Text
deleteSink_identifier = Lens.lens (\DeleteSink' {identifier} -> identifier) (\s@DeleteSink' {} a -> s {identifier = a} :: DeleteSink)

instance Core.AWSRequest DeleteSink where
  type AWSResponse DeleteSink = DeleteSinkResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteSinkResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteSink where
  hashWithSalt _salt DeleteSink' {..} =
    _salt `Prelude.hashWithSalt` identifier

instance Prelude.NFData DeleteSink where
  rnf DeleteSink' {..} = Prelude.rnf identifier

instance Data.ToHeaders DeleteSink where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteSink where
  toJSON DeleteSink' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Identifier" Data..= identifier)]
      )

instance Data.ToPath DeleteSink where
  toPath = Prelude.const "/DeleteSink"

instance Data.ToQuery DeleteSink where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteSinkResponse' smart constructor.
data DeleteSinkResponse = DeleteSinkResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSinkResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteSinkResponse_httpStatus' - The response's http status code.
newDeleteSinkResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteSinkResponse
newDeleteSinkResponse pHttpStatus_ =
  DeleteSinkResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteSinkResponse_httpStatus :: Lens.Lens' DeleteSinkResponse Prelude.Int
deleteSinkResponse_httpStatus = Lens.lens (\DeleteSinkResponse' {httpStatus} -> httpStatus) (\s@DeleteSinkResponse' {} a -> s {httpStatus = a} :: DeleteSinkResponse)

instance Prelude.NFData DeleteSinkResponse where
  rnf DeleteSinkResponse' {..} = Prelude.rnf httpStatus
