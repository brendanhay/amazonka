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
-- Module      : Amazonka.MediaConnect.RemoveFlowSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a source from an existing flow. This request can be made only if
-- there is more than one source on the flow.
module Amazonka.MediaConnect.RemoveFlowSource
  ( -- * Creating a Request
    RemoveFlowSource (..),
    newRemoveFlowSource,

    -- * Request Lenses
    removeFlowSource_flowArn,
    removeFlowSource_sourceArn,

    -- * Destructuring the Response
    RemoveFlowSourceResponse (..),
    newRemoveFlowSourceResponse,

    -- * Response Lenses
    removeFlowSourceResponse_flowArn,
    removeFlowSourceResponse_sourceArn,
    removeFlowSourceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRemoveFlowSource' smart constructor.
data RemoveFlowSource = RemoveFlowSource'
  { -- | The flow that you want to remove a source from.
    flowArn :: Prelude.Text,
    -- | The ARN of the source that you want to remove.
    sourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveFlowSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'flowArn', 'removeFlowSource_flowArn' - The flow that you want to remove a source from.
--
-- 'sourceArn', 'removeFlowSource_sourceArn' - The ARN of the source that you want to remove.
newRemoveFlowSource ::
  -- | 'flowArn'
  Prelude.Text ->
  -- | 'sourceArn'
  Prelude.Text ->
  RemoveFlowSource
newRemoveFlowSource pFlowArn_ pSourceArn_ =
  RemoveFlowSource'
    { flowArn = pFlowArn_,
      sourceArn = pSourceArn_
    }

-- | The flow that you want to remove a source from.
removeFlowSource_flowArn :: Lens.Lens' RemoveFlowSource Prelude.Text
removeFlowSource_flowArn = Lens.lens (\RemoveFlowSource' {flowArn} -> flowArn) (\s@RemoveFlowSource' {} a -> s {flowArn = a} :: RemoveFlowSource)

-- | The ARN of the source that you want to remove.
removeFlowSource_sourceArn :: Lens.Lens' RemoveFlowSource Prelude.Text
removeFlowSource_sourceArn = Lens.lens (\RemoveFlowSource' {sourceArn} -> sourceArn) (\s@RemoveFlowSource' {} a -> s {sourceArn = a} :: RemoveFlowSource)

instance Core.AWSRequest RemoveFlowSource where
  type
    AWSResponse RemoveFlowSource =
      RemoveFlowSourceResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RemoveFlowSourceResponse'
            Prelude.<$> (x Data..?> "flowArn")
            Prelude.<*> (x Data..?> "sourceArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RemoveFlowSource where
  hashWithSalt _salt RemoveFlowSource' {..} =
    _salt
      `Prelude.hashWithSalt` flowArn
      `Prelude.hashWithSalt` sourceArn

instance Prelude.NFData RemoveFlowSource where
  rnf RemoveFlowSource' {..} =
    Prelude.rnf flowArn
      `Prelude.seq` Prelude.rnf sourceArn

instance Data.ToHeaders RemoveFlowSource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath RemoveFlowSource where
  toPath RemoveFlowSource' {..} =
    Prelude.mconcat
      [ "/v1/flows/",
        Data.toBS flowArn,
        "/source/",
        Data.toBS sourceArn
      ]

instance Data.ToQuery RemoveFlowSource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRemoveFlowSourceResponse' smart constructor.
data RemoveFlowSourceResponse = RemoveFlowSourceResponse'
  { -- | The ARN of the flow that is associated with the source you removed.
    flowArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the source that was removed.
    sourceArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveFlowSourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'flowArn', 'removeFlowSourceResponse_flowArn' - The ARN of the flow that is associated with the source you removed.
--
-- 'sourceArn', 'removeFlowSourceResponse_sourceArn' - The ARN of the source that was removed.
--
-- 'httpStatus', 'removeFlowSourceResponse_httpStatus' - The response's http status code.
newRemoveFlowSourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RemoveFlowSourceResponse
newRemoveFlowSourceResponse pHttpStatus_ =
  RemoveFlowSourceResponse'
    { flowArn =
        Prelude.Nothing,
      sourceArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the flow that is associated with the source you removed.
removeFlowSourceResponse_flowArn :: Lens.Lens' RemoveFlowSourceResponse (Prelude.Maybe Prelude.Text)
removeFlowSourceResponse_flowArn = Lens.lens (\RemoveFlowSourceResponse' {flowArn} -> flowArn) (\s@RemoveFlowSourceResponse' {} a -> s {flowArn = a} :: RemoveFlowSourceResponse)

-- | The ARN of the source that was removed.
removeFlowSourceResponse_sourceArn :: Lens.Lens' RemoveFlowSourceResponse (Prelude.Maybe Prelude.Text)
removeFlowSourceResponse_sourceArn = Lens.lens (\RemoveFlowSourceResponse' {sourceArn} -> sourceArn) (\s@RemoveFlowSourceResponse' {} a -> s {sourceArn = a} :: RemoveFlowSourceResponse)

-- | The response's http status code.
removeFlowSourceResponse_httpStatus :: Lens.Lens' RemoveFlowSourceResponse Prelude.Int
removeFlowSourceResponse_httpStatus = Lens.lens (\RemoveFlowSourceResponse' {httpStatus} -> httpStatus) (\s@RemoveFlowSourceResponse' {} a -> s {httpStatus = a} :: RemoveFlowSourceResponse)

instance Prelude.NFData RemoveFlowSourceResponse where
  rnf RemoveFlowSourceResponse' {..} =
    Prelude.rnf flowArn
      `Prelude.seq` Prelude.rnf sourceArn
      `Prelude.seq` Prelude.rnf httpStatus
