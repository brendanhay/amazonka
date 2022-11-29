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
-- Module      : Amazonka.RobOMaker.BatchDeleteWorlds
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes one or more worlds in a batch operation.
module Amazonka.RobOMaker.BatchDeleteWorlds
  ( -- * Creating a Request
    BatchDeleteWorlds (..),
    newBatchDeleteWorlds,

    -- * Request Lenses
    batchDeleteWorlds_worlds,

    -- * Destructuring the Response
    BatchDeleteWorldsResponse (..),
    newBatchDeleteWorldsResponse,

    -- * Response Lenses
    batchDeleteWorldsResponse_unprocessedWorlds,
    batchDeleteWorldsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.RobOMaker.Types

-- | /See:/ 'newBatchDeleteWorlds' smart constructor.
data BatchDeleteWorlds = BatchDeleteWorlds'
  { -- | A list of Amazon Resource Names (arns) that correspond to worlds to
    -- delete.
    worlds :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDeleteWorlds' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'worlds', 'batchDeleteWorlds_worlds' - A list of Amazon Resource Names (arns) that correspond to worlds to
-- delete.
newBatchDeleteWorlds ::
  -- | 'worlds'
  Prelude.NonEmpty Prelude.Text ->
  BatchDeleteWorlds
newBatchDeleteWorlds pWorlds_ =
  BatchDeleteWorlds'
    { worlds =
        Lens.coerced Lens.# pWorlds_
    }

-- | A list of Amazon Resource Names (arns) that correspond to worlds to
-- delete.
batchDeleteWorlds_worlds :: Lens.Lens' BatchDeleteWorlds (Prelude.NonEmpty Prelude.Text)
batchDeleteWorlds_worlds = Lens.lens (\BatchDeleteWorlds' {worlds} -> worlds) (\s@BatchDeleteWorlds' {} a -> s {worlds = a} :: BatchDeleteWorlds) Prelude.. Lens.coerced

instance Core.AWSRequest BatchDeleteWorlds where
  type
    AWSResponse BatchDeleteWorlds =
      BatchDeleteWorldsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchDeleteWorldsResponse'
            Prelude.<$> (x Core..?> "unprocessedWorlds")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchDeleteWorlds where
  hashWithSalt _salt BatchDeleteWorlds' {..} =
    _salt `Prelude.hashWithSalt` worlds

instance Prelude.NFData BatchDeleteWorlds where
  rnf BatchDeleteWorlds' {..} = Prelude.rnf worlds

instance Core.ToHeaders BatchDeleteWorlds where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON BatchDeleteWorlds where
  toJSON BatchDeleteWorlds' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("worlds" Core..= worlds)]
      )

instance Core.ToPath BatchDeleteWorlds where
  toPath = Prelude.const "/batchDeleteWorlds"

instance Core.ToQuery BatchDeleteWorlds where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchDeleteWorldsResponse' smart constructor.
data BatchDeleteWorldsResponse = BatchDeleteWorldsResponse'
  { -- | A list of unprocessed worlds associated with the call. These worlds were
    -- not deleted.
    unprocessedWorlds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDeleteWorldsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'unprocessedWorlds', 'batchDeleteWorldsResponse_unprocessedWorlds' - A list of unprocessed worlds associated with the call. These worlds were
-- not deleted.
--
-- 'httpStatus', 'batchDeleteWorldsResponse_httpStatus' - The response's http status code.
newBatchDeleteWorldsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchDeleteWorldsResponse
newBatchDeleteWorldsResponse pHttpStatus_ =
  BatchDeleteWorldsResponse'
    { unprocessedWorlds =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of unprocessed worlds associated with the call. These worlds were
-- not deleted.
batchDeleteWorldsResponse_unprocessedWorlds :: Lens.Lens' BatchDeleteWorldsResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
batchDeleteWorldsResponse_unprocessedWorlds = Lens.lens (\BatchDeleteWorldsResponse' {unprocessedWorlds} -> unprocessedWorlds) (\s@BatchDeleteWorldsResponse' {} a -> s {unprocessedWorlds = a} :: BatchDeleteWorldsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchDeleteWorldsResponse_httpStatus :: Lens.Lens' BatchDeleteWorldsResponse Prelude.Int
batchDeleteWorldsResponse_httpStatus = Lens.lens (\BatchDeleteWorldsResponse' {httpStatus} -> httpStatus) (\s@BatchDeleteWorldsResponse' {} a -> s {httpStatus = a} :: BatchDeleteWorldsResponse)

instance Prelude.NFData BatchDeleteWorldsResponse where
  rnf BatchDeleteWorldsResponse' {..} =
    Prelude.rnf unprocessedWorlds
      `Prelude.seq` Prelude.rnf httpStatus
