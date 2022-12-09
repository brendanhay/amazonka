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
-- Module      : Amazonka.Omics.CancelRun
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a run.
module Amazonka.Omics.CancelRun
  ( -- * Creating a Request
    CancelRun (..),
    newCancelRun,

    -- * Request Lenses
    cancelRun_id,

    -- * Destructuring the Response
    CancelRunResponse (..),
    newCancelRunResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCancelRun' smart constructor.
data CancelRun = CancelRun'
  { -- | The run\'s ID.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelRun' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'cancelRun_id' - The run\'s ID.
newCancelRun ::
  -- | 'id'
  Prelude.Text ->
  CancelRun
newCancelRun pId_ = CancelRun' {id = pId_}

-- | The run\'s ID.
cancelRun_id :: Lens.Lens' CancelRun Prelude.Text
cancelRun_id = Lens.lens (\CancelRun' {id} -> id) (\s@CancelRun' {} a -> s {id = a} :: CancelRun)

instance Core.AWSRequest CancelRun where
  type AWSResponse CancelRun = CancelRunResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response = Response.receiveNull CancelRunResponse'

instance Prelude.Hashable CancelRun where
  hashWithSalt _salt CancelRun' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData CancelRun where
  rnf CancelRun' {..} = Prelude.rnf id

instance Data.ToHeaders CancelRun where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CancelRun where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath CancelRun where
  toPath CancelRun' {..} =
    Prelude.mconcat ["/run/", Data.toBS id, "/cancel"]

instance Data.ToQuery CancelRun where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCancelRunResponse' smart constructor.
data CancelRunResponse = CancelRunResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelRunResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newCancelRunResponse ::
  CancelRunResponse
newCancelRunResponse = CancelRunResponse'

instance Prelude.NFData CancelRunResponse where
  rnf _ = ()
