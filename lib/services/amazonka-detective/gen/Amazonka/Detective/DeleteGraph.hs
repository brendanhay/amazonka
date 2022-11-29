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
-- Module      : Amazonka.Detective.DeleteGraph
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the specified behavior graph and queues it to be deleted. This
-- operation removes the behavior graph from each member account\'s list of
-- behavior graphs.
--
-- @DeleteGraph@ can only be called by the administrator account for a
-- behavior graph.
module Amazonka.Detective.DeleteGraph
  ( -- * Creating a Request
    DeleteGraph (..),
    newDeleteGraph,

    -- * Request Lenses
    deleteGraph_graphArn,

    -- * Destructuring the Response
    DeleteGraphResponse (..),
    newDeleteGraphResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Detective.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteGraph' smart constructor.
data DeleteGraph = DeleteGraph'
  { -- | The ARN of the behavior graph to disable.
    graphArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteGraph' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'graphArn', 'deleteGraph_graphArn' - The ARN of the behavior graph to disable.
newDeleteGraph ::
  -- | 'graphArn'
  Prelude.Text ->
  DeleteGraph
newDeleteGraph pGraphArn_ =
  DeleteGraph' {graphArn = pGraphArn_}

-- | The ARN of the behavior graph to disable.
deleteGraph_graphArn :: Lens.Lens' DeleteGraph Prelude.Text
deleteGraph_graphArn = Lens.lens (\DeleteGraph' {graphArn} -> graphArn) (\s@DeleteGraph' {} a -> s {graphArn = a} :: DeleteGraph)

instance Core.AWSRequest DeleteGraph where
  type AWSResponse DeleteGraph = DeleteGraphResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response = Response.receiveNull DeleteGraphResponse'

instance Prelude.Hashable DeleteGraph where
  hashWithSalt _salt DeleteGraph' {..} =
    _salt `Prelude.hashWithSalt` graphArn

instance Prelude.NFData DeleteGraph where
  rnf DeleteGraph' {..} = Prelude.rnf graphArn

instance Core.ToHeaders DeleteGraph where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteGraph where
  toJSON DeleteGraph' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("GraphArn" Core..= graphArn)]
      )

instance Core.ToPath DeleteGraph where
  toPath = Prelude.const "/graph/removal"

instance Core.ToQuery DeleteGraph where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteGraphResponse' smart constructor.
data DeleteGraphResponse = DeleteGraphResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteGraphResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteGraphResponse ::
  DeleteGraphResponse
newDeleteGraphResponse = DeleteGraphResponse'

instance Prelude.NFData DeleteGraphResponse where
  rnf _ = ()
