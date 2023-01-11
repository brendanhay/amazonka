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
-- Module      : Amazonka.Detective.CreateGraph
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new behavior graph for the calling account, and sets that
-- account as the administrator account. This operation is called by the
-- account that is enabling Detective.
--
-- Before you try to enable Detective, make sure that your account has been
-- enrolled in Amazon GuardDuty for at least 48 hours. If you do not meet
-- this requirement, you cannot enable Detective. If you do meet the
-- GuardDuty prerequisite, then when you make the request to enable
-- Detective, it checks whether your data volume is within the Detective
-- quota. If it exceeds the quota, then you cannot enable Detective.
--
-- The operation also enables Detective for the calling account in the
-- currently selected Region. It returns the ARN of the new behavior graph.
--
-- @CreateGraph@ triggers a process to create the corresponding data tables
-- for the new behavior graph.
--
-- An account can only be the administrator account for one behavior graph
-- within a Region. If the same account calls @CreateGraph@ with the same
-- administrator account, it always returns the same behavior graph ARN. It
-- does not create a new behavior graph.
module Amazonka.Detective.CreateGraph
  ( -- * Creating a Request
    CreateGraph (..),
    newCreateGraph,

    -- * Request Lenses
    createGraph_tags,

    -- * Destructuring the Response
    CreateGraphResponse (..),
    newCreateGraphResponse,

    -- * Response Lenses
    createGraphResponse_graphArn,
    createGraphResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Detective.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateGraph' smart constructor.
data CreateGraph = CreateGraph'
  { -- | The tags to assign to the new behavior graph. You can add up to 50 tags.
    -- For each tag, you provide the tag key and the tag value. Each tag key
    -- can contain up to 128 characters. Each tag value can contain up to 256
    -- characters.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateGraph' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createGraph_tags' - The tags to assign to the new behavior graph. You can add up to 50 tags.
-- For each tag, you provide the tag key and the tag value. Each tag key
-- can contain up to 128 characters. Each tag value can contain up to 256
-- characters.
newCreateGraph ::
  CreateGraph
newCreateGraph = CreateGraph' {tags = Prelude.Nothing}

-- | The tags to assign to the new behavior graph. You can add up to 50 tags.
-- For each tag, you provide the tag key and the tag value. Each tag key
-- can contain up to 128 characters. Each tag value can contain up to 256
-- characters.
createGraph_tags :: Lens.Lens' CreateGraph (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createGraph_tags = Lens.lens (\CreateGraph' {tags} -> tags) (\s@CreateGraph' {} a -> s {tags = a} :: CreateGraph) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest CreateGraph where
  type AWSResponse CreateGraph = CreateGraphResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateGraphResponse'
            Prelude.<$> (x Data..?> "GraphArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateGraph where
  hashWithSalt _salt CreateGraph' {..} =
    _salt `Prelude.hashWithSalt` tags

instance Prelude.NFData CreateGraph where
  rnf CreateGraph' {..} = Prelude.rnf tags

instance Data.ToHeaders CreateGraph where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateGraph where
  toJSON CreateGraph' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Tags" Data..=) Prelude.<$> tags]
      )

instance Data.ToPath CreateGraph where
  toPath = Prelude.const "/graph"

instance Data.ToQuery CreateGraph where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateGraphResponse' smart constructor.
data CreateGraphResponse = CreateGraphResponse'
  { -- | The ARN of the new behavior graph.
    graphArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateGraphResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'graphArn', 'createGraphResponse_graphArn' - The ARN of the new behavior graph.
--
-- 'httpStatus', 'createGraphResponse_httpStatus' - The response's http status code.
newCreateGraphResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateGraphResponse
newCreateGraphResponse pHttpStatus_ =
  CreateGraphResponse'
    { graphArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the new behavior graph.
createGraphResponse_graphArn :: Lens.Lens' CreateGraphResponse (Prelude.Maybe Prelude.Text)
createGraphResponse_graphArn = Lens.lens (\CreateGraphResponse' {graphArn} -> graphArn) (\s@CreateGraphResponse' {} a -> s {graphArn = a} :: CreateGraphResponse)

-- | The response's http status code.
createGraphResponse_httpStatus :: Lens.Lens' CreateGraphResponse Prelude.Int
createGraphResponse_httpStatus = Lens.lens (\CreateGraphResponse' {httpStatus} -> httpStatus) (\s@CreateGraphResponse' {} a -> s {httpStatus = a} :: CreateGraphResponse)

instance Prelude.NFData CreateGraphResponse where
  rnf CreateGraphResponse' {..} =
    Prelude.rnf graphArn
      `Prelude.seq` Prelude.rnf httpStatus
