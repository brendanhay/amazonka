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
-- Module      : Amazonka.MediaConnect.AddFlowSources
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds Sources to flow
module Amazonka.MediaConnect.AddFlowSources
  ( -- * Creating a Request
    AddFlowSources (..),
    newAddFlowSources,

    -- * Request Lenses
    addFlowSources_flowArn,
    addFlowSources_sources,

    -- * Destructuring the Response
    AddFlowSourcesResponse (..),
    newAddFlowSourcesResponse,

    -- * Response Lenses
    addFlowSourcesResponse_sources,
    addFlowSourcesResponse_flowArn,
    addFlowSourcesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | A request to add sources to the flow.
--
-- /See:/ 'newAddFlowSources' smart constructor.
data AddFlowSources = AddFlowSources'
  { -- | The flow that you want to mutate.
    flowArn :: Prelude.Text,
    -- | A list of sources that you want to add.
    sources :: [SetSourceRequest]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddFlowSources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'flowArn', 'addFlowSources_flowArn' - The flow that you want to mutate.
--
-- 'sources', 'addFlowSources_sources' - A list of sources that you want to add.
newAddFlowSources ::
  -- | 'flowArn'
  Prelude.Text ->
  AddFlowSources
newAddFlowSources pFlowArn_ =
  AddFlowSources'
    { flowArn = pFlowArn_,
      sources = Prelude.mempty
    }

-- | The flow that you want to mutate.
addFlowSources_flowArn :: Lens.Lens' AddFlowSources Prelude.Text
addFlowSources_flowArn = Lens.lens (\AddFlowSources' {flowArn} -> flowArn) (\s@AddFlowSources' {} a -> s {flowArn = a} :: AddFlowSources)

-- | A list of sources that you want to add.
addFlowSources_sources :: Lens.Lens' AddFlowSources [SetSourceRequest]
addFlowSources_sources = Lens.lens (\AddFlowSources' {sources} -> sources) (\s@AddFlowSources' {} a -> s {sources = a} :: AddFlowSources) Prelude.. Lens.coerced

instance Core.AWSRequest AddFlowSources where
  type
    AWSResponse AddFlowSources =
      AddFlowSourcesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AddFlowSourcesResponse'
            Prelude.<$> (x Core..?> "sources" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "flowArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AddFlowSources where
  hashWithSalt _salt AddFlowSources' {..} =
    _salt `Prelude.hashWithSalt` flowArn
      `Prelude.hashWithSalt` sources

instance Prelude.NFData AddFlowSources where
  rnf AddFlowSources' {..} =
    Prelude.rnf flowArn
      `Prelude.seq` Prelude.rnf sources

instance Core.ToHeaders AddFlowSources where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON AddFlowSources where
  toJSON AddFlowSources' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("sources" Core..= sources)]
      )

instance Core.ToPath AddFlowSources where
  toPath AddFlowSources' {..} =
    Prelude.mconcat
      ["/v1/flows/", Core.toBS flowArn, "/source"]

instance Core.ToQuery AddFlowSources where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAddFlowSourcesResponse' smart constructor.
data AddFlowSourcesResponse = AddFlowSourcesResponse'
  { -- | The details of the newly added sources.
    sources :: Prelude.Maybe [Source],
    -- | The ARN of the flow that these sources were added to.
    flowArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddFlowSourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sources', 'addFlowSourcesResponse_sources' - The details of the newly added sources.
--
-- 'flowArn', 'addFlowSourcesResponse_flowArn' - The ARN of the flow that these sources were added to.
--
-- 'httpStatus', 'addFlowSourcesResponse_httpStatus' - The response's http status code.
newAddFlowSourcesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AddFlowSourcesResponse
newAddFlowSourcesResponse pHttpStatus_ =
  AddFlowSourcesResponse'
    { sources = Prelude.Nothing,
      flowArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The details of the newly added sources.
addFlowSourcesResponse_sources :: Lens.Lens' AddFlowSourcesResponse (Prelude.Maybe [Source])
addFlowSourcesResponse_sources = Lens.lens (\AddFlowSourcesResponse' {sources} -> sources) (\s@AddFlowSourcesResponse' {} a -> s {sources = a} :: AddFlowSourcesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the flow that these sources were added to.
addFlowSourcesResponse_flowArn :: Lens.Lens' AddFlowSourcesResponse (Prelude.Maybe Prelude.Text)
addFlowSourcesResponse_flowArn = Lens.lens (\AddFlowSourcesResponse' {flowArn} -> flowArn) (\s@AddFlowSourcesResponse' {} a -> s {flowArn = a} :: AddFlowSourcesResponse)

-- | The response's http status code.
addFlowSourcesResponse_httpStatus :: Lens.Lens' AddFlowSourcesResponse Prelude.Int
addFlowSourcesResponse_httpStatus = Lens.lens (\AddFlowSourcesResponse' {httpStatus} -> httpStatus) (\s@AddFlowSourcesResponse' {} a -> s {httpStatus = a} :: AddFlowSourcesResponse)

instance Prelude.NFData AddFlowSourcesResponse where
  rnf AddFlowSourcesResponse' {..} =
    Prelude.rnf sources
      `Prelude.seq` Prelude.rnf flowArn
      `Prelude.seq` Prelude.rnf httpStatus
