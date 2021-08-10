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
-- Module      : Network.AWS.IoTData.ListNamedShadowsForThing
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the shadows for the specified thing.
module Network.AWS.IoTData.ListNamedShadowsForThing
  ( -- * Creating a Request
    ListNamedShadowsForThing (..),
    newListNamedShadowsForThing,

    -- * Request Lenses
    listNamedShadowsForThing_nextToken,
    listNamedShadowsForThing_pageSize,
    listNamedShadowsForThing_thingName,

    -- * Destructuring the Response
    ListNamedShadowsForThingResponse (..),
    newListNamedShadowsForThingResponse,

    -- * Response Lenses
    listNamedShadowsForThingResponse_nextToken,
    listNamedShadowsForThingResponse_timestamp,
    listNamedShadowsForThingResponse_results,
    listNamedShadowsForThingResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTData.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListNamedShadowsForThing' smart constructor.
data ListNamedShadowsForThing = ListNamedShadowsForThing'
  { -- | The token to retrieve the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The result page size.
    pageSize :: Prelude.Maybe Prelude.Natural,
    -- | The name of the thing.
    thingName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListNamedShadowsForThing' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listNamedShadowsForThing_nextToken' - The token to retrieve the next set of results.
--
-- 'pageSize', 'listNamedShadowsForThing_pageSize' - The result page size.
--
-- 'thingName', 'listNamedShadowsForThing_thingName' - The name of the thing.
newListNamedShadowsForThing ::
  -- | 'thingName'
  Prelude.Text ->
  ListNamedShadowsForThing
newListNamedShadowsForThing pThingName_ =
  ListNamedShadowsForThing'
    { nextToken =
        Prelude.Nothing,
      pageSize = Prelude.Nothing,
      thingName = pThingName_
    }

-- | The token to retrieve the next set of results.
listNamedShadowsForThing_nextToken :: Lens.Lens' ListNamedShadowsForThing (Prelude.Maybe Prelude.Text)
listNamedShadowsForThing_nextToken = Lens.lens (\ListNamedShadowsForThing' {nextToken} -> nextToken) (\s@ListNamedShadowsForThing' {} a -> s {nextToken = a} :: ListNamedShadowsForThing)

-- | The result page size.
listNamedShadowsForThing_pageSize :: Lens.Lens' ListNamedShadowsForThing (Prelude.Maybe Prelude.Natural)
listNamedShadowsForThing_pageSize = Lens.lens (\ListNamedShadowsForThing' {pageSize} -> pageSize) (\s@ListNamedShadowsForThing' {} a -> s {pageSize = a} :: ListNamedShadowsForThing)

-- | The name of the thing.
listNamedShadowsForThing_thingName :: Lens.Lens' ListNamedShadowsForThing Prelude.Text
listNamedShadowsForThing_thingName = Lens.lens (\ListNamedShadowsForThing' {thingName} -> thingName) (\s@ListNamedShadowsForThing' {} a -> s {thingName = a} :: ListNamedShadowsForThing)

instance Core.AWSRequest ListNamedShadowsForThing where
  type
    AWSResponse ListNamedShadowsForThing =
      ListNamedShadowsForThingResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListNamedShadowsForThingResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "timestamp")
            Prelude.<*> (x Core..?> "results" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListNamedShadowsForThing

instance Prelude.NFData ListNamedShadowsForThing

instance Core.ToHeaders ListNamedShadowsForThing where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListNamedShadowsForThing where
  toPath ListNamedShadowsForThing' {..} =
    Prelude.mconcat
      [ "/api/things/shadow/ListNamedShadowsForThing/",
        Core.toBS thingName
      ]

instance Core.ToQuery ListNamedShadowsForThing where
  toQuery ListNamedShadowsForThing' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "pageSize" Core.=: pageSize
      ]

-- | /See:/ 'newListNamedShadowsForThingResponse' smart constructor.
data ListNamedShadowsForThingResponse = ListNamedShadowsForThingResponse'
  { -- | The token for the next set of results, or null if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Epoch date and time the response was generated by AWS IoT.
    timestamp :: Prelude.Maybe Prelude.Integer,
    -- | The list of shadows for the specified thing.
    results :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListNamedShadowsForThingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listNamedShadowsForThingResponse_nextToken' - The token for the next set of results, or null if there are no
-- additional results.
--
-- 'timestamp', 'listNamedShadowsForThingResponse_timestamp' - The Epoch date and time the response was generated by AWS IoT.
--
-- 'results', 'listNamedShadowsForThingResponse_results' - The list of shadows for the specified thing.
--
-- 'httpStatus', 'listNamedShadowsForThingResponse_httpStatus' - The response's http status code.
newListNamedShadowsForThingResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListNamedShadowsForThingResponse
newListNamedShadowsForThingResponse pHttpStatus_ =
  ListNamedShadowsForThingResponse'
    { nextToken =
        Prelude.Nothing,
      timestamp = Prelude.Nothing,
      results = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of results, or null if there are no
-- additional results.
listNamedShadowsForThingResponse_nextToken :: Lens.Lens' ListNamedShadowsForThingResponse (Prelude.Maybe Prelude.Text)
listNamedShadowsForThingResponse_nextToken = Lens.lens (\ListNamedShadowsForThingResponse' {nextToken} -> nextToken) (\s@ListNamedShadowsForThingResponse' {} a -> s {nextToken = a} :: ListNamedShadowsForThingResponse)

-- | The Epoch date and time the response was generated by AWS IoT.
listNamedShadowsForThingResponse_timestamp :: Lens.Lens' ListNamedShadowsForThingResponse (Prelude.Maybe Prelude.Integer)
listNamedShadowsForThingResponse_timestamp = Lens.lens (\ListNamedShadowsForThingResponse' {timestamp} -> timestamp) (\s@ListNamedShadowsForThingResponse' {} a -> s {timestamp = a} :: ListNamedShadowsForThingResponse)

-- | The list of shadows for the specified thing.
listNamedShadowsForThingResponse_results :: Lens.Lens' ListNamedShadowsForThingResponse (Prelude.Maybe [Prelude.Text])
listNamedShadowsForThingResponse_results = Lens.lens (\ListNamedShadowsForThingResponse' {results} -> results) (\s@ListNamedShadowsForThingResponse' {} a -> s {results = a} :: ListNamedShadowsForThingResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listNamedShadowsForThingResponse_httpStatus :: Lens.Lens' ListNamedShadowsForThingResponse Prelude.Int
listNamedShadowsForThingResponse_httpStatus = Lens.lens (\ListNamedShadowsForThingResponse' {httpStatus} -> httpStatus) (\s@ListNamedShadowsForThingResponse' {} a -> s {httpStatus = a} :: ListNamedShadowsForThingResponse)

instance
  Prelude.NFData
    ListNamedShadowsForThingResponse
