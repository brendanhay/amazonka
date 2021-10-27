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
-- Module      : Network.AWS.SMSVoice.ListConfigurationSets
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List all of the configuration sets associated with your Amazon Pinpoint
-- account in the current region.
module Network.AWS.SMSVoice.ListConfigurationSets
  ( -- * Creating a Request
    ListConfigurationSets (..),
    newListConfigurationSets,

    -- * Request Lenses
    listConfigurationSets_nextToken,
    listConfigurationSets_pageSize,

    -- * Destructuring the Response
    ListConfigurationSetsResponse (..),
    newListConfigurationSetsResponse,

    -- * Response Lenses
    listConfigurationSetsResponse_configurationSets,
    listConfigurationSetsResponse_nextToken,
    listConfigurationSetsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SMSVoice.Types

-- | /See:/ 'newListConfigurationSets' smart constructor.
data ListConfigurationSets = ListConfigurationSets'
  { -- | A token returned from a previous call to the API that indicates the
    -- position in the list of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Used to specify the number of items that should be returned in the
    -- response.
    pageSize :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListConfigurationSets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listConfigurationSets_nextToken' - A token returned from a previous call to the API that indicates the
-- position in the list of results.
--
-- 'pageSize', 'listConfigurationSets_pageSize' - Used to specify the number of items that should be returned in the
-- response.
newListConfigurationSets ::
  ListConfigurationSets
newListConfigurationSets =
  ListConfigurationSets'
    { nextToken = Prelude.Nothing,
      pageSize = Prelude.Nothing
    }

-- | A token returned from a previous call to the API that indicates the
-- position in the list of results.
listConfigurationSets_nextToken :: Lens.Lens' ListConfigurationSets (Prelude.Maybe Prelude.Text)
listConfigurationSets_nextToken = Lens.lens (\ListConfigurationSets' {nextToken} -> nextToken) (\s@ListConfigurationSets' {} a -> s {nextToken = a} :: ListConfigurationSets)

-- | Used to specify the number of items that should be returned in the
-- response.
listConfigurationSets_pageSize :: Lens.Lens' ListConfigurationSets (Prelude.Maybe Prelude.Text)
listConfigurationSets_pageSize = Lens.lens (\ListConfigurationSets' {pageSize} -> pageSize) (\s@ListConfigurationSets' {} a -> s {pageSize = a} :: ListConfigurationSets)

instance Core.AWSRequest ListConfigurationSets where
  type
    AWSResponse ListConfigurationSets =
      ListConfigurationSetsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListConfigurationSetsResponse'
            Prelude.<$> ( x Core..?> "ConfigurationSets"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListConfigurationSets

instance Prelude.NFData ListConfigurationSets

instance Core.ToHeaders ListConfigurationSets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListConfigurationSets where
  toPath =
    Prelude.const "/v1/sms-voice/configuration-sets"

instance Core.ToQuery ListConfigurationSets where
  toQuery ListConfigurationSets' {..} =
    Prelude.mconcat
      [ "NextToken" Core.=: nextToken,
        "PageSize" Core.=: pageSize
      ]

-- | An object that contains information about the configuration sets for
-- your account in the current region.
--
-- /See:/ 'newListConfigurationSetsResponse' smart constructor.
data ListConfigurationSetsResponse = ListConfigurationSetsResponse'
  { -- | An object that contains a list of configuration sets for your account in
    -- the current region.
    configurationSets :: Prelude.Maybe [Prelude.Text],
    -- | A token returned from a previous call to ListConfigurationSets to
    -- indicate the position in the list of configuration sets.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListConfigurationSetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationSets', 'listConfigurationSetsResponse_configurationSets' - An object that contains a list of configuration sets for your account in
-- the current region.
--
-- 'nextToken', 'listConfigurationSetsResponse_nextToken' - A token returned from a previous call to ListConfigurationSets to
-- indicate the position in the list of configuration sets.
--
-- 'httpStatus', 'listConfigurationSetsResponse_httpStatus' - The response's http status code.
newListConfigurationSetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListConfigurationSetsResponse
newListConfigurationSetsResponse pHttpStatus_ =
  ListConfigurationSetsResponse'
    { configurationSets =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object that contains a list of configuration sets for your account in
-- the current region.
listConfigurationSetsResponse_configurationSets :: Lens.Lens' ListConfigurationSetsResponse (Prelude.Maybe [Prelude.Text])
listConfigurationSetsResponse_configurationSets = Lens.lens (\ListConfigurationSetsResponse' {configurationSets} -> configurationSets) (\s@ListConfigurationSetsResponse' {} a -> s {configurationSets = a} :: ListConfigurationSetsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A token returned from a previous call to ListConfigurationSets to
-- indicate the position in the list of configuration sets.
listConfigurationSetsResponse_nextToken :: Lens.Lens' ListConfigurationSetsResponse (Prelude.Maybe Prelude.Text)
listConfigurationSetsResponse_nextToken = Lens.lens (\ListConfigurationSetsResponse' {nextToken} -> nextToken) (\s@ListConfigurationSetsResponse' {} a -> s {nextToken = a} :: ListConfigurationSetsResponse)

-- | The response's http status code.
listConfigurationSetsResponse_httpStatus :: Lens.Lens' ListConfigurationSetsResponse Prelude.Int
listConfigurationSetsResponse_httpStatus = Lens.lens (\ListConfigurationSetsResponse' {httpStatus} -> httpStatus) (\s@ListConfigurationSetsResponse' {} a -> s {httpStatus = a} :: ListConfigurationSetsResponse)

instance Prelude.NFData ListConfigurationSetsResponse
