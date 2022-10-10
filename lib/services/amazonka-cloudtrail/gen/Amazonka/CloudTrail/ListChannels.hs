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
-- Module      : Amazonka.CloudTrail.ListChannels
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all CloudTrail channels.
module Amazonka.CloudTrail.ListChannels
  ( -- * Creating a Request
    ListChannels (..),
    newListChannels,

    -- * Request Lenses
    listChannels_nextToken,
    listChannels_maxResults,

    -- * Destructuring the Response
    ListChannelsResponse (..),
    newListChannelsResponse,

    -- * Response Lenses
    listChannelsResponse_nextToken,
    listChannelsResponse_channels,
    listChannelsResponse_httpStatus,
  )
where

import Amazonka.CloudTrail.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListChannels' smart constructor.
data ListChannels = ListChannels'
  { -- | A token you can use to get the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of CloudTrail channels to display on a single page.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListChannels' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listChannels_nextToken' - A token you can use to get the next page of results.
--
-- 'maxResults', 'listChannels_maxResults' - The maximum number of CloudTrail channels to display on a single page.
newListChannels ::
  ListChannels
newListChannels =
  ListChannels'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | A token you can use to get the next page of results.
listChannels_nextToken :: Lens.Lens' ListChannels (Prelude.Maybe Prelude.Text)
listChannels_nextToken = Lens.lens (\ListChannels' {nextToken} -> nextToken) (\s@ListChannels' {} a -> s {nextToken = a} :: ListChannels)

-- | The maximum number of CloudTrail channels to display on a single page.
listChannels_maxResults :: Lens.Lens' ListChannels (Prelude.Maybe Prelude.Natural)
listChannels_maxResults = Lens.lens (\ListChannels' {maxResults} -> maxResults) (\s@ListChannels' {} a -> s {maxResults = a} :: ListChannels)

instance Core.AWSRequest ListChannels where
  type AWSResponse ListChannels = ListChannelsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListChannelsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Channels" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListChannels where
  hashWithSalt _salt ListChannels' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListChannels where
  rnf ListChannels' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListChannels where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.ListChannels" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListChannels where
  toJSON ListChannels' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListChannels where
  toPath = Prelude.const "/"

instance Core.ToQuery ListChannels where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListChannelsResponse' smart constructor.
data ListChannelsResponse = ListChannelsResponse'
  { -- | A token used to get the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of CloudTrail channels.
    channels :: Prelude.Maybe [Channel],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListChannelsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listChannelsResponse_nextToken' - A token used to get the next page of results.
--
-- 'channels', 'listChannelsResponse_channels' - The list of CloudTrail channels.
--
-- 'httpStatus', 'listChannelsResponse_httpStatus' - The response's http status code.
newListChannelsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListChannelsResponse
newListChannelsResponse pHttpStatus_ =
  ListChannelsResponse'
    { nextToken = Prelude.Nothing,
      channels = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token used to get the next page of results.
listChannelsResponse_nextToken :: Lens.Lens' ListChannelsResponse (Prelude.Maybe Prelude.Text)
listChannelsResponse_nextToken = Lens.lens (\ListChannelsResponse' {nextToken} -> nextToken) (\s@ListChannelsResponse' {} a -> s {nextToken = a} :: ListChannelsResponse)

-- | The list of CloudTrail channels.
listChannelsResponse_channels :: Lens.Lens' ListChannelsResponse (Prelude.Maybe [Channel])
listChannelsResponse_channels = Lens.lens (\ListChannelsResponse' {channels} -> channels) (\s@ListChannelsResponse' {} a -> s {channels = a} :: ListChannelsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listChannelsResponse_httpStatus :: Lens.Lens' ListChannelsResponse Prelude.Int
listChannelsResponse_httpStatus = Lens.lens (\ListChannelsResponse' {httpStatus} -> httpStatus) (\s@ListChannelsResponse' {} a -> s {httpStatus = a} :: ListChannelsResponse)

instance Prelude.NFData ListChannelsResponse where
  rnf ListChannelsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf channels
      `Prelude.seq` Prelude.rnf httpStatus
