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
-- Module      : Amazonka.Chime.ListVoiceConnectors
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the Amazon Chime Voice Connectors for the administrator\'s AWS
-- account.
module Amazonka.Chime.ListVoiceConnectors
  ( -- * Creating a Request
    ListVoiceConnectors (..),
    newListVoiceConnectors,

    -- * Request Lenses
    listVoiceConnectors_maxResults,
    listVoiceConnectors_nextToken,

    -- * Destructuring the Response
    ListVoiceConnectorsResponse (..),
    newListVoiceConnectorsResponse,

    -- * Response Lenses
    listVoiceConnectorsResponse_nextToken,
    listVoiceConnectorsResponse_voiceConnectors,
    listVoiceConnectorsResponse_httpStatus,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListVoiceConnectors' smart constructor.
data ListVoiceConnectors = ListVoiceConnectors'
  { -- | The maximum number of results to return in a single call.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token to use to retrieve the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListVoiceConnectors' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listVoiceConnectors_maxResults' - The maximum number of results to return in a single call.
--
-- 'nextToken', 'listVoiceConnectors_nextToken' - The token to use to retrieve the next page of results.
newListVoiceConnectors ::
  ListVoiceConnectors
newListVoiceConnectors =
  ListVoiceConnectors'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of results to return in a single call.
listVoiceConnectors_maxResults :: Lens.Lens' ListVoiceConnectors (Prelude.Maybe Prelude.Natural)
listVoiceConnectors_maxResults = Lens.lens (\ListVoiceConnectors' {maxResults} -> maxResults) (\s@ListVoiceConnectors' {} a -> s {maxResults = a} :: ListVoiceConnectors)

-- | The token to use to retrieve the next page of results.
listVoiceConnectors_nextToken :: Lens.Lens' ListVoiceConnectors (Prelude.Maybe Prelude.Text)
listVoiceConnectors_nextToken = Lens.lens (\ListVoiceConnectors' {nextToken} -> nextToken) (\s@ListVoiceConnectors' {} a -> s {nextToken = a} :: ListVoiceConnectors)

instance Core.AWSRequest ListVoiceConnectors where
  type
    AWSResponse ListVoiceConnectors =
      ListVoiceConnectorsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListVoiceConnectorsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x
                            Data..?> "VoiceConnectors"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListVoiceConnectors where
  hashWithSalt _salt ListVoiceConnectors' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListVoiceConnectors where
  rnf ListVoiceConnectors' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListVoiceConnectors where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListVoiceConnectors where
  toPath = Prelude.const "/voice-connectors"

instance Data.ToQuery ListVoiceConnectors where
  toQuery ListVoiceConnectors' {..} =
    Prelude.mconcat
      [ "max-results" Data.=: maxResults,
        "next-token" Data.=: nextToken
      ]

-- | /See:/ 'newListVoiceConnectorsResponse' smart constructor.
data ListVoiceConnectorsResponse = ListVoiceConnectorsResponse'
  { -- | The token to use to retrieve the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The details of the Amazon Chime Voice Connectors.
    voiceConnectors :: Prelude.Maybe [VoiceConnector],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListVoiceConnectorsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listVoiceConnectorsResponse_nextToken' - The token to use to retrieve the next page of results.
--
-- 'voiceConnectors', 'listVoiceConnectorsResponse_voiceConnectors' - The details of the Amazon Chime Voice Connectors.
--
-- 'httpStatus', 'listVoiceConnectorsResponse_httpStatus' - The response's http status code.
newListVoiceConnectorsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListVoiceConnectorsResponse
newListVoiceConnectorsResponse pHttpStatus_ =
  ListVoiceConnectorsResponse'
    { nextToken =
        Prelude.Nothing,
      voiceConnectors = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results.
listVoiceConnectorsResponse_nextToken :: Lens.Lens' ListVoiceConnectorsResponse (Prelude.Maybe Prelude.Text)
listVoiceConnectorsResponse_nextToken = Lens.lens (\ListVoiceConnectorsResponse' {nextToken} -> nextToken) (\s@ListVoiceConnectorsResponse' {} a -> s {nextToken = a} :: ListVoiceConnectorsResponse)

-- | The details of the Amazon Chime Voice Connectors.
listVoiceConnectorsResponse_voiceConnectors :: Lens.Lens' ListVoiceConnectorsResponse (Prelude.Maybe [VoiceConnector])
listVoiceConnectorsResponse_voiceConnectors = Lens.lens (\ListVoiceConnectorsResponse' {voiceConnectors} -> voiceConnectors) (\s@ListVoiceConnectorsResponse' {} a -> s {voiceConnectors = a} :: ListVoiceConnectorsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listVoiceConnectorsResponse_httpStatus :: Lens.Lens' ListVoiceConnectorsResponse Prelude.Int
listVoiceConnectorsResponse_httpStatus = Lens.lens (\ListVoiceConnectorsResponse' {httpStatus} -> httpStatus) (\s@ListVoiceConnectorsResponse' {} a -> s {httpStatus = a} :: ListVoiceConnectorsResponse)

instance Prelude.NFData ListVoiceConnectorsResponse where
  rnf ListVoiceConnectorsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf voiceConnectors
      `Prelude.seq` Prelude.rnf httpStatus
