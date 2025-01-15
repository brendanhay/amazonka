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
-- Module      : Amazonka.ChimeSdkVoice.ListAvailableVoiceConnectorRegions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- -- | Undocumented operation.
module Amazonka.ChimeSdkVoice.ListAvailableVoiceConnectorRegions
  ( -- * Creating a Request
    ListAvailableVoiceConnectorRegions (..),
    newListAvailableVoiceConnectorRegions,

    -- * Destructuring the Response
    ListAvailableVoiceConnectorRegionsResponse (..),
    newListAvailableVoiceConnectorRegionsResponse,

    -- * Response Lenses
    listAvailableVoiceConnectorRegionsResponse_voiceConnectorRegions,
    listAvailableVoiceConnectorRegionsResponse_httpStatus,
  )
where

import Amazonka.ChimeSdkVoice.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAvailableVoiceConnectorRegions' smart constructor.
data ListAvailableVoiceConnectorRegions = ListAvailableVoiceConnectorRegions'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAvailableVoiceConnectorRegions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newListAvailableVoiceConnectorRegions ::
  ListAvailableVoiceConnectorRegions
newListAvailableVoiceConnectorRegions =
  ListAvailableVoiceConnectorRegions'

instance
  Core.AWSRequest
    ListAvailableVoiceConnectorRegions
  where
  type
    AWSResponse ListAvailableVoiceConnectorRegions =
      ListAvailableVoiceConnectorRegionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAvailableVoiceConnectorRegionsResponse'
            Prelude.<$> ( x
                            Data..?> "VoiceConnectorRegions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListAvailableVoiceConnectorRegions
  where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance
  Prelude.NFData
    ListAvailableVoiceConnectorRegions
  where
  rnf _ = ()

instance
  Data.ToHeaders
    ListAvailableVoiceConnectorRegions
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    ListAvailableVoiceConnectorRegions
  where
  toPath = Prelude.const "/voice-connector-regions"

instance
  Data.ToQuery
    ListAvailableVoiceConnectorRegions
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAvailableVoiceConnectorRegionsResponse' smart constructor.
data ListAvailableVoiceConnectorRegionsResponse = ListAvailableVoiceConnectorRegionsResponse'
  { voiceConnectorRegions :: Prelude.Maybe [VoiceConnectorAwsRegion],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAvailableVoiceConnectorRegionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'voiceConnectorRegions', 'listAvailableVoiceConnectorRegionsResponse_voiceConnectorRegions' - Undocumented member.
--
-- 'httpStatus', 'listAvailableVoiceConnectorRegionsResponse_httpStatus' - The response's http status code.
newListAvailableVoiceConnectorRegionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAvailableVoiceConnectorRegionsResponse
newListAvailableVoiceConnectorRegionsResponse
  pHttpStatus_ =
    ListAvailableVoiceConnectorRegionsResponse'
      { voiceConnectorRegions =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Undocumented member.
listAvailableVoiceConnectorRegionsResponse_voiceConnectorRegions :: Lens.Lens' ListAvailableVoiceConnectorRegionsResponse (Prelude.Maybe [VoiceConnectorAwsRegion])
listAvailableVoiceConnectorRegionsResponse_voiceConnectorRegions = Lens.lens (\ListAvailableVoiceConnectorRegionsResponse' {voiceConnectorRegions} -> voiceConnectorRegions) (\s@ListAvailableVoiceConnectorRegionsResponse' {} a -> s {voiceConnectorRegions = a} :: ListAvailableVoiceConnectorRegionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listAvailableVoiceConnectorRegionsResponse_httpStatus :: Lens.Lens' ListAvailableVoiceConnectorRegionsResponse Prelude.Int
listAvailableVoiceConnectorRegionsResponse_httpStatus = Lens.lens (\ListAvailableVoiceConnectorRegionsResponse' {httpStatus} -> httpStatus) (\s@ListAvailableVoiceConnectorRegionsResponse' {} a -> s {httpStatus = a} :: ListAvailableVoiceConnectorRegionsResponse)

instance
  Prelude.NFData
    ListAvailableVoiceConnectorRegionsResponse
  where
  rnf ListAvailableVoiceConnectorRegionsResponse' {..} =
    Prelude.rnf voiceConnectorRegions `Prelude.seq`
      Prelude.rnf httpStatus
