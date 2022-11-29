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
-- Module      : Amazonka.Chime.PutVoiceConnectorOrigination
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds origination settings for the specified Amazon Chime Voice
-- Connector.
--
-- If emergency calling is configured for the Amazon Chime Voice Connector,
-- it must be deleted prior to turning off origination settings.
module Amazonka.Chime.PutVoiceConnectorOrigination
  ( -- * Creating a Request
    PutVoiceConnectorOrigination (..),
    newPutVoiceConnectorOrigination,

    -- * Request Lenses
    putVoiceConnectorOrigination_voiceConnectorId,
    putVoiceConnectorOrigination_origination,

    -- * Destructuring the Response
    PutVoiceConnectorOriginationResponse (..),
    newPutVoiceConnectorOriginationResponse,

    -- * Response Lenses
    putVoiceConnectorOriginationResponse_origination,
    putVoiceConnectorOriginationResponse_httpStatus,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutVoiceConnectorOrigination' smart constructor.
data PutVoiceConnectorOrigination = PutVoiceConnectorOrigination'
  { -- | The Amazon Chime Voice Connector ID.
    voiceConnectorId :: Prelude.Text,
    -- | The origination setting details to add.
    origination :: Origination
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutVoiceConnectorOrigination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'voiceConnectorId', 'putVoiceConnectorOrigination_voiceConnectorId' - The Amazon Chime Voice Connector ID.
--
-- 'origination', 'putVoiceConnectorOrigination_origination' - The origination setting details to add.
newPutVoiceConnectorOrigination ::
  -- | 'voiceConnectorId'
  Prelude.Text ->
  -- | 'origination'
  Origination ->
  PutVoiceConnectorOrigination
newPutVoiceConnectorOrigination
  pVoiceConnectorId_
  pOrigination_ =
    PutVoiceConnectorOrigination'
      { voiceConnectorId =
          pVoiceConnectorId_,
        origination = pOrigination_
      }

-- | The Amazon Chime Voice Connector ID.
putVoiceConnectorOrigination_voiceConnectorId :: Lens.Lens' PutVoiceConnectorOrigination Prelude.Text
putVoiceConnectorOrigination_voiceConnectorId = Lens.lens (\PutVoiceConnectorOrigination' {voiceConnectorId} -> voiceConnectorId) (\s@PutVoiceConnectorOrigination' {} a -> s {voiceConnectorId = a} :: PutVoiceConnectorOrigination)

-- | The origination setting details to add.
putVoiceConnectorOrigination_origination :: Lens.Lens' PutVoiceConnectorOrigination Origination
putVoiceConnectorOrigination_origination = Lens.lens (\PutVoiceConnectorOrigination' {origination} -> origination) (\s@PutVoiceConnectorOrigination' {} a -> s {origination = a} :: PutVoiceConnectorOrigination)

instance Core.AWSRequest PutVoiceConnectorOrigination where
  type
    AWSResponse PutVoiceConnectorOrigination =
      PutVoiceConnectorOriginationResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutVoiceConnectorOriginationResponse'
            Prelude.<$> (x Core..?> "Origination")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    PutVoiceConnectorOrigination
  where
  hashWithSalt _salt PutVoiceConnectorOrigination' {..} =
    _salt `Prelude.hashWithSalt` voiceConnectorId
      `Prelude.hashWithSalt` origination

instance Prelude.NFData PutVoiceConnectorOrigination where
  rnf PutVoiceConnectorOrigination' {..} =
    Prelude.rnf voiceConnectorId
      `Prelude.seq` Prelude.rnf origination

instance Core.ToHeaders PutVoiceConnectorOrigination where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON PutVoiceConnectorOrigination where
  toJSON PutVoiceConnectorOrigination' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("Origination" Core..= origination)]
      )

instance Core.ToPath PutVoiceConnectorOrigination where
  toPath PutVoiceConnectorOrigination' {..} =
    Prelude.mconcat
      [ "/voice-connectors/",
        Core.toBS voiceConnectorId,
        "/origination"
      ]

instance Core.ToQuery PutVoiceConnectorOrigination where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutVoiceConnectorOriginationResponse' smart constructor.
data PutVoiceConnectorOriginationResponse = PutVoiceConnectorOriginationResponse'
  { -- | The updated origination setting details.
    origination :: Prelude.Maybe Origination,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutVoiceConnectorOriginationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'origination', 'putVoiceConnectorOriginationResponse_origination' - The updated origination setting details.
--
-- 'httpStatus', 'putVoiceConnectorOriginationResponse_httpStatus' - The response's http status code.
newPutVoiceConnectorOriginationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutVoiceConnectorOriginationResponse
newPutVoiceConnectorOriginationResponse pHttpStatus_ =
  PutVoiceConnectorOriginationResponse'
    { origination =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The updated origination setting details.
putVoiceConnectorOriginationResponse_origination :: Lens.Lens' PutVoiceConnectorOriginationResponse (Prelude.Maybe Origination)
putVoiceConnectorOriginationResponse_origination = Lens.lens (\PutVoiceConnectorOriginationResponse' {origination} -> origination) (\s@PutVoiceConnectorOriginationResponse' {} a -> s {origination = a} :: PutVoiceConnectorOriginationResponse)

-- | The response's http status code.
putVoiceConnectorOriginationResponse_httpStatus :: Lens.Lens' PutVoiceConnectorOriginationResponse Prelude.Int
putVoiceConnectorOriginationResponse_httpStatus = Lens.lens (\PutVoiceConnectorOriginationResponse' {httpStatus} -> httpStatus) (\s@PutVoiceConnectorOriginationResponse' {} a -> s {httpStatus = a} :: PutVoiceConnectorOriginationResponse)

instance
  Prelude.NFData
    PutVoiceConnectorOriginationResponse
  where
  rnf PutVoiceConnectorOriginationResponse' {..} =
    Prelude.rnf origination
      `Prelude.seq` Prelude.rnf httpStatus
