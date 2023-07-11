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
-- Module      : Amazonka.ChimeSdkVoice.DeleteVoiceConnectorTermination
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- -- | Undocumented operation.
module Amazonka.ChimeSdkVoice.DeleteVoiceConnectorTermination
  ( -- * Creating a Request
    DeleteVoiceConnectorTermination (..),
    newDeleteVoiceConnectorTermination,

    -- * Request Lenses
    deleteVoiceConnectorTermination_voiceConnectorId,

    -- * Destructuring the Response
    DeleteVoiceConnectorTerminationResponse (..),
    newDeleteVoiceConnectorTerminationResponse,
  )
where

import Amazonka.ChimeSdkVoice.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteVoiceConnectorTermination' smart constructor.
data DeleteVoiceConnectorTermination = DeleteVoiceConnectorTermination'
  { voiceConnectorId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVoiceConnectorTermination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'voiceConnectorId', 'deleteVoiceConnectorTermination_voiceConnectorId' - Undocumented member.
newDeleteVoiceConnectorTermination ::
  -- | 'voiceConnectorId'
  Prelude.Text ->
  DeleteVoiceConnectorTermination
newDeleteVoiceConnectorTermination pVoiceConnectorId_ =
  DeleteVoiceConnectorTermination'
    { voiceConnectorId =
        pVoiceConnectorId_
    }

-- | Undocumented member.
deleteVoiceConnectorTermination_voiceConnectorId :: Lens.Lens' DeleteVoiceConnectorTermination Prelude.Text
deleteVoiceConnectorTermination_voiceConnectorId = Lens.lens (\DeleteVoiceConnectorTermination' {voiceConnectorId} -> voiceConnectorId) (\s@DeleteVoiceConnectorTermination' {} a -> s {voiceConnectorId = a} :: DeleteVoiceConnectorTermination)

instance
  Core.AWSRequest
    DeleteVoiceConnectorTermination
  where
  type
    AWSResponse DeleteVoiceConnectorTermination =
      DeleteVoiceConnectorTerminationResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull
      DeleteVoiceConnectorTerminationResponse'

instance
  Prelude.Hashable
    DeleteVoiceConnectorTermination
  where
  hashWithSalt
    _salt
    DeleteVoiceConnectorTermination' {..} =
      _salt `Prelude.hashWithSalt` voiceConnectorId

instance
  Prelude.NFData
    DeleteVoiceConnectorTermination
  where
  rnf DeleteVoiceConnectorTermination' {..} =
    Prelude.rnf voiceConnectorId

instance
  Data.ToHeaders
    DeleteVoiceConnectorTermination
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteVoiceConnectorTermination where
  toPath DeleteVoiceConnectorTermination' {..} =
    Prelude.mconcat
      [ "/voice-connectors/",
        Data.toBS voiceConnectorId,
        "/termination"
      ]

instance Data.ToQuery DeleteVoiceConnectorTermination where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteVoiceConnectorTerminationResponse' smart constructor.
data DeleteVoiceConnectorTerminationResponse = DeleteVoiceConnectorTerminationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVoiceConnectorTerminationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteVoiceConnectorTerminationResponse ::
  DeleteVoiceConnectorTerminationResponse
newDeleteVoiceConnectorTerminationResponse =
  DeleteVoiceConnectorTerminationResponse'

instance
  Prelude.NFData
    DeleteVoiceConnectorTerminationResponse
  where
  rnf _ = ()
