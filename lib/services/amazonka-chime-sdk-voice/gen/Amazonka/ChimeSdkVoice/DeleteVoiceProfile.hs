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
-- Module      : Amazonka.ChimeSdkVoice.DeleteVoiceProfile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a voice profile, including its voice print and enrollment data.
-- WARNING: This action is not reversible.
module Amazonka.ChimeSdkVoice.DeleteVoiceProfile
  ( -- * Creating a Request
    DeleteVoiceProfile (..),
    newDeleteVoiceProfile,

    -- * Request Lenses
    deleteVoiceProfile_voiceProfileId,

    -- * Destructuring the Response
    DeleteVoiceProfileResponse (..),
    newDeleteVoiceProfileResponse,
  )
where

import Amazonka.ChimeSdkVoice.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteVoiceProfile' smart constructor.
data DeleteVoiceProfile = DeleteVoiceProfile'
  { -- | The voice profile ID.
    voiceProfileId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVoiceProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'voiceProfileId', 'deleteVoiceProfile_voiceProfileId' - The voice profile ID.
newDeleteVoiceProfile ::
  -- | 'voiceProfileId'
  Prelude.Text ->
  DeleteVoiceProfile
newDeleteVoiceProfile pVoiceProfileId_ =
  DeleteVoiceProfile'
    { voiceProfileId =
        pVoiceProfileId_
    }

-- | The voice profile ID.
deleteVoiceProfile_voiceProfileId :: Lens.Lens' DeleteVoiceProfile Prelude.Text
deleteVoiceProfile_voiceProfileId = Lens.lens (\DeleteVoiceProfile' {voiceProfileId} -> voiceProfileId) (\s@DeleteVoiceProfile' {} a -> s {voiceProfileId = a} :: DeleteVoiceProfile)

instance Core.AWSRequest DeleteVoiceProfile where
  type
    AWSResponse DeleteVoiceProfile =
      DeleteVoiceProfileResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteVoiceProfileResponse'

instance Prelude.Hashable DeleteVoiceProfile where
  hashWithSalt _salt DeleteVoiceProfile' {..} =
    _salt `Prelude.hashWithSalt` voiceProfileId

instance Prelude.NFData DeleteVoiceProfile where
  rnf DeleteVoiceProfile' {..} =
    Prelude.rnf voiceProfileId

instance Data.ToHeaders DeleteVoiceProfile where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteVoiceProfile where
  toPath DeleteVoiceProfile' {..} =
    Prelude.mconcat
      ["/voice-profiles/", Data.toBS voiceProfileId]

instance Data.ToQuery DeleteVoiceProfile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteVoiceProfileResponse' smart constructor.
data DeleteVoiceProfileResponse = DeleteVoiceProfileResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVoiceProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteVoiceProfileResponse ::
  DeleteVoiceProfileResponse
newDeleteVoiceProfileResponse =
  DeleteVoiceProfileResponse'

instance Prelude.NFData DeleteVoiceProfileResponse where
  rnf _ = ()
