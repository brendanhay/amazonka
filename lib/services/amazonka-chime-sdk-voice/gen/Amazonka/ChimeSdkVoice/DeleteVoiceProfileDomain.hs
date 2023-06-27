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
-- Module      : Amazonka.ChimeSdkVoice.DeleteVoiceProfileDomain
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes all voice profiles in the domain. WARNING: This action is not
-- reversible.
module Amazonka.ChimeSdkVoice.DeleteVoiceProfileDomain
  ( -- * Creating a Request
    DeleteVoiceProfileDomain (..),
    newDeleteVoiceProfileDomain,

    -- * Request Lenses
    deleteVoiceProfileDomain_voiceProfileDomainId,

    -- * Destructuring the Response
    DeleteVoiceProfileDomainResponse (..),
    newDeleteVoiceProfileDomainResponse,
  )
where

import Amazonka.ChimeSdkVoice.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteVoiceProfileDomain' smart constructor.
data DeleteVoiceProfileDomain = DeleteVoiceProfileDomain'
  { -- | The voice profile domain ID.
    voiceProfileDomainId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVoiceProfileDomain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'voiceProfileDomainId', 'deleteVoiceProfileDomain_voiceProfileDomainId' - The voice profile domain ID.
newDeleteVoiceProfileDomain ::
  -- | 'voiceProfileDomainId'
  Prelude.Text ->
  DeleteVoiceProfileDomain
newDeleteVoiceProfileDomain pVoiceProfileDomainId_ =
  DeleteVoiceProfileDomain'
    { voiceProfileDomainId =
        pVoiceProfileDomainId_
    }

-- | The voice profile domain ID.
deleteVoiceProfileDomain_voiceProfileDomainId :: Lens.Lens' DeleteVoiceProfileDomain Prelude.Text
deleteVoiceProfileDomain_voiceProfileDomainId = Lens.lens (\DeleteVoiceProfileDomain' {voiceProfileDomainId} -> voiceProfileDomainId) (\s@DeleteVoiceProfileDomain' {} a -> s {voiceProfileDomainId = a} :: DeleteVoiceProfileDomain)

instance Core.AWSRequest DeleteVoiceProfileDomain where
  type
    AWSResponse DeleteVoiceProfileDomain =
      DeleteVoiceProfileDomainResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull
      DeleteVoiceProfileDomainResponse'

instance Prelude.Hashable DeleteVoiceProfileDomain where
  hashWithSalt _salt DeleteVoiceProfileDomain' {..} =
    _salt `Prelude.hashWithSalt` voiceProfileDomainId

instance Prelude.NFData DeleteVoiceProfileDomain where
  rnf DeleteVoiceProfileDomain' {..} =
    Prelude.rnf voiceProfileDomainId

instance Data.ToHeaders DeleteVoiceProfileDomain where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteVoiceProfileDomain where
  toPath DeleteVoiceProfileDomain' {..} =
    Prelude.mconcat
      [ "/voice-profile-domains/",
        Data.toBS voiceProfileDomainId
      ]

instance Data.ToQuery DeleteVoiceProfileDomain where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteVoiceProfileDomainResponse' smart constructor.
data DeleteVoiceProfileDomainResponse = DeleteVoiceProfileDomainResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVoiceProfileDomainResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteVoiceProfileDomainResponse ::
  DeleteVoiceProfileDomainResponse
newDeleteVoiceProfileDomainResponse =
  DeleteVoiceProfileDomainResponse'

instance
  Prelude.NFData
    DeleteVoiceProfileDomainResponse
  where
  rnf _ = ()
