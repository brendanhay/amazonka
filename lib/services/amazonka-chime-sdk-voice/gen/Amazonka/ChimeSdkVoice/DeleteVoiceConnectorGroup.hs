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
-- Module      : Amazonka.ChimeSdkVoice.DeleteVoiceConnectorGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- -- | Undocumented operation.
module Amazonka.ChimeSdkVoice.DeleteVoiceConnectorGroup
  ( -- * Creating a Request
    DeleteVoiceConnectorGroup (..),
    newDeleteVoiceConnectorGroup,

    -- * Request Lenses
    deleteVoiceConnectorGroup_voiceConnectorGroupId,

    -- * Destructuring the Response
    DeleteVoiceConnectorGroupResponse (..),
    newDeleteVoiceConnectorGroupResponse,
  )
where

import Amazonka.ChimeSdkVoice.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteVoiceConnectorGroup' smart constructor.
data DeleteVoiceConnectorGroup = DeleteVoiceConnectorGroup'
  { voiceConnectorGroupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVoiceConnectorGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'voiceConnectorGroupId', 'deleteVoiceConnectorGroup_voiceConnectorGroupId' - Undocumented member.
newDeleteVoiceConnectorGroup ::
  -- | 'voiceConnectorGroupId'
  Prelude.Text ->
  DeleteVoiceConnectorGroup
newDeleteVoiceConnectorGroup pVoiceConnectorGroupId_ =
  DeleteVoiceConnectorGroup'
    { voiceConnectorGroupId =
        pVoiceConnectorGroupId_
    }

-- | Undocumented member.
deleteVoiceConnectorGroup_voiceConnectorGroupId :: Lens.Lens' DeleteVoiceConnectorGroup Prelude.Text
deleteVoiceConnectorGroup_voiceConnectorGroupId = Lens.lens (\DeleteVoiceConnectorGroup' {voiceConnectorGroupId} -> voiceConnectorGroupId) (\s@DeleteVoiceConnectorGroup' {} a -> s {voiceConnectorGroupId = a} :: DeleteVoiceConnectorGroup)

instance Core.AWSRequest DeleteVoiceConnectorGroup where
  type
    AWSResponse DeleteVoiceConnectorGroup =
      DeleteVoiceConnectorGroupResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull
      DeleteVoiceConnectorGroupResponse'

instance Prelude.Hashable DeleteVoiceConnectorGroup where
  hashWithSalt _salt DeleteVoiceConnectorGroup' {..} =
    _salt `Prelude.hashWithSalt` voiceConnectorGroupId

instance Prelude.NFData DeleteVoiceConnectorGroup where
  rnf DeleteVoiceConnectorGroup' {..} =
    Prelude.rnf voiceConnectorGroupId

instance Data.ToHeaders DeleteVoiceConnectorGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteVoiceConnectorGroup where
  toPath DeleteVoiceConnectorGroup' {..} =
    Prelude.mconcat
      [ "/voice-connector-groups/",
        Data.toBS voiceConnectorGroupId
      ]

instance Data.ToQuery DeleteVoiceConnectorGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteVoiceConnectorGroupResponse' smart constructor.
data DeleteVoiceConnectorGroupResponse = DeleteVoiceConnectorGroupResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVoiceConnectorGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteVoiceConnectorGroupResponse ::
  DeleteVoiceConnectorGroupResponse
newDeleteVoiceConnectorGroupResponse =
  DeleteVoiceConnectorGroupResponse'

instance
  Prelude.NFData
    DeleteVoiceConnectorGroupResponse
  where
  rnf _ = ()
