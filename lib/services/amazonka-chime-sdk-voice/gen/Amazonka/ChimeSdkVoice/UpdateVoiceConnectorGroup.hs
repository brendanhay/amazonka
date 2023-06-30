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
-- Module      : Amazonka.ChimeSdkVoice.UpdateVoiceConnectorGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- -- | Undocumented operation.
module Amazonka.ChimeSdkVoice.UpdateVoiceConnectorGroup
  ( -- * Creating a Request
    UpdateVoiceConnectorGroup (..),
    newUpdateVoiceConnectorGroup,

    -- * Request Lenses
    updateVoiceConnectorGroup_voiceConnectorGroupId,
    updateVoiceConnectorGroup_name,
    updateVoiceConnectorGroup_voiceConnectorItems,

    -- * Destructuring the Response
    UpdateVoiceConnectorGroupResponse (..),
    newUpdateVoiceConnectorGroupResponse,

    -- * Response Lenses
    updateVoiceConnectorGroupResponse_voiceConnectorGroup,
    updateVoiceConnectorGroupResponse_httpStatus,
  )
where

import Amazonka.ChimeSdkVoice.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateVoiceConnectorGroup' smart constructor.
data UpdateVoiceConnectorGroup = UpdateVoiceConnectorGroup'
  { voiceConnectorGroupId :: Prelude.Text,
    name :: Prelude.Text,
    voiceConnectorItems :: [VoiceConnectorItem]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateVoiceConnectorGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'voiceConnectorGroupId', 'updateVoiceConnectorGroup_voiceConnectorGroupId' - Undocumented member.
--
-- 'name', 'updateVoiceConnectorGroup_name' - Undocumented member.
--
-- 'voiceConnectorItems', 'updateVoiceConnectorGroup_voiceConnectorItems' - Undocumented member.
newUpdateVoiceConnectorGroup ::
  -- | 'voiceConnectorGroupId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  UpdateVoiceConnectorGroup
newUpdateVoiceConnectorGroup
  pVoiceConnectorGroupId_
  pName_ =
    UpdateVoiceConnectorGroup'
      { voiceConnectorGroupId =
          pVoiceConnectorGroupId_,
        name = pName_,
        voiceConnectorItems = Prelude.mempty
      }

-- | Undocumented member.
updateVoiceConnectorGroup_voiceConnectorGroupId :: Lens.Lens' UpdateVoiceConnectorGroup Prelude.Text
updateVoiceConnectorGroup_voiceConnectorGroupId = Lens.lens (\UpdateVoiceConnectorGroup' {voiceConnectorGroupId} -> voiceConnectorGroupId) (\s@UpdateVoiceConnectorGroup' {} a -> s {voiceConnectorGroupId = a} :: UpdateVoiceConnectorGroup)

-- | Undocumented member.
updateVoiceConnectorGroup_name :: Lens.Lens' UpdateVoiceConnectorGroup Prelude.Text
updateVoiceConnectorGroup_name = Lens.lens (\UpdateVoiceConnectorGroup' {name} -> name) (\s@UpdateVoiceConnectorGroup' {} a -> s {name = a} :: UpdateVoiceConnectorGroup)

-- | Undocumented member.
updateVoiceConnectorGroup_voiceConnectorItems :: Lens.Lens' UpdateVoiceConnectorGroup [VoiceConnectorItem]
updateVoiceConnectorGroup_voiceConnectorItems = Lens.lens (\UpdateVoiceConnectorGroup' {voiceConnectorItems} -> voiceConnectorItems) (\s@UpdateVoiceConnectorGroup' {} a -> s {voiceConnectorItems = a} :: UpdateVoiceConnectorGroup) Prelude.. Lens.coerced

instance Core.AWSRequest UpdateVoiceConnectorGroup where
  type
    AWSResponse UpdateVoiceConnectorGroup =
      UpdateVoiceConnectorGroupResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateVoiceConnectorGroupResponse'
            Prelude.<$> (x Data..?> "VoiceConnectorGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateVoiceConnectorGroup where
  hashWithSalt _salt UpdateVoiceConnectorGroup' {..} =
    _salt
      `Prelude.hashWithSalt` voiceConnectorGroupId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` voiceConnectorItems

instance Prelude.NFData UpdateVoiceConnectorGroup where
  rnf UpdateVoiceConnectorGroup' {..} =
    Prelude.rnf voiceConnectorGroupId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf voiceConnectorItems

instance Data.ToHeaders UpdateVoiceConnectorGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON UpdateVoiceConnectorGroup where
  toJSON UpdateVoiceConnectorGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just
              ("VoiceConnectorItems" Data..= voiceConnectorItems)
          ]
      )

instance Data.ToPath UpdateVoiceConnectorGroup where
  toPath UpdateVoiceConnectorGroup' {..} =
    Prelude.mconcat
      [ "/voice-connector-groups/",
        Data.toBS voiceConnectorGroupId
      ]

instance Data.ToQuery UpdateVoiceConnectorGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateVoiceConnectorGroupResponse' smart constructor.
data UpdateVoiceConnectorGroupResponse = UpdateVoiceConnectorGroupResponse'
  { voiceConnectorGroup :: Prelude.Maybe VoiceConnectorGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateVoiceConnectorGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'voiceConnectorGroup', 'updateVoiceConnectorGroupResponse_voiceConnectorGroup' - Undocumented member.
--
-- 'httpStatus', 'updateVoiceConnectorGroupResponse_httpStatus' - The response's http status code.
newUpdateVoiceConnectorGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateVoiceConnectorGroupResponse
newUpdateVoiceConnectorGroupResponse pHttpStatus_ =
  UpdateVoiceConnectorGroupResponse'
    { voiceConnectorGroup =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
updateVoiceConnectorGroupResponse_voiceConnectorGroup :: Lens.Lens' UpdateVoiceConnectorGroupResponse (Prelude.Maybe VoiceConnectorGroup)
updateVoiceConnectorGroupResponse_voiceConnectorGroup = Lens.lens (\UpdateVoiceConnectorGroupResponse' {voiceConnectorGroup} -> voiceConnectorGroup) (\s@UpdateVoiceConnectorGroupResponse' {} a -> s {voiceConnectorGroup = a} :: UpdateVoiceConnectorGroupResponse)

-- | The response's http status code.
updateVoiceConnectorGroupResponse_httpStatus :: Lens.Lens' UpdateVoiceConnectorGroupResponse Prelude.Int
updateVoiceConnectorGroupResponse_httpStatus = Lens.lens (\UpdateVoiceConnectorGroupResponse' {httpStatus} -> httpStatus) (\s@UpdateVoiceConnectorGroupResponse' {} a -> s {httpStatus = a} :: UpdateVoiceConnectorGroupResponse)

instance
  Prelude.NFData
    UpdateVoiceConnectorGroupResponse
  where
  rnf UpdateVoiceConnectorGroupResponse' {..} =
    Prelude.rnf voiceConnectorGroup
      `Prelude.seq` Prelude.rnf httpStatus
