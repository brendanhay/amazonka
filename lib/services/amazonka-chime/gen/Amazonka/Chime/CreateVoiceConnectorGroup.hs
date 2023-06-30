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
-- Module      : Amazonka.Chime.CreateVoiceConnectorGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon Chime Voice Connector group under the administrator\'s
-- AWS account. You can associate Amazon Chime Voice Connectors with the
-- Amazon Chime Voice Connector group by including @VoiceConnectorItems@ in
-- the request.
--
-- You can include Amazon Chime Voice Connectors from different AWS Regions
-- in your group. This creates a fault tolerant mechanism for fallback in
-- case of availability events.
module Amazonka.Chime.CreateVoiceConnectorGroup
  ( -- * Creating a Request
    CreateVoiceConnectorGroup (..),
    newCreateVoiceConnectorGroup,

    -- * Request Lenses
    createVoiceConnectorGroup_voiceConnectorItems,
    createVoiceConnectorGroup_name,

    -- * Destructuring the Response
    CreateVoiceConnectorGroupResponse (..),
    newCreateVoiceConnectorGroupResponse,

    -- * Response Lenses
    createVoiceConnectorGroupResponse_voiceConnectorGroup,
    createVoiceConnectorGroupResponse_httpStatus,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateVoiceConnectorGroup' smart constructor.
data CreateVoiceConnectorGroup = CreateVoiceConnectorGroup'
  { -- | The Amazon Chime Voice Connectors to route inbound calls to.
    voiceConnectorItems :: Prelude.Maybe [VoiceConnectorItem],
    -- | The name of the Amazon Chime Voice Connector group.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVoiceConnectorGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'voiceConnectorItems', 'createVoiceConnectorGroup_voiceConnectorItems' - The Amazon Chime Voice Connectors to route inbound calls to.
--
-- 'name', 'createVoiceConnectorGroup_name' - The name of the Amazon Chime Voice Connector group.
newCreateVoiceConnectorGroup ::
  -- | 'name'
  Prelude.Text ->
  CreateVoiceConnectorGroup
newCreateVoiceConnectorGroup pName_ =
  CreateVoiceConnectorGroup'
    { voiceConnectorItems =
        Prelude.Nothing,
      name = pName_
    }

-- | The Amazon Chime Voice Connectors to route inbound calls to.
createVoiceConnectorGroup_voiceConnectorItems :: Lens.Lens' CreateVoiceConnectorGroup (Prelude.Maybe [VoiceConnectorItem])
createVoiceConnectorGroup_voiceConnectorItems = Lens.lens (\CreateVoiceConnectorGroup' {voiceConnectorItems} -> voiceConnectorItems) (\s@CreateVoiceConnectorGroup' {} a -> s {voiceConnectorItems = a} :: CreateVoiceConnectorGroup) Prelude.. Lens.mapping Lens.coerced

-- | The name of the Amazon Chime Voice Connector group.
createVoiceConnectorGroup_name :: Lens.Lens' CreateVoiceConnectorGroup Prelude.Text
createVoiceConnectorGroup_name = Lens.lens (\CreateVoiceConnectorGroup' {name} -> name) (\s@CreateVoiceConnectorGroup' {} a -> s {name = a} :: CreateVoiceConnectorGroup)

instance Core.AWSRequest CreateVoiceConnectorGroup where
  type
    AWSResponse CreateVoiceConnectorGroup =
      CreateVoiceConnectorGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateVoiceConnectorGroupResponse'
            Prelude.<$> (x Data..?> "VoiceConnectorGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateVoiceConnectorGroup where
  hashWithSalt _salt CreateVoiceConnectorGroup' {..} =
    _salt
      `Prelude.hashWithSalt` voiceConnectorItems
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateVoiceConnectorGroup where
  rnf CreateVoiceConnectorGroup' {..} =
    Prelude.rnf voiceConnectorItems
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders CreateVoiceConnectorGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON CreateVoiceConnectorGroup where
  toJSON CreateVoiceConnectorGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("VoiceConnectorItems" Data..=)
              Prelude.<$> voiceConnectorItems,
            Prelude.Just ("Name" Data..= name)
          ]
      )

instance Data.ToPath CreateVoiceConnectorGroup where
  toPath = Prelude.const "/voice-connector-groups"

instance Data.ToQuery CreateVoiceConnectorGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateVoiceConnectorGroupResponse' smart constructor.
data CreateVoiceConnectorGroupResponse = CreateVoiceConnectorGroupResponse'
  { -- | The Amazon Chime Voice Connector group details.
    voiceConnectorGroup :: Prelude.Maybe VoiceConnectorGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVoiceConnectorGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'voiceConnectorGroup', 'createVoiceConnectorGroupResponse_voiceConnectorGroup' - The Amazon Chime Voice Connector group details.
--
-- 'httpStatus', 'createVoiceConnectorGroupResponse_httpStatus' - The response's http status code.
newCreateVoiceConnectorGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateVoiceConnectorGroupResponse
newCreateVoiceConnectorGroupResponse pHttpStatus_ =
  CreateVoiceConnectorGroupResponse'
    { voiceConnectorGroup =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Chime Voice Connector group details.
createVoiceConnectorGroupResponse_voiceConnectorGroup :: Lens.Lens' CreateVoiceConnectorGroupResponse (Prelude.Maybe VoiceConnectorGroup)
createVoiceConnectorGroupResponse_voiceConnectorGroup = Lens.lens (\CreateVoiceConnectorGroupResponse' {voiceConnectorGroup} -> voiceConnectorGroup) (\s@CreateVoiceConnectorGroupResponse' {} a -> s {voiceConnectorGroup = a} :: CreateVoiceConnectorGroupResponse)

-- | The response's http status code.
createVoiceConnectorGroupResponse_httpStatus :: Lens.Lens' CreateVoiceConnectorGroupResponse Prelude.Int
createVoiceConnectorGroupResponse_httpStatus = Lens.lens (\CreateVoiceConnectorGroupResponse' {httpStatus} -> httpStatus) (\s@CreateVoiceConnectorGroupResponse' {} a -> s {httpStatus = a} :: CreateVoiceConnectorGroupResponse)

instance
  Prelude.NFData
    CreateVoiceConnectorGroupResponse
  where
  rnf CreateVoiceConnectorGroupResponse' {..} =
    Prelude.rnf voiceConnectorGroup
      `Prelude.seq` Prelude.rnf httpStatus
