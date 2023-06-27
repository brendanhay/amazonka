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
-- Module      : Amazonka.ChimeSdkVoice.UpdateVoiceProfileDomain
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the settings for the specified voice profile domain.
module Amazonka.ChimeSdkVoice.UpdateVoiceProfileDomain
  ( -- * Creating a Request
    UpdateVoiceProfileDomain (..),
    newUpdateVoiceProfileDomain,

    -- * Request Lenses
    updateVoiceProfileDomain_description,
    updateVoiceProfileDomain_name,
    updateVoiceProfileDomain_voiceProfileDomainId,

    -- * Destructuring the Response
    UpdateVoiceProfileDomainResponse (..),
    newUpdateVoiceProfileDomainResponse,

    -- * Response Lenses
    updateVoiceProfileDomainResponse_voiceProfileDomain,
    updateVoiceProfileDomainResponse_httpStatus,
  )
where

import Amazonka.ChimeSdkVoice.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateVoiceProfileDomain' smart constructor.
data UpdateVoiceProfileDomain = UpdateVoiceProfileDomain'
  { -- | The description of the voice profile domain.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the voice profile domain.
    name :: Prelude.Maybe Prelude.Text,
    -- | The domain ID.
    voiceProfileDomainId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateVoiceProfileDomain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateVoiceProfileDomain_description' - The description of the voice profile domain.
--
-- 'name', 'updateVoiceProfileDomain_name' - The name of the voice profile domain.
--
-- 'voiceProfileDomainId', 'updateVoiceProfileDomain_voiceProfileDomainId' - The domain ID.
newUpdateVoiceProfileDomain ::
  -- | 'voiceProfileDomainId'
  Prelude.Text ->
  UpdateVoiceProfileDomain
newUpdateVoiceProfileDomain pVoiceProfileDomainId_ =
  UpdateVoiceProfileDomain'
    { description =
        Prelude.Nothing,
      name = Prelude.Nothing,
      voiceProfileDomainId = pVoiceProfileDomainId_
    }

-- | The description of the voice profile domain.
updateVoiceProfileDomain_description :: Lens.Lens' UpdateVoiceProfileDomain (Prelude.Maybe Prelude.Text)
updateVoiceProfileDomain_description = Lens.lens (\UpdateVoiceProfileDomain' {description} -> description) (\s@UpdateVoiceProfileDomain' {} a -> s {description = a} :: UpdateVoiceProfileDomain)

-- | The name of the voice profile domain.
updateVoiceProfileDomain_name :: Lens.Lens' UpdateVoiceProfileDomain (Prelude.Maybe Prelude.Text)
updateVoiceProfileDomain_name = Lens.lens (\UpdateVoiceProfileDomain' {name} -> name) (\s@UpdateVoiceProfileDomain' {} a -> s {name = a} :: UpdateVoiceProfileDomain)

-- | The domain ID.
updateVoiceProfileDomain_voiceProfileDomainId :: Lens.Lens' UpdateVoiceProfileDomain Prelude.Text
updateVoiceProfileDomain_voiceProfileDomainId = Lens.lens (\UpdateVoiceProfileDomain' {voiceProfileDomainId} -> voiceProfileDomainId) (\s@UpdateVoiceProfileDomain' {} a -> s {voiceProfileDomainId = a} :: UpdateVoiceProfileDomain)

instance Core.AWSRequest UpdateVoiceProfileDomain where
  type
    AWSResponse UpdateVoiceProfileDomain =
      UpdateVoiceProfileDomainResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateVoiceProfileDomainResponse'
            Prelude.<$> (x Data..?> "VoiceProfileDomain")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateVoiceProfileDomain where
  hashWithSalt _salt UpdateVoiceProfileDomain' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` voiceProfileDomainId

instance Prelude.NFData UpdateVoiceProfileDomain where
  rnf UpdateVoiceProfileDomain' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf voiceProfileDomainId

instance Data.ToHeaders UpdateVoiceProfileDomain where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON UpdateVoiceProfileDomain where
  toJSON UpdateVoiceProfileDomain' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("Name" Data..=) Prelude.<$> name
          ]
      )

instance Data.ToPath UpdateVoiceProfileDomain where
  toPath UpdateVoiceProfileDomain' {..} =
    Prelude.mconcat
      [ "/voice-profile-domains/",
        Data.toBS voiceProfileDomainId
      ]

instance Data.ToQuery UpdateVoiceProfileDomain where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateVoiceProfileDomainResponse' smart constructor.
data UpdateVoiceProfileDomainResponse = UpdateVoiceProfileDomainResponse'
  { -- | The updated details of the voice profile domain.
    voiceProfileDomain :: Prelude.Maybe VoiceProfileDomain,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateVoiceProfileDomainResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'voiceProfileDomain', 'updateVoiceProfileDomainResponse_voiceProfileDomain' - The updated details of the voice profile domain.
--
-- 'httpStatus', 'updateVoiceProfileDomainResponse_httpStatus' - The response's http status code.
newUpdateVoiceProfileDomainResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateVoiceProfileDomainResponse
newUpdateVoiceProfileDomainResponse pHttpStatus_ =
  UpdateVoiceProfileDomainResponse'
    { voiceProfileDomain =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The updated details of the voice profile domain.
updateVoiceProfileDomainResponse_voiceProfileDomain :: Lens.Lens' UpdateVoiceProfileDomainResponse (Prelude.Maybe VoiceProfileDomain)
updateVoiceProfileDomainResponse_voiceProfileDomain = Lens.lens (\UpdateVoiceProfileDomainResponse' {voiceProfileDomain} -> voiceProfileDomain) (\s@UpdateVoiceProfileDomainResponse' {} a -> s {voiceProfileDomain = a} :: UpdateVoiceProfileDomainResponse)

-- | The response's http status code.
updateVoiceProfileDomainResponse_httpStatus :: Lens.Lens' UpdateVoiceProfileDomainResponse Prelude.Int
updateVoiceProfileDomainResponse_httpStatus = Lens.lens (\UpdateVoiceProfileDomainResponse' {httpStatus} -> httpStatus) (\s@UpdateVoiceProfileDomainResponse' {} a -> s {httpStatus = a} :: UpdateVoiceProfileDomainResponse)

instance
  Prelude.NFData
    UpdateVoiceProfileDomainResponse
  where
  rnf UpdateVoiceProfileDomainResponse' {..} =
    Prelude.rnf voiceProfileDomain
      `Prelude.seq` Prelude.rnf httpStatus
