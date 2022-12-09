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
-- Module      : Amazonka.ChimeSdkVoice.GetSipMediaApplicationAlexaSkillConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- -- | Undocumented operation.
module Amazonka.ChimeSdkVoice.GetSipMediaApplicationAlexaSkillConfiguration
  ( -- * Creating a Request
    GetSipMediaApplicationAlexaSkillConfiguration (..),
    newGetSipMediaApplicationAlexaSkillConfiguration,

    -- * Request Lenses
    getSipMediaApplicationAlexaSkillConfiguration_sipMediaApplicationId,

    -- * Destructuring the Response
    GetSipMediaApplicationAlexaSkillConfigurationResponse (..),
    newGetSipMediaApplicationAlexaSkillConfigurationResponse,

    -- * Response Lenses
    getSipMediaApplicationAlexaSkillConfigurationResponse_sipMediaApplicationAlexaSkillConfiguration,
    getSipMediaApplicationAlexaSkillConfigurationResponse_httpStatus,
  )
where

import Amazonka.ChimeSdkVoice.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetSipMediaApplicationAlexaSkillConfiguration' smart constructor.
data GetSipMediaApplicationAlexaSkillConfiguration = GetSipMediaApplicationAlexaSkillConfiguration'
  { sipMediaApplicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSipMediaApplicationAlexaSkillConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sipMediaApplicationId', 'getSipMediaApplicationAlexaSkillConfiguration_sipMediaApplicationId' - Undocumented member.
newGetSipMediaApplicationAlexaSkillConfiguration ::
  -- | 'sipMediaApplicationId'
  Prelude.Text ->
  GetSipMediaApplicationAlexaSkillConfiguration
newGetSipMediaApplicationAlexaSkillConfiguration
  pSipMediaApplicationId_ =
    GetSipMediaApplicationAlexaSkillConfiguration'
      { sipMediaApplicationId =
          pSipMediaApplicationId_
      }

-- | Undocumented member.
getSipMediaApplicationAlexaSkillConfiguration_sipMediaApplicationId :: Lens.Lens' GetSipMediaApplicationAlexaSkillConfiguration Prelude.Text
getSipMediaApplicationAlexaSkillConfiguration_sipMediaApplicationId = Lens.lens (\GetSipMediaApplicationAlexaSkillConfiguration' {sipMediaApplicationId} -> sipMediaApplicationId) (\s@GetSipMediaApplicationAlexaSkillConfiguration' {} a -> s {sipMediaApplicationId = a} :: GetSipMediaApplicationAlexaSkillConfiguration)

instance
  Core.AWSRequest
    GetSipMediaApplicationAlexaSkillConfiguration
  where
  type
    AWSResponse
      GetSipMediaApplicationAlexaSkillConfiguration =
      GetSipMediaApplicationAlexaSkillConfigurationResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSipMediaApplicationAlexaSkillConfigurationResponse'
            Prelude.<$> ( x
                            Data..?> "SipMediaApplicationAlexaSkillConfiguration"
                        )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetSipMediaApplicationAlexaSkillConfiguration
  where
  hashWithSalt
    _salt
    GetSipMediaApplicationAlexaSkillConfiguration' {..} =
      _salt `Prelude.hashWithSalt` sipMediaApplicationId

instance
  Prelude.NFData
    GetSipMediaApplicationAlexaSkillConfiguration
  where
  rnf
    GetSipMediaApplicationAlexaSkillConfiguration' {..} =
      Prelude.rnf sipMediaApplicationId

instance
  Data.ToHeaders
    GetSipMediaApplicationAlexaSkillConfiguration
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    GetSipMediaApplicationAlexaSkillConfiguration
  where
  toPath
    GetSipMediaApplicationAlexaSkillConfiguration' {..} =
      Prelude.mconcat
        [ "/sip-media-applications/",
          Data.toBS sipMediaApplicationId,
          "/alexa-skill-configuration"
        ]

instance
  Data.ToQuery
    GetSipMediaApplicationAlexaSkillConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSipMediaApplicationAlexaSkillConfigurationResponse' smart constructor.
data GetSipMediaApplicationAlexaSkillConfigurationResponse = GetSipMediaApplicationAlexaSkillConfigurationResponse'
  { sipMediaApplicationAlexaSkillConfiguration :: Prelude.Maybe SipMediaApplicationAlexaSkillConfiguration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSipMediaApplicationAlexaSkillConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sipMediaApplicationAlexaSkillConfiguration', 'getSipMediaApplicationAlexaSkillConfigurationResponse_sipMediaApplicationAlexaSkillConfiguration' - Undocumented member.
--
-- 'httpStatus', 'getSipMediaApplicationAlexaSkillConfigurationResponse_httpStatus' - The response's http status code.
newGetSipMediaApplicationAlexaSkillConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSipMediaApplicationAlexaSkillConfigurationResponse
newGetSipMediaApplicationAlexaSkillConfigurationResponse
  pHttpStatus_ =
    GetSipMediaApplicationAlexaSkillConfigurationResponse'
      { sipMediaApplicationAlexaSkillConfiguration =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | Undocumented member.
getSipMediaApplicationAlexaSkillConfigurationResponse_sipMediaApplicationAlexaSkillConfiguration :: Lens.Lens' GetSipMediaApplicationAlexaSkillConfigurationResponse (Prelude.Maybe SipMediaApplicationAlexaSkillConfiguration)
getSipMediaApplicationAlexaSkillConfigurationResponse_sipMediaApplicationAlexaSkillConfiguration = Lens.lens (\GetSipMediaApplicationAlexaSkillConfigurationResponse' {sipMediaApplicationAlexaSkillConfiguration} -> sipMediaApplicationAlexaSkillConfiguration) (\s@GetSipMediaApplicationAlexaSkillConfigurationResponse' {} a -> s {sipMediaApplicationAlexaSkillConfiguration = a} :: GetSipMediaApplicationAlexaSkillConfigurationResponse)

-- | The response's http status code.
getSipMediaApplicationAlexaSkillConfigurationResponse_httpStatus :: Lens.Lens' GetSipMediaApplicationAlexaSkillConfigurationResponse Prelude.Int
getSipMediaApplicationAlexaSkillConfigurationResponse_httpStatus = Lens.lens (\GetSipMediaApplicationAlexaSkillConfigurationResponse' {httpStatus} -> httpStatus) (\s@GetSipMediaApplicationAlexaSkillConfigurationResponse' {} a -> s {httpStatus = a} :: GetSipMediaApplicationAlexaSkillConfigurationResponse)

instance
  Prelude.NFData
    GetSipMediaApplicationAlexaSkillConfigurationResponse
  where
  rnf
    GetSipMediaApplicationAlexaSkillConfigurationResponse' {..} =
      Prelude.rnf
        sipMediaApplicationAlexaSkillConfiguration
        `Prelude.seq` Prelude.rnf httpStatus
