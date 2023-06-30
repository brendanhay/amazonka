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
-- Module      : Amazonka.ChimeSdkVoice.PutSipMediaApplicationAlexaSkillConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- -- | Undocumented operation.
module Amazonka.ChimeSdkVoice.PutSipMediaApplicationAlexaSkillConfiguration
  ( -- * Creating a Request
    PutSipMediaApplicationAlexaSkillConfiguration (..),
    newPutSipMediaApplicationAlexaSkillConfiguration,

    -- * Request Lenses
    putSipMediaApplicationAlexaSkillConfiguration_sipMediaApplicationAlexaSkillConfiguration,
    putSipMediaApplicationAlexaSkillConfiguration_sipMediaApplicationId,

    -- * Destructuring the Response
    PutSipMediaApplicationAlexaSkillConfigurationResponse (..),
    newPutSipMediaApplicationAlexaSkillConfigurationResponse,

    -- * Response Lenses
    putSipMediaApplicationAlexaSkillConfigurationResponse_sipMediaApplicationAlexaSkillConfiguration,
    putSipMediaApplicationAlexaSkillConfigurationResponse_httpStatus,
  )
where

import Amazonka.ChimeSdkVoice.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutSipMediaApplicationAlexaSkillConfiguration' smart constructor.
data PutSipMediaApplicationAlexaSkillConfiguration = PutSipMediaApplicationAlexaSkillConfiguration'
  { sipMediaApplicationAlexaSkillConfiguration :: Prelude.Maybe SipMediaApplicationAlexaSkillConfiguration,
    sipMediaApplicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutSipMediaApplicationAlexaSkillConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sipMediaApplicationAlexaSkillConfiguration', 'putSipMediaApplicationAlexaSkillConfiguration_sipMediaApplicationAlexaSkillConfiguration' - Undocumented member.
--
-- 'sipMediaApplicationId', 'putSipMediaApplicationAlexaSkillConfiguration_sipMediaApplicationId' - Undocumented member.
newPutSipMediaApplicationAlexaSkillConfiguration ::
  -- | 'sipMediaApplicationId'
  Prelude.Text ->
  PutSipMediaApplicationAlexaSkillConfiguration
newPutSipMediaApplicationAlexaSkillConfiguration
  pSipMediaApplicationId_ =
    PutSipMediaApplicationAlexaSkillConfiguration'
      { sipMediaApplicationAlexaSkillConfiguration =
          Prelude.Nothing,
        sipMediaApplicationId =
          pSipMediaApplicationId_
      }

-- | Undocumented member.
putSipMediaApplicationAlexaSkillConfiguration_sipMediaApplicationAlexaSkillConfiguration :: Lens.Lens' PutSipMediaApplicationAlexaSkillConfiguration (Prelude.Maybe SipMediaApplicationAlexaSkillConfiguration)
putSipMediaApplicationAlexaSkillConfiguration_sipMediaApplicationAlexaSkillConfiguration = Lens.lens (\PutSipMediaApplicationAlexaSkillConfiguration' {sipMediaApplicationAlexaSkillConfiguration} -> sipMediaApplicationAlexaSkillConfiguration) (\s@PutSipMediaApplicationAlexaSkillConfiguration' {} a -> s {sipMediaApplicationAlexaSkillConfiguration = a} :: PutSipMediaApplicationAlexaSkillConfiguration)

-- | Undocumented member.
putSipMediaApplicationAlexaSkillConfiguration_sipMediaApplicationId :: Lens.Lens' PutSipMediaApplicationAlexaSkillConfiguration Prelude.Text
putSipMediaApplicationAlexaSkillConfiguration_sipMediaApplicationId = Lens.lens (\PutSipMediaApplicationAlexaSkillConfiguration' {sipMediaApplicationId} -> sipMediaApplicationId) (\s@PutSipMediaApplicationAlexaSkillConfiguration' {} a -> s {sipMediaApplicationId = a} :: PutSipMediaApplicationAlexaSkillConfiguration)

instance
  Core.AWSRequest
    PutSipMediaApplicationAlexaSkillConfiguration
  where
  type
    AWSResponse
      PutSipMediaApplicationAlexaSkillConfiguration =
      PutSipMediaApplicationAlexaSkillConfigurationResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutSipMediaApplicationAlexaSkillConfigurationResponse'
            Prelude.<$> ( x
                            Data..?> "SipMediaApplicationAlexaSkillConfiguration"
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    PutSipMediaApplicationAlexaSkillConfiguration
  where
  hashWithSalt
    _salt
    PutSipMediaApplicationAlexaSkillConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` sipMediaApplicationAlexaSkillConfiguration
        `Prelude.hashWithSalt` sipMediaApplicationId

instance
  Prelude.NFData
    PutSipMediaApplicationAlexaSkillConfiguration
  where
  rnf
    PutSipMediaApplicationAlexaSkillConfiguration' {..} =
      Prelude.rnf
        sipMediaApplicationAlexaSkillConfiguration
        `Prelude.seq` Prelude.rnf sipMediaApplicationId

instance
  Data.ToHeaders
    PutSipMediaApplicationAlexaSkillConfiguration
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToJSON
    PutSipMediaApplicationAlexaSkillConfiguration
  where
  toJSON
    PutSipMediaApplicationAlexaSkillConfiguration' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ( "SipMediaApplicationAlexaSkillConfiguration"
                  Data..=
              )
                Prelude.<$> sipMediaApplicationAlexaSkillConfiguration
            ]
        )

instance
  Data.ToPath
    PutSipMediaApplicationAlexaSkillConfiguration
  where
  toPath
    PutSipMediaApplicationAlexaSkillConfiguration' {..} =
      Prelude.mconcat
        [ "/sip-media-applications/",
          Data.toBS sipMediaApplicationId,
          "/alexa-skill-configuration"
        ]

instance
  Data.ToQuery
    PutSipMediaApplicationAlexaSkillConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutSipMediaApplicationAlexaSkillConfigurationResponse' smart constructor.
data PutSipMediaApplicationAlexaSkillConfigurationResponse = PutSipMediaApplicationAlexaSkillConfigurationResponse'
  { sipMediaApplicationAlexaSkillConfiguration :: Prelude.Maybe SipMediaApplicationAlexaSkillConfiguration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutSipMediaApplicationAlexaSkillConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sipMediaApplicationAlexaSkillConfiguration', 'putSipMediaApplicationAlexaSkillConfigurationResponse_sipMediaApplicationAlexaSkillConfiguration' - Undocumented member.
--
-- 'httpStatus', 'putSipMediaApplicationAlexaSkillConfigurationResponse_httpStatus' - The response's http status code.
newPutSipMediaApplicationAlexaSkillConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutSipMediaApplicationAlexaSkillConfigurationResponse
newPutSipMediaApplicationAlexaSkillConfigurationResponse
  pHttpStatus_ =
    PutSipMediaApplicationAlexaSkillConfigurationResponse'
      { sipMediaApplicationAlexaSkillConfiguration =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | Undocumented member.
putSipMediaApplicationAlexaSkillConfigurationResponse_sipMediaApplicationAlexaSkillConfiguration :: Lens.Lens' PutSipMediaApplicationAlexaSkillConfigurationResponse (Prelude.Maybe SipMediaApplicationAlexaSkillConfiguration)
putSipMediaApplicationAlexaSkillConfigurationResponse_sipMediaApplicationAlexaSkillConfiguration = Lens.lens (\PutSipMediaApplicationAlexaSkillConfigurationResponse' {sipMediaApplicationAlexaSkillConfiguration} -> sipMediaApplicationAlexaSkillConfiguration) (\s@PutSipMediaApplicationAlexaSkillConfigurationResponse' {} a -> s {sipMediaApplicationAlexaSkillConfiguration = a} :: PutSipMediaApplicationAlexaSkillConfigurationResponse)

-- | The response's http status code.
putSipMediaApplicationAlexaSkillConfigurationResponse_httpStatus :: Lens.Lens' PutSipMediaApplicationAlexaSkillConfigurationResponse Prelude.Int
putSipMediaApplicationAlexaSkillConfigurationResponse_httpStatus = Lens.lens (\PutSipMediaApplicationAlexaSkillConfigurationResponse' {httpStatus} -> httpStatus) (\s@PutSipMediaApplicationAlexaSkillConfigurationResponse' {} a -> s {httpStatus = a} :: PutSipMediaApplicationAlexaSkillConfigurationResponse)

instance
  Prelude.NFData
    PutSipMediaApplicationAlexaSkillConfigurationResponse
  where
  rnf
    PutSipMediaApplicationAlexaSkillConfigurationResponse' {..} =
      Prelude.rnf
        sipMediaApplicationAlexaSkillConfiguration
        `Prelude.seq` Prelude.rnf httpStatus
