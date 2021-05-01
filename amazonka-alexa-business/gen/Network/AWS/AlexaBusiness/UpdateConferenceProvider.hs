{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.AlexaBusiness.UpdateConferenceProvider
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing conference provider\'s settings.
module Network.AWS.AlexaBusiness.UpdateConferenceProvider
  ( -- * Creating a Request
    UpdateConferenceProvider (..),
    newUpdateConferenceProvider,

    -- * Request Lenses
    updateConferenceProvider_iPDialIn,
    updateConferenceProvider_pSTNDialIn,
    updateConferenceProvider_conferenceProviderArn,
    updateConferenceProvider_conferenceProviderType,
    updateConferenceProvider_meetingSetting,

    -- * Destructuring the Response
    UpdateConferenceProviderResponse (..),
    newUpdateConferenceProviderResponse,

    -- * Response Lenses
    updateConferenceProviderResponse_httpStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateConferenceProvider' smart constructor.
data UpdateConferenceProvider = UpdateConferenceProvider'
  { -- | The IP endpoint and protocol for calling.
    iPDialIn :: Prelude.Maybe IPDialIn,
    -- | The information for PSTN conferencing.
    pSTNDialIn :: Prelude.Maybe PSTNDialIn,
    -- | The ARN of the conference provider.
    conferenceProviderArn :: Prelude.Text,
    -- | The type of the conference provider.
    conferenceProviderType :: ConferenceProviderType,
    -- | The meeting settings for the conference provider.
    meetingSetting :: MeetingSetting
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateConferenceProvider' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'iPDialIn', 'updateConferenceProvider_iPDialIn' - The IP endpoint and protocol for calling.
--
-- 'pSTNDialIn', 'updateConferenceProvider_pSTNDialIn' - The information for PSTN conferencing.
--
-- 'conferenceProviderArn', 'updateConferenceProvider_conferenceProviderArn' - The ARN of the conference provider.
--
-- 'conferenceProviderType', 'updateConferenceProvider_conferenceProviderType' - The type of the conference provider.
--
-- 'meetingSetting', 'updateConferenceProvider_meetingSetting' - The meeting settings for the conference provider.
newUpdateConferenceProvider ::
  -- | 'conferenceProviderArn'
  Prelude.Text ->
  -- | 'conferenceProviderType'
  ConferenceProviderType ->
  -- | 'meetingSetting'
  MeetingSetting ->
  UpdateConferenceProvider
newUpdateConferenceProvider
  pConferenceProviderArn_
  pConferenceProviderType_
  pMeetingSetting_ =
    UpdateConferenceProvider'
      { iPDialIn =
          Prelude.Nothing,
        pSTNDialIn = Prelude.Nothing,
        conferenceProviderArn = pConferenceProviderArn_,
        conferenceProviderType = pConferenceProviderType_,
        meetingSetting = pMeetingSetting_
      }

-- | The IP endpoint and protocol for calling.
updateConferenceProvider_iPDialIn :: Lens.Lens' UpdateConferenceProvider (Prelude.Maybe IPDialIn)
updateConferenceProvider_iPDialIn = Lens.lens (\UpdateConferenceProvider' {iPDialIn} -> iPDialIn) (\s@UpdateConferenceProvider' {} a -> s {iPDialIn = a} :: UpdateConferenceProvider)

-- | The information for PSTN conferencing.
updateConferenceProvider_pSTNDialIn :: Lens.Lens' UpdateConferenceProvider (Prelude.Maybe PSTNDialIn)
updateConferenceProvider_pSTNDialIn = Lens.lens (\UpdateConferenceProvider' {pSTNDialIn} -> pSTNDialIn) (\s@UpdateConferenceProvider' {} a -> s {pSTNDialIn = a} :: UpdateConferenceProvider)

-- | The ARN of the conference provider.
updateConferenceProvider_conferenceProviderArn :: Lens.Lens' UpdateConferenceProvider Prelude.Text
updateConferenceProvider_conferenceProviderArn = Lens.lens (\UpdateConferenceProvider' {conferenceProviderArn} -> conferenceProviderArn) (\s@UpdateConferenceProvider' {} a -> s {conferenceProviderArn = a} :: UpdateConferenceProvider)

-- | The type of the conference provider.
updateConferenceProvider_conferenceProviderType :: Lens.Lens' UpdateConferenceProvider ConferenceProviderType
updateConferenceProvider_conferenceProviderType = Lens.lens (\UpdateConferenceProvider' {conferenceProviderType} -> conferenceProviderType) (\s@UpdateConferenceProvider' {} a -> s {conferenceProviderType = a} :: UpdateConferenceProvider)

-- | The meeting settings for the conference provider.
updateConferenceProvider_meetingSetting :: Lens.Lens' UpdateConferenceProvider MeetingSetting
updateConferenceProvider_meetingSetting = Lens.lens (\UpdateConferenceProvider' {meetingSetting} -> meetingSetting) (\s@UpdateConferenceProvider' {} a -> s {meetingSetting = a} :: UpdateConferenceProvider)

instance Prelude.AWSRequest UpdateConferenceProvider where
  type
    Rs UpdateConferenceProvider =
      UpdateConferenceProviderResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateConferenceProviderResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateConferenceProvider

instance Prelude.NFData UpdateConferenceProvider

instance Prelude.ToHeaders UpdateConferenceProvider where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AlexaForBusiness.UpdateConferenceProvider" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateConferenceProvider where
  toJSON UpdateConferenceProvider' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("IPDialIn" Prelude..=) Prelude.<$> iPDialIn,
            ("PSTNDialIn" Prelude..=) Prelude.<$> pSTNDialIn,
            Prelude.Just
              ( "ConferenceProviderArn"
                  Prelude..= conferenceProviderArn
              ),
            Prelude.Just
              ( "ConferenceProviderType"
                  Prelude..= conferenceProviderType
              ),
            Prelude.Just
              ("MeetingSetting" Prelude..= meetingSetting)
          ]
      )

instance Prelude.ToPath UpdateConferenceProvider where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateConferenceProvider where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateConferenceProviderResponse' smart constructor.
data UpdateConferenceProviderResponse = UpdateConferenceProviderResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateConferenceProviderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateConferenceProviderResponse_httpStatus' - The response's http status code.
newUpdateConferenceProviderResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateConferenceProviderResponse
newUpdateConferenceProviderResponse pHttpStatus_ =
  UpdateConferenceProviderResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateConferenceProviderResponse_httpStatus :: Lens.Lens' UpdateConferenceProviderResponse Prelude.Int
updateConferenceProviderResponse_httpStatus = Lens.lens (\UpdateConferenceProviderResponse' {httpStatus} -> httpStatus) (\s@UpdateConferenceProviderResponse' {} a -> s {httpStatus = a} :: UpdateConferenceProviderResponse)

instance
  Prelude.NFData
    UpdateConferenceProviderResponse
