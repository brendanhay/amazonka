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
-- Module      : Amazonka.AlexaBusiness.CreateConferenceProvider
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a new conference provider under the user\'s AWS account.
module Amazonka.AlexaBusiness.CreateConferenceProvider
  ( -- * Creating a Request
    CreateConferenceProvider (..),
    newCreateConferenceProvider,

    -- * Request Lenses
    createConferenceProvider_clientRequestToken,
    createConferenceProvider_iPDialIn,
    createConferenceProvider_pSTNDialIn,
    createConferenceProvider_tags,
    createConferenceProvider_conferenceProviderName,
    createConferenceProvider_conferenceProviderType,
    createConferenceProvider_meetingSetting,

    -- * Destructuring the Response
    CreateConferenceProviderResponse (..),
    newCreateConferenceProviderResponse,

    -- * Response Lenses
    createConferenceProviderResponse_conferenceProviderArn,
    createConferenceProviderResponse_httpStatus,
  )
where

import Amazonka.AlexaBusiness.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateConferenceProvider' smart constructor.
data CreateConferenceProvider = CreateConferenceProvider'
  { -- | The request token of the client.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The IP endpoint and protocol for calling.
    iPDialIn :: Prelude.Maybe IPDialIn,
    -- | The information for PSTN conferencing.
    pSTNDialIn :: Prelude.Maybe PSTNDialIn,
    -- | The tags to be added to the specified resource. Do not provide system
    -- tags.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the conference provider.
    conferenceProviderName :: Prelude.Text,
    -- | Represents a type within a list of predefined types.
    conferenceProviderType :: ConferenceProviderType,
    -- | The meeting settings for the conference provider.
    meetingSetting :: MeetingSetting
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateConferenceProvider' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'createConferenceProvider_clientRequestToken' - The request token of the client.
--
-- 'iPDialIn', 'createConferenceProvider_iPDialIn' - The IP endpoint and protocol for calling.
--
-- 'pSTNDialIn', 'createConferenceProvider_pSTNDialIn' - The information for PSTN conferencing.
--
-- 'tags', 'createConferenceProvider_tags' - The tags to be added to the specified resource. Do not provide system
-- tags.
--
-- 'conferenceProviderName', 'createConferenceProvider_conferenceProviderName' - The name of the conference provider.
--
-- 'conferenceProviderType', 'createConferenceProvider_conferenceProviderType' - Represents a type within a list of predefined types.
--
-- 'meetingSetting', 'createConferenceProvider_meetingSetting' - The meeting settings for the conference provider.
newCreateConferenceProvider ::
  -- | 'conferenceProviderName'
  Prelude.Text ->
  -- | 'conferenceProviderType'
  ConferenceProviderType ->
  -- | 'meetingSetting'
  MeetingSetting ->
  CreateConferenceProvider
newCreateConferenceProvider
  pConferenceProviderName_
  pConferenceProviderType_
  pMeetingSetting_ =
    CreateConferenceProvider'
      { clientRequestToken =
          Prelude.Nothing,
        iPDialIn = Prelude.Nothing,
        pSTNDialIn = Prelude.Nothing,
        tags = Prelude.Nothing,
        conferenceProviderName = pConferenceProviderName_,
        conferenceProviderType = pConferenceProviderType_,
        meetingSetting = pMeetingSetting_
      }

-- | The request token of the client.
createConferenceProvider_clientRequestToken :: Lens.Lens' CreateConferenceProvider (Prelude.Maybe Prelude.Text)
createConferenceProvider_clientRequestToken = Lens.lens (\CreateConferenceProvider' {clientRequestToken} -> clientRequestToken) (\s@CreateConferenceProvider' {} a -> s {clientRequestToken = a} :: CreateConferenceProvider)

-- | The IP endpoint and protocol for calling.
createConferenceProvider_iPDialIn :: Lens.Lens' CreateConferenceProvider (Prelude.Maybe IPDialIn)
createConferenceProvider_iPDialIn = Lens.lens (\CreateConferenceProvider' {iPDialIn} -> iPDialIn) (\s@CreateConferenceProvider' {} a -> s {iPDialIn = a} :: CreateConferenceProvider)

-- | The information for PSTN conferencing.
createConferenceProvider_pSTNDialIn :: Lens.Lens' CreateConferenceProvider (Prelude.Maybe PSTNDialIn)
createConferenceProvider_pSTNDialIn = Lens.lens (\CreateConferenceProvider' {pSTNDialIn} -> pSTNDialIn) (\s@CreateConferenceProvider' {} a -> s {pSTNDialIn = a} :: CreateConferenceProvider)

-- | The tags to be added to the specified resource. Do not provide system
-- tags.
createConferenceProvider_tags :: Lens.Lens' CreateConferenceProvider (Prelude.Maybe [Tag])
createConferenceProvider_tags = Lens.lens (\CreateConferenceProvider' {tags} -> tags) (\s@CreateConferenceProvider' {} a -> s {tags = a} :: CreateConferenceProvider) Prelude.. Lens.mapping Lens.coerced

-- | The name of the conference provider.
createConferenceProvider_conferenceProviderName :: Lens.Lens' CreateConferenceProvider Prelude.Text
createConferenceProvider_conferenceProviderName = Lens.lens (\CreateConferenceProvider' {conferenceProviderName} -> conferenceProviderName) (\s@CreateConferenceProvider' {} a -> s {conferenceProviderName = a} :: CreateConferenceProvider)

-- | Represents a type within a list of predefined types.
createConferenceProvider_conferenceProviderType :: Lens.Lens' CreateConferenceProvider ConferenceProviderType
createConferenceProvider_conferenceProviderType = Lens.lens (\CreateConferenceProvider' {conferenceProviderType} -> conferenceProviderType) (\s@CreateConferenceProvider' {} a -> s {conferenceProviderType = a} :: CreateConferenceProvider)

-- | The meeting settings for the conference provider.
createConferenceProvider_meetingSetting :: Lens.Lens' CreateConferenceProvider MeetingSetting
createConferenceProvider_meetingSetting = Lens.lens (\CreateConferenceProvider' {meetingSetting} -> meetingSetting) (\s@CreateConferenceProvider' {} a -> s {meetingSetting = a} :: CreateConferenceProvider)

instance Core.AWSRequest CreateConferenceProvider where
  type
    AWSResponse CreateConferenceProvider =
      CreateConferenceProviderResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateConferenceProviderResponse'
            Prelude.<$> (x Data..?> "ConferenceProviderArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateConferenceProvider where
  hashWithSalt _salt CreateConferenceProvider' {..} =
    _salt `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` iPDialIn
      `Prelude.hashWithSalt` pSTNDialIn
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` conferenceProviderName
      `Prelude.hashWithSalt` conferenceProviderType
      `Prelude.hashWithSalt` meetingSetting

instance Prelude.NFData CreateConferenceProvider where
  rnf CreateConferenceProvider' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf iPDialIn
      `Prelude.seq` Prelude.rnf pSTNDialIn
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf conferenceProviderName
      `Prelude.seq` Prelude.rnf conferenceProviderType
      `Prelude.seq` Prelude.rnf meetingSetting

instance Data.ToHeaders CreateConferenceProvider where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AlexaForBusiness.CreateConferenceProvider" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateConferenceProvider where
  toJSON CreateConferenceProvider' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("IPDialIn" Data..=) Prelude.<$> iPDialIn,
            ("PSTNDialIn" Data..=) Prelude.<$> pSTNDialIn,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ( "ConferenceProviderName"
                  Data..= conferenceProviderName
              ),
            Prelude.Just
              ( "ConferenceProviderType"
                  Data..= conferenceProviderType
              ),
            Prelude.Just
              ("MeetingSetting" Data..= meetingSetting)
          ]
      )

instance Data.ToPath CreateConferenceProvider where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateConferenceProvider where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateConferenceProviderResponse' smart constructor.
data CreateConferenceProviderResponse = CreateConferenceProviderResponse'
  { -- | The ARN of the newly-created conference provider.
    conferenceProviderArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateConferenceProviderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'conferenceProviderArn', 'createConferenceProviderResponse_conferenceProviderArn' - The ARN of the newly-created conference provider.
--
-- 'httpStatus', 'createConferenceProviderResponse_httpStatus' - The response's http status code.
newCreateConferenceProviderResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateConferenceProviderResponse
newCreateConferenceProviderResponse pHttpStatus_ =
  CreateConferenceProviderResponse'
    { conferenceProviderArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the newly-created conference provider.
createConferenceProviderResponse_conferenceProviderArn :: Lens.Lens' CreateConferenceProviderResponse (Prelude.Maybe Prelude.Text)
createConferenceProviderResponse_conferenceProviderArn = Lens.lens (\CreateConferenceProviderResponse' {conferenceProviderArn} -> conferenceProviderArn) (\s@CreateConferenceProviderResponse' {} a -> s {conferenceProviderArn = a} :: CreateConferenceProviderResponse)

-- | The response's http status code.
createConferenceProviderResponse_httpStatus :: Lens.Lens' CreateConferenceProviderResponse Prelude.Int
createConferenceProviderResponse_httpStatus = Lens.lens (\CreateConferenceProviderResponse' {httpStatus} -> httpStatus) (\s@CreateConferenceProviderResponse' {} a -> s {httpStatus = a} :: CreateConferenceProviderResponse)

instance
  Prelude.NFData
    CreateConferenceProviderResponse
  where
  rnf CreateConferenceProviderResponse' {..} =
    Prelude.rnf conferenceProviderArn
      `Prelude.seq` Prelude.rnf httpStatus
