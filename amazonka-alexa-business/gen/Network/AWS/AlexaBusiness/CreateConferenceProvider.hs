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
-- Module      : Network.AWS.AlexaBusiness.CreateConferenceProvider
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a new conference provider under the user\'s AWS account.
module Network.AWS.AlexaBusiness.CreateConferenceProvider
  ( -- * Creating a Request
    CreateConferenceProvider (..),
    newCreateConferenceProvider,

    -- * Request Lenses
    createConferenceProvider_iPDialIn,
    createConferenceProvider_tags,
    createConferenceProvider_pSTNDialIn,
    createConferenceProvider_clientRequestToken,
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

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateConferenceProvider' smart constructor.
data CreateConferenceProvider = CreateConferenceProvider'
  { -- | The IP endpoint and protocol for calling.
    iPDialIn :: Prelude.Maybe IPDialIn,
    -- | The tags to be added to the specified resource. Do not provide system
    -- tags.
    tags :: Prelude.Maybe [Tag],
    -- | The information for PSTN conferencing.
    pSTNDialIn :: Prelude.Maybe PSTNDialIn,
    -- | The request token of the client.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the conference provider.
    conferenceProviderName :: Prelude.Text,
    -- | Represents a type within a list of predefined types.
    conferenceProviderType :: ConferenceProviderType,
    -- | The meeting settings for the conference provider.
    meetingSetting :: MeetingSetting
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateConferenceProvider' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'iPDialIn', 'createConferenceProvider_iPDialIn' - The IP endpoint and protocol for calling.
--
-- 'tags', 'createConferenceProvider_tags' - The tags to be added to the specified resource. Do not provide system
-- tags.
--
-- 'pSTNDialIn', 'createConferenceProvider_pSTNDialIn' - The information for PSTN conferencing.
--
-- 'clientRequestToken', 'createConferenceProvider_clientRequestToken' - The request token of the client.
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
      { iPDialIn =
          Prelude.Nothing,
        tags = Prelude.Nothing,
        pSTNDialIn = Prelude.Nothing,
        clientRequestToken = Prelude.Nothing,
        conferenceProviderName = pConferenceProviderName_,
        conferenceProviderType = pConferenceProviderType_,
        meetingSetting = pMeetingSetting_
      }

-- | The IP endpoint and protocol for calling.
createConferenceProvider_iPDialIn :: Lens.Lens' CreateConferenceProvider (Prelude.Maybe IPDialIn)
createConferenceProvider_iPDialIn = Lens.lens (\CreateConferenceProvider' {iPDialIn} -> iPDialIn) (\s@CreateConferenceProvider' {} a -> s {iPDialIn = a} :: CreateConferenceProvider)

-- | The tags to be added to the specified resource. Do not provide system
-- tags.
createConferenceProvider_tags :: Lens.Lens' CreateConferenceProvider (Prelude.Maybe [Tag])
createConferenceProvider_tags = Lens.lens (\CreateConferenceProvider' {tags} -> tags) (\s@CreateConferenceProvider' {} a -> s {tags = a} :: CreateConferenceProvider) Prelude.. Lens.mapping Prelude._Coerce

-- | The information for PSTN conferencing.
createConferenceProvider_pSTNDialIn :: Lens.Lens' CreateConferenceProvider (Prelude.Maybe PSTNDialIn)
createConferenceProvider_pSTNDialIn = Lens.lens (\CreateConferenceProvider' {pSTNDialIn} -> pSTNDialIn) (\s@CreateConferenceProvider' {} a -> s {pSTNDialIn = a} :: CreateConferenceProvider)

-- | The request token of the client.
createConferenceProvider_clientRequestToken :: Lens.Lens' CreateConferenceProvider (Prelude.Maybe Prelude.Text)
createConferenceProvider_clientRequestToken = Lens.lens (\CreateConferenceProvider' {clientRequestToken} -> clientRequestToken) (\s@CreateConferenceProvider' {} a -> s {clientRequestToken = a} :: CreateConferenceProvider)

-- | The name of the conference provider.
createConferenceProvider_conferenceProviderName :: Lens.Lens' CreateConferenceProvider Prelude.Text
createConferenceProvider_conferenceProviderName = Lens.lens (\CreateConferenceProvider' {conferenceProviderName} -> conferenceProviderName) (\s@CreateConferenceProvider' {} a -> s {conferenceProviderName = a} :: CreateConferenceProvider)

-- | Represents a type within a list of predefined types.
createConferenceProvider_conferenceProviderType :: Lens.Lens' CreateConferenceProvider ConferenceProviderType
createConferenceProvider_conferenceProviderType = Lens.lens (\CreateConferenceProvider' {conferenceProviderType} -> conferenceProviderType) (\s@CreateConferenceProvider' {} a -> s {conferenceProviderType = a} :: CreateConferenceProvider)

-- | The meeting settings for the conference provider.
createConferenceProvider_meetingSetting :: Lens.Lens' CreateConferenceProvider MeetingSetting
createConferenceProvider_meetingSetting = Lens.lens (\CreateConferenceProvider' {meetingSetting} -> meetingSetting) (\s@CreateConferenceProvider' {} a -> s {meetingSetting = a} :: CreateConferenceProvider)

instance Prelude.AWSRequest CreateConferenceProvider where
  type
    Rs CreateConferenceProvider =
      CreateConferenceProviderResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateConferenceProviderResponse'
            Prelude.<$> (x Prelude..?> "ConferenceProviderArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateConferenceProvider

instance Prelude.NFData CreateConferenceProvider

instance Prelude.ToHeaders CreateConferenceProvider where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AlexaForBusiness.CreateConferenceProvider" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateConferenceProvider where
  toJSON CreateConferenceProvider' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("IPDialIn" Prelude..=) Prelude.<$> iPDialIn,
            ("Tags" Prelude..=) Prelude.<$> tags,
            ("PSTNDialIn" Prelude..=) Prelude.<$> pSTNDialIn,
            ("ClientRequestToken" Prelude..=)
              Prelude.<$> clientRequestToken,
            Prelude.Just
              ( "ConferenceProviderName"
                  Prelude..= conferenceProviderName
              ),
            Prelude.Just
              ( "ConferenceProviderType"
                  Prelude..= conferenceProviderType
              ),
            Prelude.Just
              ("MeetingSetting" Prelude..= meetingSetting)
          ]
      )

instance Prelude.ToPath CreateConferenceProvider where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateConferenceProvider where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateConferenceProviderResponse' smart constructor.
data CreateConferenceProviderResponse = CreateConferenceProviderResponse'
  { -- | The ARN of the newly-created conference provider.
    conferenceProviderArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
