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
-- Module      : Amazonka.ConnectCampaigns.CreateCampaign
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a campaign for the specified Amazon Connect account. This API is
-- idempotent.
module Amazonka.ConnectCampaigns.CreateCampaign
  ( -- * Creating a Request
    CreateCampaign (..),
    newCreateCampaign,

    -- * Request Lenses
    createCampaign_tags,
    createCampaign_connectInstanceId,
    createCampaign_dialerConfig,
    createCampaign_name,
    createCampaign_outboundCallConfig,

    -- * Destructuring the Response
    CreateCampaignResponse (..),
    newCreateCampaignResponse,

    -- * Response Lenses
    createCampaignResponse_arn,
    createCampaignResponse_id,
    createCampaignResponse_tags,
    createCampaignResponse_httpStatus,
  )
where

import Amazonka.ConnectCampaigns.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request for Create Campaign API.
--
-- /See:/ 'newCreateCampaign' smart constructor.
data CreateCampaign = CreateCampaign'
  { tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    connectInstanceId :: Prelude.Text,
    dialerConfig :: DialerConfig,
    name :: Prelude.Text,
    outboundCallConfig :: OutboundCallConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCampaign' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createCampaign_tags' - Undocumented member.
--
-- 'connectInstanceId', 'createCampaign_connectInstanceId' - Undocumented member.
--
-- 'dialerConfig', 'createCampaign_dialerConfig' - Undocumented member.
--
-- 'name', 'createCampaign_name' - Undocumented member.
--
-- 'outboundCallConfig', 'createCampaign_outboundCallConfig' - Undocumented member.
newCreateCampaign ::
  -- | 'connectInstanceId'
  Prelude.Text ->
  -- | 'dialerConfig'
  DialerConfig ->
  -- | 'name'
  Prelude.Text ->
  -- | 'outboundCallConfig'
  OutboundCallConfig ->
  CreateCampaign
newCreateCampaign
  pConnectInstanceId_
  pDialerConfig_
  pName_
  pOutboundCallConfig_ =
    CreateCampaign'
      { tags = Prelude.Nothing,
        connectInstanceId = pConnectInstanceId_,
        dialerConfig = pDialerConfig_,
        name = pName_,
        outboundCallConfig = pOutboundCallConfig_
      }

-- | Undocumented member.
createCampaign_tags :: Lens.Lens' CreateCampaign (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createCampaign_tags = Lens.lens (\CreateCampaign' {tags} -> tags) (\s@CreateCampaign' {} a -> s {tags = a} :: CreateCampaign) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
createCampaign_connectInstanceId :: Lens.Lens' CreateCampaign Prelude.Text
createCampaign_connectInstanceId = Lens.lens (\CreateCampaign' {connectInstanceId} -> connectInstanceId) (\s@CreateCampaign' {} a -> s {connectInstanceId = a} :: CreateCampaign)

-- | Undocumented member.
createCampaign_dialerConfig :: Lens.Lens' CreateCampaign DialerConfig
createCampaign_dialerConfig = Lens.lens (\CreateCampaign' {dialerConfig} -> dialerConfig) (\s@CreateCampaign' {} a -> s {dialerConfig = a} :: CreateCampaign)

-- | Undocumented member.
createCampaign_name :: Lens.Lens' CreateCampaign Prelude.Text
createCampaign_name = Lens.lens (\CreateCampaign' {name} -> name) (\s@CreateCampaign' {} a -> s {name = a} :: CreateCampaign)

-- | Undocumented member.
createCampaign_outboundCallConfig :: Lens.Lens' CreateCampaign OutboundCallConfig
createCampaign_outboundCallConfig = Lens.lens (\CreateCampaign' {outboundCallConfig} -> outboundCallConfig) (\s@CreateCampaign' {} a -> s {outboundCallConfig = a} :: CreateCampaign)

instance Core.AWSRequest CreateCampaign where
  type
    AWSResponse CreateCampaign =
      CreateCampaignResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateCampaignResponse'
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "id")
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateCampaign where
  hashWithSalt _salt CreateCampaign' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` connectInstanceId
      `Prelude.hashWithSalt` dialerConfig
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` outboundCallConfig

instance Prelude.NFData CreateCampaign where
  rnf CreateCampaign' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf connectInstanceId
      `Prelude.seq` Prelude.rnf dialerConfig
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf outboundCallConfig

instance Data.ToHeaders CreateCampaign where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateCampaign where
  toJSON CreateCampaign' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("connectInstanceId" Data..= connectInstanceId),
            Prelude.Just ("dialerConfig" Data..= dialerConfig),
            Prelude.Just ("name" Data..= name),
            Prelude.Just
              ("outboundCallConfig" Data..= outboundCallConfig)
          ]
      )

instance Data.ToPath CreateCampaign where
  toPath = Prelude.const "/campaigns"

instance Data.ToQuery CreateCampaign where
  toQuery = Prelude.const Prelude.mempty

-- | The response for Create Campaign API
--
-- /See:/ 'newCreateCampaignResponse' smart constructor.
data CreateCampaignResponse = CreateCampaignResponse'
  { arn :: Prelude.Maybe Prelude.Text,
    id :: Prelude.Maybe Prelude.Text,
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCampaignResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'createCampaignResponse_arn' - Undocumented member.
--
-- 'id', 'createCampaignResponse_id' - Undocumented member.
--
-- 'tags', 'createCampaignResponse_tags' - Undocumented member.
--
-- 'httpStatus', 'createCampaignResponse_httpStatus' - The response's http status code.
newCreateCampaignResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateCampaignResponse
newCreateCampaignResponse pHttpStatus_ =
  CreateCampaignResponse'
    { arn = Prelude.Nothing,
      id = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createCampaignResponse_arn :: Lens.Lens' CreateCampaignResponse (Prelude.Maybe Prelude.Text)
createCampaignResponse_arn = Lens.lens (\CreateCampaignResponse' {arn} -> arn) (\s@CreateCampaignResponse' {} a -> s {arn = a} :: CreateCampaignResponse)

-- | Undocumented member.
createCampaignResponse_id :: Lens.Lens' CreateCampaignResponse (Prelude.Maybe Prelude.Text)
createCampaignResponse_id = Lens.lens (\CreateCampaignResponse' {id} -> id) (\s@CreateCampaignResponse' {} a -> s {id = a} :: CreateCampaignResponse)

-- | Undocumented member.
createCampaignResponse_tags :: Lens.Lens' CreateCampaignResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createCampaignResponse_tags = Lens.lens (\CreateCampaignResponse' {tags} -> tags) (\s@CreateCampaignResponse' {} a -> s {tags = a} :: CreateCampaignResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createCampaignResponse_httpStatus :: Lens.Lens' CreateCampaignResponse Prelude.Int
createCampaignResponse_httpStatus = Lens.lens (\CreateCampaignResponse' {httpStatus} -> httpStatus) (\s@CreateCampaignResponse' {} a -> s {httpStatus = a} :: CreateCampaignResponse)

instance Prelude.NFData CreateCampaignResponse where
  rnf CreateCampaignResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
