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
-- Module      : Amazonka.ConnectCampaigns.UpdateCampaignDialerConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the dialer config of a campaign. This API is idempotent.
module Amazonka.ConnectCampaigns.UpdateCampaignDialerConfig
  ( -- * Creating a Request
    UpdateCampaignDialerConfig (..),
    newUpdateCampaignDialerConfig,

    -- * Request Lenses
    updateCampaignDialerConfig_dialerConfig,
    updateCampaignDialerConfig_id,

    -- * Destructuring the Response
    UpdateCampaignDialerConfigResponse (..),
    newUpdateCampaignDialerConfigResponse,
  )
where

import Amazonka.ConnectCampaigns.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | UpdateCampaignDialerConfigRequest
--
-- /See:/ 'newUpdateCampaignDialerConfig' smart constructor.
data UpdateCampaignDialerConfig = UpdateCampaignDialerConfig'
  { dialerConfig :: DialerConfig,
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateCampaignDialerConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dialerConfig', 'updateCampaignDialerConfig_dialerConfig' - Undocumented member.
--
-- 'id', 'updateCampaignDialerConfig_id' - Undocumented member.
newUpdateCampaignDialerConfig ::
  -- | 'dialerConfig'
  DialerConfig ->
  -- | 'id'
  Prelude.Text ->
  UpdateCampaignDialerConfig
newUpdateCampaignDialerConfig pDialerConfig_ pId_ =
  UpdateCampaignDialerConfig'
    { dialerConfig =
        pDialerConfig_,
      id = pId_
    }

-- | Undocumented member.
updateCampaignDialerConfig_dialerConfig :: Lens.Lens' UpdateCampaignDialerConfig DialerConfig
updateCampaignDialerConfig_dialerConfig = Lens.lens (\UpdateCampaignDialerConfig' {dialerConfig} -> dialerConfig) (\s@UpdateCampaignDialerConfig' {} a -> s {dialerConfig = a} :: UpdateCampaignDialerConfig)

-- | Undocumented member.
updateCampaignDialerConfig_id :: Lens.Lens' UpdateCampaignDialerConfig Prelude.Text
updateCampaignDialerConfig_id = Lens.lens (\UpdateCampaignDialerConfig' {id} -> id) (\s@UpdateCampaignDialerConfig' {} a -> s {id = a} :: UpdateCampaignDialerConfig)

instance Core.AWSRequest UpdateCampaignDialerConfig where
  type
    AWSResponse UpdateCampaignDialerConfig =
      UpdateCampaignDialerConfigResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      UpdateCampaignDialerConfigResponse'

instance Prelude.Hashable UpdateCampaignDialerConfig where
  hashWithSalt _salt UpdateCampaignDialerConfig' {..} =
    _salt `Prelude.hashWithSalt` dialerConfig
      `Prelude.hashWithSalt` id

instance Prelude.NFData UpdateCampaignDialerConfig where
  rnf UpdateCampaignDialerConfig' {..} =
    Prelude.rnf dialerConfig
      `Prelude.seq` Prelude.rnf id

instance Data.ToHeaders UpdateCampaignDialerConfig where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateCampaignDialerConfig where
  toJSON UpdateCampaignDialerConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("dialerConfig" Data..= dialerConfig)]
      )

instance Data.ToPath UpdateCampaignDialerConfig where
  toPath UpdateCampaignDialerConfig' {..} =
    Prelude.mconcat
      ["/campaigns/", Data.toBS id, "/dialer-config"]

instance Data.ToQuery UpdateCampaignDialerConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateCampaignDialerConfigResponse' smart constructor.
data UpdateCampaignDialerConfigResponse = UpdateCampaignDialerConfigResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateCampaignDialerConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateCampaignDialerConfigResponse ::
  UpdateCampaignDialerConfigResponse
newUpdateCampaignDialerConfigResponse =
  UpdateCampaignDialerConfigResponse'

instance
  Prelude.NFData
    UpdateCampaignDialerConfigResponse
  where
  rnf _ = ()
