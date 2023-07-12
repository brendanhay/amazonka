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
-- Module      : Amazonka.ConnectCampaigns.PauseCampaign
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Pauses a campaign for the specified Amazon Connect account.
module Amazonka.ConnectCampaigns.PauseCampaign
  ( -- * Creating a Request
    PauseCampaign (..),
    newPauseCampaign,

    -- * Request Lenses
    pauseCampaign_id,

    -- * Destructuring the Response
    PauseCampaignResponse (..),
    newPauseCampaignResponse,
  )
where

import Amazonka.ConnectCampaigns.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | PauseCampaignRequest
--
-- /See:/ 'newPauseCampaign' smart constructor.
data PauseCampaign = PauseCampaign'
  { id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PauseCampaign' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'pauseCampaign_id' - Undocumented member.
newPauseCampaign ::
  -- | 'id'
  Prelude.Text ->
  PauseCampaign
newPauseCampaign pId_ = PauseCampaign' {id = pId_}

-- | Undocumented member.
pauseCampaign_id :: Lens.Lens' PauseCampaign Prelude.Text
pauseCampaign_id = Lens.lens (\PauseCampaign' {id} -> id) (\s@PauseCampaign' {} a -> s {id = a} :: PauseCampaign)

instance Core.AWSRequest PauseCampaign where
  type
    AWSResponse PauseCampaign =
      PauseCampaignResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull PauseCampaignResponse'

instance Prelude.Hashable PauseCampaign where
  hashWithSalt _salt PauseCampaign' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData PauseCampaign where
  rnf PauseCampaign' {..} = Prelude.rnf id

instance Data.ToHeaders PauseCampaign where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PauseCampaign where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath PauseCampaign where
  toPath PauseCampaign' {..} =
    Prelude.mconcat
      ["/campaigns/", Data.toBS id, "/pause"]

instance Data.ToQuery PauseCampaign where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPauseCampaignResponse' smart constructor.
data PauseCampaignResponse = PauseCampaignResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PauseCampaignResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPauseCampaignResponse ::
  PauseCampaignResponse
newPauseCampaignResponse = PauseCampaignResponse'

instance Prelude.NFData PauseCampaignResponse where
  rnf _ = ()
