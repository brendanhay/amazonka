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
-- Module      : Amazonka.ConnectCampaigns.StopCampaign
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a campaign for the specified Amazon Connect account.
module Amazonka.ConnectCampaigns.StopCampaign
  ( -- * Creating a Request
    StopCampaign (..),
    newStopCampaign,

    -- * Request Lenses
    stopCampaign_id,

    -- * Destructuring the Response
    StopCampaignResponse (..),
    newStopCampaignResponse,
  )
where

import Amazonka.ConnectCampaigns.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | StopCampaignRequest
--
-- /See:/ 'newStopCampaign' smart constructor.
data StopCampaign = StopCampaign'
  { id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopCampaign' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'stopCampaign_id' - Undocumented member.
newStopCampaign ::
  -- | 'id'
  Prelude.Text ->
  StopCampaign
newStopCampaign pId_ = StopCampaign' {id = pId_}

-- | Undocumented member.
stopCampaign_id :: Lens.Lens' StopCampaign Prelude.Text
stopCampaign_id = Lens.lens (\StopCampaign' {id} -> id) (\s@StopCampaign' {} a -> s {id = a} :: StopCampaign)

instance Core.AWSRequest StopCampaign where
  type AWSResponse StopCampaign = StopCampaignResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response = Response.receiveNull StopCampaignResponse'

instance Prelude.Hashable StopCampaign where
  hashWithSalt _salt StopCampaign' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData StopCampaign where
  rnf StopCampaign' {..} = Prelude.rnf id

instance Data.ToHeaders StopCampaign where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopCampaign where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath StopCampaign where
  toPath StopCampaign' {..} =
    Prelude.mconcat
      ["/campaigns/", Data.toBS id, "/stop"]

instance Data.ToQuery StopCampaign where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopCampaignResponse' smart constructor.
data StopCampaignResponse = StopCampaignResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopCampaignResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newStopCampaignResponse ::
  StopCampaignResponse
newStopCampaignResponse = StopCampaignResponse'

instance Prelude.NFData StopCampaignResponse where
  rnf _ = ()
