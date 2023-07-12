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
-- Module      : Amazonka.ConnectCampaigns.StartCampaign
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a campaign for the specified Amazon Connect account.
module Amazonka.ConnectCampaigns.StartCampaign
  ( -- * Creating a Request
    StartCampaign (..),
    newStartCampaign,

    -- * Request Lenses
    startCampaign_id,

    -- * Destructuring the Response
    StartCampaignResponse (..),
    newStartCampaignResponse,
  )
where

import Amazonka.ConnectCampaigns.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | StartCampaignRequest
--
-- /See:/ 'newStartCampaign' smart constructor.
data StartCampaign = StartCampaign'
  { id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartCampaign' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'startCampaign_id' - Undocumented member.
newStartCampaign ::
  -- | 'id'
  Prelude.Text ->
  StartCampaign
newStartCampaign pId_ = StartCampaign' {id = pId_}

-- | Undocumented member.
startCampaign_id :: Lens.Lens' StartCampaign Prelude.Text
startCampaign_id = Lens.lens (\StartCampaign' {id} -> id) (\s@StartCampaign' {} a -> s {id = a} :: StartCampaign)

instance Core.AWSRequest StartCampaign where
  type
    AWSResponse StartCampaign =
      StartCampaignResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull StartCampaignResponse'

instance Prelude.Hashable StartCampaign where
  hashWithSalt _salt StartCampaign' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData StartCampaign where
  rnf StartCampaign' {..} = Prelude.rnf id

instance Data.ToHeaders StartCampaign where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartCampaign where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath StartCampaign where
  toPath StartCampaign' {..} =
    Prelude.mconcat
      ["/campaigns/", Data.toBS id, "/start"]

instance Data.ToQuery StartCampaign where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartCampaignResponse' smart constructor.
data StartCampaignResponse = StartCampaignResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartCampaignResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newStartCampaignResponse ::
  StartCampaignResponse
newStartCampaignResponse = StartCampaignResponse'

instance Prelude.NFData StartCampaignResponse where
  rnf _ = ()
