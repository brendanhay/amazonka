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
-- Module      : Amazonka.ConnectCampaigns.ResumeCampaign
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a campaign for the specified Amazon Connect account.
module Amazonka.ConnectCampaigns.ResumeCampaign
  ( -- * Creating a Request
    ResumeCampaign (..),
    newResumeCampaign,

    -- * Request Lenses
    resumeCampaign_id,

    -- * Destructuring the Response
    ResumeCampaignResponse (..),
    newResumeCampaignResponse,
  )
where

import Amazonka.ConnectCampaigns.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | ResumeCampaignRequest
--
-- /See:/ 'newResumeCampaign' smart constructor.
data ResumeCampaign = ResumeCampaign'
  { id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResumeCampaign' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'resumeCampaign_id' - Undocumented member.
newResumeCampaign ::
  -- | 'id'
  Prelude.Text ->
  ResumeCampaign
newResumeCampaign pId_ = ResumeCampaign' {id = pId_}

-- | Undocumented member.
resumeCampaign_id :: Lens.Lens' ResumeCampaign Prelude.Text
resumeCampaign_id = Lens.lens (\ResumeCampaign' {id} -> id) (\s@ResumeCampaign' {} a -> s {id = a} :: ResumeCampaign)

instance Core.AWSRequest ResumeCampaign where
  type
    AWSResponse ResumeCampaign =
      ResumeCampaignResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull ResumeCampaignResponse'

instance Prelude.Hashable ResumeCampaign where
  hashWithSalt _salt ResumeCampaign' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData ResumeCampaign where
  rnf ResumeCampaign' {..} = Prelude.rnf id

instance Data.ToHeaders ResumeCampaign where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ResumeCampaign where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath ResumeCampaign where
  toPath ResumeCampaign' {..} =
    Prelude.mconcat
      ["/campaigns/", Data.toBS id, "/resume"]

instance Data.ToQuery ResumeCampaign where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newResumeCampaignResponse' smart constructor.
data ResumeCampaignResponse = ResumeCampaignResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResumeCampaignResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newResumeCampaignResponse ::
  ResumeCampaignResponse
newResumeCampaignResponse = ResumeCampaignResponse'

instance Prelude.NFData ResumeCampaignResponse where
  rnf _ = ()
