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
-- Module      : Amazonka.ConnectCampaigns.DescribeCampaign
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specific campaign.
module Amazonka.ConnectCampaigns.DescribeCampaign
  ( -- * Creating a Request
    DescribeCampaign (..),
    newDescribeCampaign,

    -- * Request Lenses
    describeCampaign_id,

    -- * Destructuring the Response
    DescribeCampaignResponse (..),
    newDescribeCampaignResponse,

    -- * Response Lenses
    describeCampaignResponse_campaign,
    describeCampaignResponse_httpStatus,
  )
where

import Amazonka.ConnectCampaigns.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | DescribeCampaignRequests
--
-- /See:/ 'newDescribeCampaign' smart constructor.
data DescribeCampaign = DescribeCampaign'
  { id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCampaign' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'describeCampaign_id' - Undocumented member.
newDescribeCampaign ::
  -- | 'id'
  Prelude.Text ->
  DescribeCampaign
newDescribeCampaign pId_ =
  DescribeCampaign' {id = pId_}

-- | Undocumented member.
describeCampaign_id :: Lens.Lens' DescribeCampaign Prelude.Text
describeCampaign_id = Lens.lens (\DescribeCampaign' {id} -> id) (\s@DescribeCampaign' {} a -> s {id = a} :: DescribeCampaign)

instance Core.AWSRequest DescribeCampaign where
  type
    AWSResponse DescribeCampaign =
      DescribeCampaignResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeCampaignResponse'
            Prelude.<$> (x Data..?> "campaign")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeCampaign where
  hashWithSalt _salt DescribeCampaign' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData DescribeCampaign where
  rnf DescribeCampaign' {..} = Prelude.rnf id

instance Data.ToHeaders DescribeCampaign where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeCampaign where
  toPath DescribeCampaign' {..} =
    Prelude.mconcat ["/campaigns/", Data.toBS id]

instance Data.ToQuery DescribeCampaign where
  toQuery = Prelude.const Prelude.mempty

-- | DescribeCampaignResponse
--
-- /See:/ 'newDescribeCampaignResponse' smart constructor.
data DescribeCampaignResponse = DescribeCampaignResponse'
  { campaign :: Prelude.Maybe Campaign,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCampaignResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'campaign', 'describeCampaignResponse_campaign' - Undocumented member.
--
-- 'httpStatus', 'describeCampaignResponse_httpStatus' - The response's http status code.
newDescribeCampaignResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeCampaignResponse
newDescribeCampaignResponse pHttpStatus_ =
  DescribeCampaignResponse'
    { campaign =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
describeCampaignResponse_campaign :: Lens.Lens' DescribeCampaignResponse (Prelude.Maybe Campaign)
describeCampaignResponse_campaign = Lens.lens (\DescribeCampaignResponse' {campaign} -> campaign) (\s@DescribeCampaignResponse' {} a -> s {campaign = a} :: DescribeCampaignResponse)

-- | The response's http status code.
describeCampaignResponse_httpStatus :: Lens.Lens' DescribeCampaignResponse Prelude.Int
describeCampaignResponse_httpStatus = Lens.lens (\DescribeCampaignResponse' {httpStatus} -> httpStatus) (\s@DescribeCampaignResponse' {} a -> s {httpStatus = a} :: DescribeCampaignResponse)

instance Prelude.NFData DescribeCampaignResponse where
  rnf DescribeCampaignResponse' {..} =
    Prelude.rnf campaign `Prelude.seq`
      Prelude.rnf httpStatus
