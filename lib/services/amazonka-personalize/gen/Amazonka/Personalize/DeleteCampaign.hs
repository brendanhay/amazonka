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
-- Module      : Amazonka.Personalize.DeleteCampaign
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a campaign by deleting the solution deployment. The solution
-- that the campaign is based on is not deleted and can be redeployed when
-- needed. A deleted campaign can no longer be specified in a
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_RS_GetRecommendations.html GetRecommendations>
-- request. For information on creating campaigns, see
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_CreateCampaign.html CreateCampaign>.
module Amazonka.Personalize.DeleteCampaign
  ( -- * Creating a Request
    DeleteCampaign (..),
    newDeleteCampaign,

    -- * Request Lenses
    deleteCampaign_campaignArn,

    -- * Destructuring the Response
    DeleteCampaignResponse (..),
    newDeleteCampaignResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Personalize.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteCampaign' smart constructor.
data DeleteCampaign = DeleteCampaign'
  { -- | The Amazon Resource Name (ARN) of the campaign to delete.
    campaignArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCampaign' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'campaignArn', 'deleteCampaign_campaignArn' - The Amazon Resource Name (ARN) of the campaign to delete.
newDeleteCampaign ::
  -- | 'campaignArn'
  Prelude.Text ->
  DeleteCampaign
newDeleteCampaign pCampaignArn_ =
  DeleteCampaign' {campaignArn = pCampaignArn_}

-- | The Amazon Resource Name (ARN) of the campaign to delete.
deleteCampaign_campaignArn :: Lens.Lens' DeleteCampaign Prelude.Text
deleteCampaign_campaignArn = Lens.lens (\DeleteCampaign' {campaignArn} -> campaignArn) (\s@DeleteCampaign' {} a -> s {campaignArn = a} :: DeleteCampaign)

instance Core.AWSRequest DeleteCampaign where
  type
    AWSResponse DeleteCampaign =
      DeleteCampaignResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DeleteCampaignResponse'

instance Prelude.Hashable DeleteCampaign where
  hashWithSalt _salt DeleteCampaign' {..} =
    _salt `Prelude.hashWithSalt` campaignArn

instance Prelude.NFData DeleteCampaign where
  rnf DeleteCampaign' {..} = Prelude.rnf campaignArn

instance Data.ToHeaders DeleteCampaign where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonPersonalize.DeleteCampaign" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteCampaign where
  toJSON DeleteCampaign' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("campaignArn" Data..= campaignArn)]
      )

instance Data.ToPath DeleteCampaign where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteCampaign where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteCampaignResponse' smart constructor.
data DeleteCampaignResponse = DeleteCampaignResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCampaignResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteCampaignResponse ::
  DeleteCampaignResponse
newDeleteCampaignResponse = DeleteCampaignResponse'

instance Prelude.NFData DeleteCampaignResponse where
  rnf _ = ()
