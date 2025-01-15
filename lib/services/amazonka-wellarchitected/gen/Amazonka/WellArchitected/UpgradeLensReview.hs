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
-- Module      : Amazonka.WellArchitected.UpgradeLensReview
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Upgrade lens review.
module Amazonka.WellArchitected.UpgradeLensReview
  ( -- * Creating a Request
    UpgradeLensReview (..),
    newUpgradeLensReview,

    -- * Request Lenses
    upgradeLensReview_clientRequestToken,
    upgradeLensReview_workloadId,
    upgradeLensReview_lensAlias,
    upgradeLensReview_milestoneName,

    -- * Destructuring the Response
    UpgradeLensReviewResponse (..),
    newUpgradeLensReviewResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WellArchitected.Types

-- | /See:/ 'newUpgradeLensReview' smart constructor.
data UpgradeLensReview = UpgradeLensReview'
  { clientRequestToken :: Prelude.Maybe Prelude.Text,
    workloadId :: Prelude.Text,
    lensAlias :: Prelude.Text,
    milestoneName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpgradeLensReview' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'upgradeLensReview_clientRequestToken' - Undocumented member.
--
-- 'workloadId', 'upgradeLensReview_workloadId' - Undocumented member.
--
-- 'lensAlias', 'upgradeLensReview_lensAlias' - Undocumented member.
--
-- 'milestoneName', 'upgradeLensReview_milestoneName' - Undocumented member.
newUpgradeLensReview ::
  -- | 'workloadId'
  Prelude.Text ->
  -- | 'lensAlias'
  Prelude.Text ->
  -- | 'milestoneName'
  Prelude.Text ->
  UpgradeLensReview
newUpgradeLensReview
  pWorkloadId_
  pLensAlias_
  pMilestoneName_ =
    UpgradeLensReview'
      { clientRequestToken =
          Prelude.Nothing,
        workloadId = pWorkloadId_,
        lensAlias = pLensAlias_,
        milestoneName = pMilestoneName_
      }

-- | Undocumented member.
upgradeLensReview_clientRequestToken :: Lens.Lens' UpgradeLensReview (Prelude.Maybe Prelude.Text)
upgradeLensReview_clientRequestToken = Lens.lens (\UpgradeLensReview' {clientRequestToken} -> clientRequestToken) (\s@UpgradeLensReview' {} a -> s {clientRequestToken = a} :: UpgradeLensReview)

-- | Undocumented member.
upgradeLensReview_workloadId :: Lens.Lens' UpgradeLensReview Prelude.Text
upgradeLensReview_workloadId = Lens.lens (\UpgradeLensReview' {workloadId} -> workloadId) (\s@UpgradeLensReview' {} a -> s {workloadId = a} :: UpgradeLensReview)

-- | Undocumented member.
upgradeLensReview_lensAlias :: Lens.Lens' UpgradeLensReview Prelude.Text
upgradeLensReview_lensAlias = Lens.lens (\UpgradeLensReview' {lensAlias} -> lensAlias) (\s@UpgradeLensReview' {} a -> s {lensAlias = a} :: UpgradeLensReview)

-- | Undocumented member.
upgradeLensReview_milestoneName :: Lens.Lens' UpgradeLensReview Prelude.Text
upgradeLensReview_milestoneName = Lens.lens (\UpgradeLensReview' {milestoneName} -> milestoneName) (\s@UpgradeLensReview' {} a -> s {milestoneName = a} :: UpgradeLensReview)

instance Core.AWSRequest UpgradeLensReview where
  type
    AWSResponse UpgradeLensReview =
      UpgradeLensReviewResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveNull UpgradeLensReviewResponse'

instance Prelude.Hashable UpgradeLensReview where
  hashWithSalt _salt UpgradeLensReview' {..} =
    _salt
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` workloadId
      `Prelude.hashWithSalt` lensAlias
      `Prelude.hashWithSalt` milestoneName

instance Prelude.NFData UpgradeLensReview where
  rnf UpgradeLensReview' {..} =
    Prelude.rnf clientRequestToken `Prelude.seq`
      Prelude.rnf workloadId `Prelude.seq`
        Prelude.rnf lensAlias `Prelude.seq`
          Prelude.rnf milestoneName

instance Data.ToHeaders UpgradeLensReview where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpgradeLensReview where
  toJSON UpgradeLensReview' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            Prelude.Just
              ("MilestoneName" Data..= milestoneName)
          ]
      )

instance Data.ToPath UpgradeLensReview where
  toPath UpgradeLensReview' {..} =
    Prelude.mconcat
      [ "/workloads/",
        Data.toBS workloadId,
        "/lensReviews/",
        Data.toBS lensAlias,
        "/upgrade"
      ]

instance Data.ToQuery UpgradeLensReview where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpgradeLensReviewResponse' smart constructor.
data UpgradeLensReviewResponse = UpgradeLensReviewResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpgradeLensReviewResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpgradeLensReviewResponse ::
  UpgradeLensReviewResponse
newUpgradeLensReviewResponse =
  UpgradeLensReviewResponse'

instance Prelude.NFData UpgradeLensReviewResponse where
  rnf _ = ()
