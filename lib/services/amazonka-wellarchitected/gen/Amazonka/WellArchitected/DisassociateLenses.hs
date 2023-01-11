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
-- Module      : Amazonka.WellArchitected.DisassociateLenses
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociate a lens from a workload.
--
-- Up to 10 lenses can be disassociated from a workload in a single API
-- operation.
--
-- The Amazon Web Services Well-Architected Framework lens
-- (@wellarchitected@) cannot be removed from a workload.
module Amazonka.WellArchitected.DisassociateLenses
  ( -- * Creating a Request
    DisassociateLenses (..),
    newDisassociateLenses,

    -- * Request Lenses
    disassociateLenses_workloadId,
    disassociateLenses_lensAliases,

    -- * Destructuring the Response
    DisassociateLensesResponse (..),
    newDisassociateLensesResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WellArchitected.Types

-- | Input to disassociate lens reviews.
--
-- /See:/ 'newDisassociateLenses' smart constructor.
data DisassociateLenses = DisassociateLenses'
  { workloadId :: Prelude.Text,
    lensAliases :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateLenses' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workloadId', 'disassociateLenses_workloadId' - Undocumented member.
--
-- 'lensAliases', 'disassociateLenses_lensAliases' - Undocumented member.
newDisassociateLenses ::
  -- | 'workloadId'
  Prelude.Text ->
  -- | 'lensAliases'
  Prelude.NonEmpty Prelude.Text ->
  DisassociateLenses
newDisassociateLenses pWorkloadId_ pLensAliases_ =
  DisassociateLenses'
    { workloadId = pWorkloadId_,
      lensAliases = Lens.coerced Lens.# pLensAliases_
    }

-- | Undocumented member.
disassociateLenses_workloadId :: Lens.Lens' DisassociateLenses Prelude.Text
disassociateLenses_workloadId = Lens.lens (\DisassociateLenses' {workloadId} -> workloadId) (\s@DisassociateLenses' {} a -> s {workloadId = a} :: DisassociateLenses)

-- | Undocumented member.
disassociateLenses_lensAliases :: Lens.Lens' DisassociateLenses (Prelude.NonEmpty Prelude.Text)
disassociateLenses_lensAliases = Lens.lens (\DisassociateLenses' {lensAliases} -> lensAliases) (\s@DisassociateLenses' {} a -> s {lensAliases = a} :: DisassociateLenses) Prelude.. Lens.coerced

instance Core.AWSRequest DisassociateLenses where
  type
    AWSResponse DisassociateLenses =
      DisassociateLensesResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveNull DisassociateLensesResponse'

instance Prelude.Hashable DisassociateLenses where
  hashWithSalt _salt DisassociateLenses' {..} =
    _salt `Prelude.hashWithSalt` workloadId
      `Prelude.hashWithSalt` lensAliases

instance Prelude.NFData DisassociateLenses where
  rnf DisassociateLenses' {..} =
    Prelude.rnf workloadId
      `Prelude.seq` Prelude.rnf lensAliases

instance Data.ToHeaders DisassociateLenses where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DisassociateLenses where
  toJSON DisassociateLenses' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("LensAliases" Data..= lensAliases)]
      )

instance Data.ToPath DisassociateLenses where
  toPath DisassociateLenses' {..} =
    Prelude.mconcat
      [ "/workloads/",
        Data.toBS workloadId,
        "/disassociateLenses"
      ]

instance Data.ToQuery DisassociateLenses where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateLensesResponse' smart constructor.
data DisassociateLensesResponse = DisassociateLensesResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateLensesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDisassociateLensesResponse ::
  DisassociateLensesResponse
newDisassociateLensesResponse =
  DisassociateLensesResponse'

instance Prelude.NFData DisassociateLensesResponse where
  rnf _ = ()
