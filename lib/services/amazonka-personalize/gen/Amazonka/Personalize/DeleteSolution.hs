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
-- Module      : Amazonka.Personalize.DeleteSolution
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes all versions of a solution and the @Solution@ object itself.
-- Before deleting a solution, you must delete all campaigns based on the
-- solution. To determine what campaigns are using the solution, call
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_ListCampaigns.html ListCampaigns>
-- and supply the Amazon Resource Name (ARN) of the solution. You can\'t
-- delete a solution if an associated @SolutionVersion@ is in the CREATE
-- PENDING or IN PROGRESS state. For more information on solutions, see
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_CreateSolution.html CreateSolution>.
module Amazonka.Personalize.DeleteSolution
  ( -- * Creating a Request
    DeleteSolution (..),
    newDeleteSolution,

    -- * Request Lenses
    deleteSolution_solutionArn,

    -- * Destructuring the Response
    DeleteSolutionResponse (..),
    newDeleteSolutionResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Personalize.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteSolution' smart constructor.
data DeleteSolution = DeleteSolution'
  { -- | The ARN of the solution to delete.
    solutionArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSolution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'solutionArn', 'deleteSolution_solutionArn' - The ARN of the solution to delete.
newDeleteSolution ::
  -- | 'solutionArn'
  Prelude.Text ->
  DeleteSolution
newDeleteSolution pSolutionArn_ =
  DeleteSolution' {solutionArn = pSolutionArn_}

-- | The ARN of the solution to delete.
deleteSolution_solutionArn :: Lens.Lens' DeleteSolution Prelude.Text
deleteSolution_solutionArn = Lens.lens (\DeleteSolution' {solutionArn} -> solutionArn) (\s@DeleteSolution' {} a -> s {solutionArn = a} :: DeleteSolution)

instance Core.AWSRequest DeleteSolution where
  type
    AWSResponse DeleteSolution =
      DeleteSolutionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DeleteSolutionResponse'

instance Prelude.Hashable DeleteSolution where
  hashWithSalt _salt DeleteSolution' {..} =
    _salt `Prelude.hashWithSalt` solutionArn

instance Prelude.NFData DeleteSolution where
  rnf DeleteSolution' {..} = Prelude.rnf solutionArn

instance Data.ToHeaders DeleteSolution where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonPersonalize.DeleteSolution" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteSolution where
  toJSON DeleteSolution' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("solutionArn" Data..= solutionArn)]
      )

instance Data.ToPath DeleteSolution where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteSolution where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteSolutionResponse' smart constructor.
data DeleteSolutionResponse = DeleteSolutionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSolutionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteSolutionResponse ::
  DeleteSolutionResponse
newDeleteSolutionResponse = DeleteSolutionResponse'

instance Prelude.NFData DeleteSolutionResponse where
  rnf _ = ()
