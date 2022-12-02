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
-- Module      : Amazonka.Personalize.StopSolutionVersionCreation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops creating a solution version that is in a state of CREATE_PENDING
-- or CREATE IN_PROGRESS.
--
-- Depending on the current state of the solution version, the solution
-- version state changes as follows:
--
-- -   CREATE_PENDING > CREATE_STOPPED
--
--     or
--
-- -   CREATE_IN_PROGRESS > CREATE_STOPPING > CREATE_STOPPED
--
-- You are billed for all of the training completed up until you stop the
-- solution version creation. You cannot resume creating a solution version
-- once it has been stopped.
module Amazonka.Personalize.StopSolutionVersionCreation
  ( -- * Creating a Request
    StopSolutionVersionCreation (..),
    newStopSolutionVersionCreation,

    -- * Request Lenses
    stopSolutionVersionCreation_solutionVersionArn,

    -- * Destructuring the Response
    StopSolutionVersionCreationResponse (..),
    newStopSolutionVersionCreationResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Personalize.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStopSolutionVersionCreation' smart constructor.
data StopSolutionVersionCreation = StopSolutionVersionCreation'
  { -- | The Amazon Resource Name (ARN) of the solution version you want to stop
    -- creating.
    solutionVersionArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopSolutionVersionCreation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'solutionVersionArn', 'stopSolutionVersionCreation_solutionVersionArn' - The Amazon Resource Name (ARN) of the solution version you want to stop
-- creating.
newStopSolutionVersionCreation ::
  -- | 'solutionVersionArn'
  Prelude.Text ->
  StopSolutionVersionCreation
newStopSolutionVersionCreation pSolutionVersionArn_ =
  StopSolutionVersionCreation'
    { solutionVersionArn =
        pSolutionVersionArn_
    }

-- | The Amazon Resource Name (ARN) of the solution version you want to stop
-- creating.
stopSolutionVersionCreation_solutionVersionArn :: Lens.Lens' StopSolutionVersionCreation Prelude.Text
stopSolutionVersionCreation_solutionVersionArn = Lens.lens (\StopSolutionVersionCreation' {solutionVersionArn} -> solutionVersionArn) (\s@StopSolutionVersionCreation' {} a -> s {solutionVersionArn = a} :: StopSolutionVersionCreation)

instance Core.AWSRequest StopSolutionVersionCreation where
  type
    AWSResponse StopSolutionVersionCreation =
      StopSolutionVersionCreationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      StopSolutionVersionCreationResponse'

instance Prelude.Hashable StopSolutionVersionCreation where
  hashWithSalt _salt StopSolutionVersionCreation' {..} =
    _salt `Prelude.hashWithSalt` solutionVersionArn

instance Prelude.NFData StopSolutionVersionCreation where
  rnf StopSolutionVersionCreation' {..} =
    Prelude.rnf solutionVersionArn

instance Data.ToHeaders StopSolutionVersionCreation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonPersonalize.StopSolutionVersionCreation" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopSolutionVersionCreation where
  toJSON StopSolutionVersionCreation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("solutionVersionArn" Data..= solutionVersionArn)
          ]
      )

instance Data.ToPath StopSolutionVersionCreation where
  toPath = Prelude.const "/"

instance Data.ToQuery StopSolutionVersionCreation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopSolutionVersionCreationResponse' smart constructor.
data StopSolutionVersionCreationResponse = StopSolutionVersionCreationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopSolutionVersionCreationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newStopSolutionVersionCreationResponse ::
  StopSolutionVersionCreationResponse
newStopSolutionVersionCreationResponse =
  StopSolutionVersionCreationResponse'

instance
  Prelude.NFData
    StopSolutionVersionCreationResponse
  where
  rnf _ = ()
