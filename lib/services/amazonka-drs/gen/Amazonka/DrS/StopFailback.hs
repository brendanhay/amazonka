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
-- Module      : Amazonka.DrS.StopFailback
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops the failback process for a specified Recovery Instance. This
-- changes the Failback State of the Recovery Instance back to
-- FAILBACK_NOT_STARTED.
module Amazonka.DrS.StopFailback
  ( -- * Creating a Request
    StopFailback (..),
    newStopFailback,

    -- * Request Lenses
    stopFailback_recoveryInstanceID,

    -- * Destructuring the Response
    StopFailbackResponse (..),
    newStopFailbackResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DrS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStopFailback' smart constructor.
data StopFailback = StopFailback'
  { -- | The ID of the Recovery Instance we want to stop failback for.
    recoveryInstanceID :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopFailback' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recoveryInstanceID', 'stopFailback_recoveryInstanceID' - The ID of the Recovery Instance we want to stop failback for.
newStopFailback ::
  -- | 'recoveryInstanceID'
  Prelude.Text ->
  StopFailback
newStopFailback pRecoveryInstanceID_ =
  StopFailback'
    { recoveryInstanceID =
        pRecoveryInstanceID_
    }

-- | The ID of the Recovery Instance we want to stop failback for.
stopFailback_recoveryInstanceID :: Lens.Lens' StopFailback Prelude.Text
stopFailback_recoveryInstanceID = Lens.lens (\StopFailback' {recoveryInstanceID} -> recoveryInstanceID) (\s@StopFailback' {} a -> s {recoveryInstanceID = a} :: StopFailback)

instance Core.AWSRequest StopFailback where
  type AWSResponse StopFailback = StopFailbackResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response = Response.receiveNull StopFailbackResponse'

instance Prelude.Hashable StopFailback where
  hashWithSalt _salt StopFailback' {..} =
    _salt `Prelude.hashWithSalt` recoveryInstanceID

instance Prelude.NFData StopFailback where
  rnf StopFailback' {..} =
    Prelude.rnf recoveryInstanceID

instance Data.ToHeaders StopFailback where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopFailback where
  toJSON StopFailback' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("recoveryInstanceID" Data..= recoveryInstanceID)
          ]
      )

instance Data.ToPath StopFailback where
  toPath = Prelude.const "/StopFailback"

instance Data.ToQuery StopFailback where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopFailbackResponse' smart constructor.
data StopFailbackResponse = StopFailbackResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopFailbackResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newStopFailbackResponse ::
  StopFailbackResponse
newStopFailbackResponse = StopFailbackResponse'

instance Prelude.NFData StopFailbackResponse where
  rnf _ = ()
