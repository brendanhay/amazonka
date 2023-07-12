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
-- Module      : Amazonka.SageMaker.StopAutoMLJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A method for forcing the termination of a running job.
module Amazonka.SageMaker.StopAutoMLJob
  ( -- * Creating a Request
    StopAutoMLJob (..),
    newStopAutoMLJob,

    -- * Request Lenses
    stopAutoMLJob_autoMLJobName,

    -- * Destructuring the Response
    StopAutoMLJobResponse (..),
    newStopAutoMLJobResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newStopAutoMLJob' smart constructor.
data StopAutoMLJob = StopAutoMLJob'
  { -- | The name of the object you are requesting.
    autoMLJobName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopAutoMLJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoMLJobName', 'stopAutoMLJob_autoMLJobName' - The name of the object you are requesting.
newStopAutoMLJob ::
  -- | 'autoMLJobName'
  Prelude.Text ->
  StopAutoMLJob
newStopAutoMLJob pAutoMLJobName_ =
  StopAutoMLJob' {autoMLJobName = pAutoMLJobName_}

-- | The name of the object you are requesting.
stopAutoMLJob_autoMLJobName :: Lens.Lens' StopAutoMLJob Prelude.Text
stopAutoMLJob_autoMLJobName = Lens.lens (\StopAutoMLJob' {autoMLJobName} -> autoMLJobName) (\s@StopAutoMLJob' {} a -> s {autoMLJobName = a} :: StopAutoMLJob)

instance Core.AWSRequest StopAutoMLJob where
  type
    AWSResponse StopAutoMLJob =
      StopAutoMLJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull StopAutoMLJobResponse'

instance Prelude.Hashable StopAutoMLJob where
  hashWithSalt _salt StopAutoMLJob' {..} =
    _salt `Prelude.hashWithSalt` autoMLJobName

instance Prelude.NFData StopAutoMLJob where
  rnf StopAutoMLJob' {..} = Prelude.rnf autoMLJobName

instance Data.ToHeaders StopAutoMLJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SageMaker.StopAutoMLJob" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopAutoMLJob where
  toJSON StopAutoMLJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("AutoMLJobName" Data..= autoMLJobName)
          ]
      )

instance Data.ToPath StopAutoMLJob where
  toPath = Prelude.const "/"

instance Data.ToQuery StopAutoMLJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopAutoMLJobResponse' smart constructor.
data StopAutoMLJobResponse = StopAutoMLJobResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopAutoMLJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newStopAutoMLJobResponse ::
  StopAutoMLJobResponse
newStopAutoMLJobResponse = StopAutoMLJobResponse'

instance Prelude.NFData StopAutoMLJobResponse where
  rnf _ = ()
