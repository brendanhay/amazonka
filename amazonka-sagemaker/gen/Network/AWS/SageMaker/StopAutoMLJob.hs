{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SageMaker.StopAutoMLJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A method for forcing the termination of a running job.
module Network.AWS.SageMaker.StopAutoMLJob
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newStopAutoMLJob' smart constructor.
data StopAutoMLJob = StopAutoMLJob'
  { -- | The name of the object you are requesting.
    autoMLJobName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest StopAutoMLJob where
  type Rs StopAutoMLJob = StopAutoMLJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull StopAutoMLJobResponse'

instance Prelude.Hashable StopAutoMLJob

instance Prelude.NFData StopAutoMLJob

instance Prelude.ToHeaders StopAutoMLJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("SageMaker.StopAutoMLJob" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON StopAutoMLJob where
  toJSON StopAutoMLJob' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("AutoMLJobName" Prelude..= autoMLJobName)
          ]
      )

instance Prelude.ToPath StopAutoMLJob where
  toPath = Prelude.const "/"

instance Prelude.ToQuery StopAutoMLJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopAutoMLJobResponse' smart constructor.
data StopAutoMLJobResponse = StopAutoMLJobResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StopAutoMLJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newStopAutoMLJobResponse ::
  StopAutoMLJobResponse
newStopAutoMLJobResponse = StopAutoMLJobResponse'

instance Prelude.NFData StopAutoMLJobResponse
