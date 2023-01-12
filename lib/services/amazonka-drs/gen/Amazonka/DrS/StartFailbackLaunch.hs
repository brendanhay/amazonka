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
-- Module      : Amazonka.DrS.StartFailbackLaunch
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates a Job for launching the machine that is being failed back to
-- from the specified Recovery Instance. This will run conversion on the
-- failback client and will reboot your machine, thus completing the
-- failback process.
module Amazonka.DrS.StartFailbackLaunch
  ( -- * Creating a Request
    StartFailbackLaunch (..),
    newStartFailbackLaunch,

    -- * Request Lenses
    startFailbackLaunch_tags,
    startFailbackLaunch_recoveryInstanceIDs,

    -- * Destructuring the Response
    StartFailbackLaunchResponse (..),
    newStartFailbackLaunchResponse,

    -- * Response Lenses
    startFailbackLaunchResponse_job,
    startFailbackLaunchResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DrS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartFailbackLaunch' smart constructor.
data StartFailbackLaunch = StartFailbackLaunch'
  { -- | The tags to be associated with the failback launch Job.
    tags :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | The IDs of the Recovery Instance whose failback launch we want to
    -- request.
    recoveryInstanceIDs :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartFailbackLaunch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'startFailbackLaunch_tags' - The tags to be associated with the failback launch Job.
--
-- 'recoveryInstanceIDs', 'startFailbackLaunch_recoveryInstanceIDs' - The IDs of the Recovery Instance whose failback launch we want to
-- request.
newStartFailbackLaunch ::
  -- | 'recoveryInstanceIDs'
  Prelude.NonEmpty Prelude.Text ->
  StartFailbackLaunch
newStartFailbackLaunch pRecoveryInstanceIDs_ =
  StartFailbackLaunch'
    { tags = Prelude.Nothing,
      recoveryInstanceIDs =
        Lens.coerced Lens.# pRecoveryInstanceIDs_
    }

-- | The tags to be associated with the failback launch Job.
startFailbackLaunch_tags :: Lens.Lens' StartFailbackLaunch (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
startFailbackLaunch_tags = Lens.lens (\StartFailbackLaunch' {tags} -> tags) (\s@StartFailbackLaunch' {} a -> s {tags = a} :: StartFailbackLaunch) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | The IDs of the Recovery Instance whose failback launch we want to
-- request.
startFailbackLaunch_recoveryInstanceIDs :: Lens.Lens' StartFailbackLaunch (Prelude.NonEmpty Prelude.Text)
startFailbackLaunch_recoveryInstanceIDs = Lens.lens (\StartFailbackLaunch' {recoveryInstanceIDs} -> recoveryInstanceIDs) (\s@StartFailbackLaunch' {} a -> s {recoveryInstanceIDs = a} :: StartFailbackLaunch) Prelude.. Lens.coerced

instance Core.AWSRequest StartFailbackLaunch where
  type
    AWSResponse StartFailbackLaunch =
      StartFailbackLaunchResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartFailbackLaunchResponse'
            Prelude.<$> (x Data..?> "job")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartFailbackLaunch where
  hashWithSalt _salt StartFailbackLaunch' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` recoveryInstanceIDs

instance Prelude.NFData StartFailbackLaunch where
  rnf StartFailbackLaunch' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf recoveryInstanceIDs

instance Data.ToHeaders StartFailbackLaunch where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartFailbackLaunch where
  toJSON StartFailbackLaunch' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("recoveryInstanceIDs" Data..= recoveryInstanceIDs)
          ]
      )

instance Data.ToPath StartFailbackLaunch where
  toPath = Prelude.const "/StartFailbackLaunch"

instance Data.ToQuery StartFailbackLaunch where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartFailbackLaunchResponse' smart constructor.
data StartFailbackLaunchResponse = StartFailbackLaunchResponse'
  { -- | The failback launch Job.
    job :: Prelude.Maybe Job,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartFailbackLaunchResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'job', 'startFailbackLaunchResponse_job' - The failback launch Job.
--
-- 'httpStatus', 'startFailbackLaunchResponse_httpStatus' - The response's http status code.
newStartFailbackLaunchResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartFailbackLaunchResponse
newStartFailbackLaunchResponse pHttpStatus_ =
  StartFailbackLaunchResponse'
    { job = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The failback launch Job.
startFailbackLaunchResponse_job :: Lens.Lens' StartFailbackLaunchResponse (Prelude.Maybe Job)
startFailbackLaunchResponse_job = Lens.lens (\StartFailbackLaunchResponse' {job} -> job) (\s@StartFailbackLaunchResponse' {} a -> s {job = a} :: StartFailbackLaunchResponse)

-- | The response's http status code.
startFailbackLaunchResponse_httpStatus :: Lens.Lens' StartFailbackLaunchResponse Prelude.Int
startFailbackLaunchResponse_httpStatus = Lens.lens (\StartFailbackLaunchResponse' {httpStatus} -> httpStatus) (\s@StartFailbackLaunchResponse' {} a -> s {httpStatus = a} :: StartFailbackLaunchResponse)

instance Prelude.NFData StartFailbackLaunchResponse where
  rnf StartFailbackLaunchResponse' {..} =
    Prelude.rnf job
      `Prelude.seq` Prelude.rnf httpStatus
