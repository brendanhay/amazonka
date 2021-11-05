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
-- Module      : Network.AWS.MGN.StartCutover
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Launches a Cutover Instance for specific Source Servers. This command
-- starts a LAUNCH job whose initiatedBy property is StartCutover and
-- changes the SourceServer.lifeCycle.state property to CUTTING_OVER.
module Network.AWS.MGN.StartCutover
  ( -- * Creating a Request
    StartCutover (..),
    newStartCutover,

    -- * Request Lenses
    startCutover_tags,
    startCutover_sourceServerIDs,

    -- * Destructuring the Response
    StartCutoverResponse (..),
    newStartCutoverResponse,

    -- * Response Lenses
    startCutoverResponse_job,
    startCutoverResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MGN.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartCutover' smart constructor.
data StartCutover = StartCutover'
  { -- | Start Cutover by Tags.
    tags :: Prelude.Maybe (Core.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | Start Cutover by Source Server IDs.
    sourceServerIDs :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartCutover' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'startCutover_tags' - Start Cutover by Tags.
--
-- 'sourceServerIDs', 'startCutover_sourceServerIDs' - Start Cutover by Source Server IDs.
newStartCutover ::
  -- | 'sourceServerIDs'
  Prelude.NonEmpty Prelude.Text ->
  StartCutover
newStartCutover pSourceServerIDs_ =
  StartCutover'
    { tags = Prelude.Nothing,
      sourceServerIDs =
        Lens.coerced Lens.# pSourceServerIDs_
    }

-- | Start Cutover by Tags.
startCutover_tags :: Lens.Lens' StartCutover (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
startCutover_tags = Lens.lens (\StartCutover' {tags} -> tags) (\s@StartCutover' {} a -> s {tags = a} :: StartCutover) Prelude.. Lens.mapping (Core._Sensitive Prelude.. Lens.coerced)

-- | Start Cutover by Source Server IDs.
startCutover_sourceServerIDs :: Lens.Lens' StartCutover (Prelude.NonEmpty Prelude.Text)
startCutover_sourceServerIDs = Lens.lens (\StartCutover' {sourceServerIDs} -> sourceServerIDs) (\s@StartCutover' {} a -> s {sourceServerIDs = a} :: StartCutover) Prelude.. Lens.coerced

instance Core.AWSRequest StartCutover where
  type AWSResponse StartCutover = StartCutoverResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartCutoverResponse'
            Prelude.<$> (x Core..?> "job")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartCutover

instance Prelude.NFData StartCutover

instance Core.ToHeaders StartCutover where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StartCutover where
  toJSON StartCutover' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("tags" Core..=) Prelude.<$> tags,
            Prelude.Just
              ("sourceServerIDs" Core..= sourceServerIDs)
          ]
      )

instance Core.ToPath StartCutover where
  toPath = Prelude.const "/StartCutover"

instance Core.ToQuery StartCutover where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartCutoverResponse' smart constructor.
data StartCutoverResponse = StartCutoverResponse'
  { -- | Start Cutover Job response.
    job :: Prelude.Maybe Job,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartCutoverResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'job', 'startCutoverResponse_job' - Start Cutover Job response.
--
-- 'httpStatus', 'startCutoverResponse_httpStatus' - The response's http status code.
newStartCutoverResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartCutoverResponse
newStartCutoverResponse pHttpStatus_ =
  StartCutoverResponse'
    { job = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Start Cutover Job response.
startCutoverResponse_job :: Lens.Lens' StartCutoverResponse (Prelude.Maybe Job)
startCutoverResponse_job = Lens.lens (\StartCutoverResponse' {job} -> job) (\s@StartCutoverResponse' {} a -> s {job = a} :: StartCutoverResponse)

-- | The response's http status code.
startCutoverResponse_httpStatus :: Lens.Lens' StartCutoverResponse Prelude.Int
startCutoverResponse_httpStatus = Lens.lens (\StartCutoverResponse' {httpStatus} -> httpStatus) (\s@StartCutoverResponse' {} a -> s {httpStatus = a} :: StartCutoverResponse)

instance Prelude.NFData StartCutoverResponse
