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
-- Module      : Amazonka.DrS.StartRecovery
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Launches Recovery Instances for the specified Source Servers. For each
-- Source Server you may choose a point in time snapshot to launch from, or
-- use an on demand snapshot.
module Amazonka.DrS.StartRecovery
  ( -- * Creating a Request
    StartRecovery (..),
    newStartRecovery,

    -- * Request Lenses
    startRecovery_isDrill,
    startRecovery_tags,
    startRecovery_sourceServers,

    -- * Destructuring the Response
    StartRecoveryResponse (..),
    newStartRecoveryResponse,

    -- * Response Lenses
    startRecoveryResponse_job,
    startRecoveryResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DrS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartRecovery' smart constructor.
data StartRecovery = StartRecovery'
  { -- | Whether this Source Server Recovery operation is a drill or not.
    isDrill :: Prelude.Maybe Prelude.Bool,
    -- | The tags to be associated with the Recovery Job.
    tags :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | The Source Servers that we want to start a Recovery Job for.
    sourceServers :: Prelude.NonEmpty StartRecoveryRequestSourceServer
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartRecovery' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isDrill', 'startRecovery_isDrill' - Whether this Source Server Recovery operation is a drill or not.
--
-- 'tags', 'startRecovery_tags' - The tags to be associated with the Recovery Job.
--
-- 'sourceServers', 'startRecovery_sourceServers' - The Source Servers that we want to start a Recovery Job for.
newStartRecovery ::
  -- | 'sourceServers'
  Prelude.NonEmpty StartRecoveryRequestSourceServer ->
  StartRecovery
newStartRecovery pSourceServers_ =
  StartRecovery'
    { isDrill = Prelude.Nothing,
      tags = Prelude.Nothing,
      sourceServers = Lens.coerced Lens.# pSourceServers_
    }

-- | Whether this Source Server Recovery operation is a drill or not.
startRecovery_isDrill :: Lens.Lens' StartRecovery (Prelude.Maybe Prelude.Bool)
startRecovery_isDrill = Lens.lens (\StartRecovery' {isDrill} -> isDrill) (\s@StartRecovery' {} a -> s {isDrill = a} :: StartRecovery)

-- | The tags to be associated with the Recovery Job.
startRecovery_tags :: Lens.Lens' StartRecovery (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
startRecovery_tags = Lens.lens (\StartRecovery' {tags} -> tags) (\s@StartRecovery' {} a -> s {tags = a} :: StartRecovery) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | The Source Servers that we want to start a Recovery Job for.
startRecovery_sourceServers :: Lens.Lens' StartRecovery (Prelude.NonEmpty StartRecoveryRequestSourceServer)
startRecovery_sourceServers = Lens.lens (\StartRecovery' {sourceServers} -> sourceServers) (\s@StartRecovery' {} a -> s {sourceServers = a} :: StartRecovery) Prelude.. Lens.coerced

instance Core.AWSRequest StartRecovery where
  type
    AWSResponse StartRecovery =
      StartRecoveryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartRecoveryResponse'
            Prelude.<$> (x Data..?> "job")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartRecovery where
  hashWithSalt _salt StartRecovery' {..} =
    _salt
      `Prelude.hashWithSalt` isDrill
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` sourceServers

instance Prelude.NFData StartRecovery where
  rnf StartRecovery' {..} =
    Prelude.rnf isDrill
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf sourceServers

instance Data.ToHeaders StartRecovery where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartRecovery where
  toJSON StartRecovery' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("isDrill" Data..=) Prelude.<$> isDrill,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("sourceServers" Data..= sourceServers)
          ]
      )

instance Data.ToPath StartRecovery where
  toPath = Prelude.const "/StartRecovery"

instance Data.ToQuery StartRecovery where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartRecoveryResponse' smart constructor.
data StartRecoveryResponse = StartRecoveryResponse'
  { -- | The Recovery Job.
    job :: Prelude.Maybe Job,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartRecoveryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'job', 'startRecoveryResponse_job' - The Recovery Job.
--
-- 'httpStatus', 'startRecoveryResponse_httpStatus' - The response's http status code.
newStartRecoveryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartRecoveryResponse
newStartRecoveryResponse pHttpStatus_ =
  StartRecoveryResponse'
    { job = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Recovery Job.
startRecoveryResponse_job :: Lens.Lens' StartRecoveryResponse (Prelude.Maybe Job)
startRecoveryResponse_job = Lens.lens (\StartRecoveryResponse' {job} -> job) (\s@StartRecoveryResponse' {} a -> s {job = a} :: StartRecoveryResponse)

-- | The response's http status code.
startRecoveryResponse_httpStatus :: Lens.Lens' StartRecoveryResponse Prelude.Int
startRecoveryResponse_httpStatus = Lens.lens (\StartRecoveryResponse' {httpStatus} -> httpStatus) (\s@StartRecoveryResponse' {} a -> s {httpStatus = a} :: StartRecoveryResponse)

instance Prelude.NFData StartRecoveryResponse where
  rnf StartRecoveryResponse' {..} =
    Prelude.rnf job
      `Prelude.seq` Prelude.rnf httpStatus
