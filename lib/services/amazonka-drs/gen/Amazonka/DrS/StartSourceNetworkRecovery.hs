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
-- Module      : Amazonka.DrS.StartSourceNetworkRecovery
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deploy VPC for the specified Source Network and modify launch templates
-- to use this network. The VPC will be deployed using a dedicated
-- CloudFormation stack.
module Amazonka.DrS.StartSourceNetworkRecovery
  ( -- * Creating a Request
    StartSourceNetworkRecovery (..),
    newStartSourceNetworkRecovery,

    -- * Request Lenses
    startSourceNetworkRecovery_deployAsNew,
    startSourceNetworkRecovery_tags,
    startSourceNetworkRecovery_sourceNetworks,

    -- * Destructuring the Response
    StartSourceNetworkRecoveryResponse (..),
    newStartSourceNetworkRecoveryResponse,

    -- * Response Lenses
    startSourceNetworkRecoveryResponse_job,
    startSourceNetworkRecoveryResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DrS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartSourceNetworkRecovery' smart constructor.
data StartSourceNetworkRecovery = StartSourceNetworkRecovery'
  { -- | Don\'t update existing CloudFormation Stack, recover the network using a
    -- new stack.
    deployAsNew :: Prelude.Maybe Prelude.Bool,
    -- | The tags to be associated with the Source Network recovery Job.
    tags :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | The Source Networks that we want to start a Recovery Job for.
    sourceNetworks :: Prelude.NonEmpty StartSourceNetworkRecoveryRequestNetworkEntry
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartSourceNetworkRecovery' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deployAsNew', 'startSourceNetworkRecovery_deployAsNew' - Don\'t update existing CloudFormation Stack, recover the network using a
-- new stack.
--
-- 'tags', 'startSourceNetworkRecovery_tags' - The tags to be associated with the Source Network recovery Job.
--
-- 'sourceNetworks', 'startSourceNetworkRecovery_sourceNetworks' - The Source Networks that we want to start a Recovery Job for.
newStartSourceNetworkRecovery ::
  -- | 'sourceNetworks'
  Prelude.NonEmpty StartSourceNetworkRecoveryRequestNetworkEntry ->
  StartSourceNetworkRecovery
newStartSourceNetworkRecovery pSourceNetworks_ =
  StartSourceNetworkRecovery'
    { deployAsNew =
        Prelude.Nothing,
      tags = Prelude.Nothing,
      sourceNetworks =
        Lens.coerced Lens.# pSourceNetworks_
    }

-- | Don\'t update existing CloudFormation Stack, recover the network using a
-- new stack.
startSourceNetworkRecovery_deployAsNew :: Lens.Lens' StartSourceNetworkRecovery (Prelude.Maybe Prelude.Bool)
startSourceNetworkRecovery_deployAsNew = Lens.lens (\StartSourceNetworkRecovery' {deployAsNew} -> deployAsNew) (\s@StartSourceNetworkRecovery' {} a -> s {deployAsNew = a} :: StartSourceNetworkRecovery)

-- | The tags to be associated with the Source Network recovery Job.
startSourceNetworkRecovery_tags :: Lens.Lens' StartSourceNetworkRecovery (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
startSourceNetworkRecovery_tags = Lens.lens (\StartSourceNetworkRecovery' {tags} -> tags) (\s@StartSourceNetworkRecovery' {} a -> s {tags = a} :: StartSourceNetworkRecovery) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | The Source Networks that we want to start a Recovery Job for.
startSourceNetworkRecovery_sourceNetworks :: Lens.Lens' StartSourceNetworkRecovery (Prelude.NonEmpty StartSourceNetworkRecoveryRequestNetworkEntry)
startSourceNetworkRecovery_sourceNetworks = Lens.lens (\StartSourceNetworkRecovery' {sourceNetworks} -> sourceNetworks) (\s@StartSourceNetworkRecovery' {} a -> s {sourceNetworks = a} :: StartSourceNetworkRecovery) Prelude.. Lens.coerced

instance Core.AWSRequest StartSourceNetworkRecovery where
  type
    AWSResponse StartSourceNetworkRecovery =
      StartSourceNetworkRecoveryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartSourceNetworkRecoveryResponse'
            Prelude.<$> (x Data..?> "job")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartSourceNetworkRecovery where
  hashWithSalt _salt StartSourceNetworkRecovery' {..} =
    _salt
      `Prelude.hashWithSalt` deployAsNew
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` sourceNetworks

instance Prelude.NFData StartSourceNetworkRecovery where
  rnf StartSourceNetworkRecovery' {..} =
    Prelude.rnf deployAsNew
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf sourceNetworks

instance Data.ToHeaders StartSourceNetworkRecovery where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartSourceNetworkRecovery where
  toJSON StartSourceNetworkRecovery' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("deployAsNew" Data..=) Prelude.<$> deployAsNew,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("sourceNetworks" Data..= sourceNetworks)
          ]
      )

instance Data.ToPath StartSourceNetworkRecovery where
  toPath = Prelude.const "/StartSourceNetworkRecovery"

instance Data.ToQuery StartSourceNetworkRecovery where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartSourceNetworkRecoveryResponse' smart constructor.
data StartSourceNetworkRecoveryResponse = StartSourceNetworkRecoveryResponse'
  { -- | The Source Network recovery Job.
    job :: Prelude.Maybe Job,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartSourceNetworkRecoveryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'job', 'startSourceNetworkRecoveryResponse_job' - The Source Network recovery Job.
--
-- 'httpStatus', 'startSourceNetworkRecoveryResponse_httpStatus' - The response's http status code.
newStartSourceNetworkRecoveryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartSourceNetworkRecoveryResponse
newStartSourceNetworkRecoveryResponse pHttpStatus_ =
  StartSourceNetworkRecoveryResponse'
    { job =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Source Network recovery Job.
startSourceNetworkRecoveryResponse_job :: Lens.Lens' StartSourceNetworkRecoveryResponse (Prelude.Maybe Job)
startSourceNetworkRecoveryResponse_job = Lens.lens (\StartSourceNetworkRecoveryResponse' {job} -> job) (\s@StartSourceNetworkRecoveryResponse' {} a -> s {job = a} :: StartSourceNetworkRecoveryResponse)

-- | The response's http status code.
startSourceNetworkRecoveryResponse_httpStatus :: Lens.Lens' StartSourceNetworkRecoveryResponse Prelude.Int
startSourceNetworkRecoveryResponse_httpStatus = Lens.lens (\StartSourceNetworkRecoveryResponse' {httpStatus} -> httpStatus) (\s@StartSourceNetworkRecoveryResponse' {} a -> s {httpStatus = a} :: StartSourceNetworkRecoveryResponse)

instance
  Prelude.NFData
    StartSourceNetworkRecoveryResponse
  where
  rnf StartSourceNetworkRecoveryResponse' {..} =
    Prelude.rnf job
      `Prelude.seq` Prelude.rnf httpStatus
