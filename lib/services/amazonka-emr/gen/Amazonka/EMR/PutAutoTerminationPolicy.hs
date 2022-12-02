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
-- Module      : Amazonka.EMR.PutAutoTerminationPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Auto-termination is supported in Amazon EMR versions 5.30.0 and 6.1.0
-- and later. For more information, see
-- <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-auto-termination-policy.html Using an auto-termination policy>.
--
-- Creates or updates an auto-termination policy for an Amazon EMR cluster.
-- An auto-termination policy defines the amount of idle time in seconds
-- after which a cluster automatically terminates. For alternative cluster
-- termination options, see
-- <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-plan-termination.html Control cluster termination>.
module Amazonka.EMR.PutAutoTerminationPolicy
  ( -- * Creating a Request
    PutAutoTerminationPolicy (..),
    newPutAutoTerminationPolicy,

    -- * Request Lenses
    putAutoTerminationPolicy_autoTerminationPolicy,
    putAutoTerminationPolicy_clusterId,

    -- * Destructuring the Response
    PutAutoTerminationPolicyResponse (..),
    newPutAutoTerminationPolicyResponse,

    -- * Response Lenses
    putAutoTerminationPolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMR.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutAutoTerminationPolicy' smart constructor.
data PutAutoTerminationPolicy = PutAutoTerminationPolicy'
  { -- | Specifies the auto-termination policy to attach to the cluster.
    autoTerminationPolicy :: Prelude.Maybe AutoTerminationPolicy,
    -- | Specifies the ID of the Amazon EMR cluster to which the auto-termination
    -- policy will be attached.
    clusterId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutAutoTerminationPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoTerminationPolicy', 'putAutoTerminationPolicy_autoTerminationPolicy' - Specifies the auto-termination policy to attach to the cluster.
--
-- 'clusterId', 'putAutoTerminationPolicy_clusterId' - Specifies the ID of the Amazon EMR cluster to which the auto-termination
-- policy will be attached.
newPutAutoTerminationPolicy ::
  -- | 'clusterId'
  Prelude.Text ->
  PutAutoTerminationPolicy
newPutAutoTerminationPolicy pClusterId_ =
  PutAutoTerminationPolicy'
    { autoTerminationPolicy =
        Prelude.Nothing,
      clusterId = pClusterId_
    }

-- | Specifies the auto-termination policy to attach to the cluster.
putAutoTerminationPolicy_autoTerminationPolicy :: Lens.Lens' PutAutoTerminationPolicy (Prelude.Maybe AutoTerminationPolicy)
putAutoTerminationPolicy_autoTerminationPolicy = Lens.lens (\PutAutoTerminationPolicy' {autoTerminationPolicy} -> autoTerminationPolicy) (\s@PutAutoTerminationPolicy' {} a -> s {autoTerminationPolicy = a} :: PutAutoTerminationPolicy)

-- | Specifies the ID of the Amazon EMR cluster to which the auto-termination
-- policy will be attached.
putAutoTerminationPolicy_clusterId :: Lens.Lens' PutAutoTerminationPolicy Prelude.Text
putAutoTerminationPolicy_clusterId = Lens.lens (\PutAutoTerminationPolicy' {clusterId} -> clusterId) (\s@PutAutoTerminationPolicy' {} a -> s {clusterId = a} :: PutAutoTerminationPolicy)

instance Core.AWSRequest PutAutoTerminationPolicy where
  type
    AWSResponse PutAutoTerminationPolicy =
      PutAutoTerminationPolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutAutoTerminationPolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutAutoTerminationPolicy where
  hashWithSalt _salt PutAutoTerminationPolicy' {..} =
    _salt `Prelude.hashWithSalt` autoTerminationPolicy
      `Prelude.hashWithSalt` clusterId

instance Prelude.NFData PutAutoTerminationPolicy where
  rnf PutAutoTerminationPolicy' {..} =
    Prelude.rnf autoTerminationPolicy
      `Prelude.seq` Prelude.rnf clusterId

instance Data.ToHeaders PutAutoTerminationPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ElasticMapReduce.PutAutoTerminationPolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutAutoTerminationPolicy where
  toJSON PutAutoTerminationPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AutoTerminationPolicy" Data..=)
              Prelude.<$> autoTerminationPolicy,
            Prelude.Just ("ClusterId" Data..= clusterId)
          ]
      )

instance Data.ToPath PutAutoTerminationPolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery PutAutoTerminationPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutAutoTerminationPolicyResponse' smart constructor.
data PutAutoTerminationPolicyResponse = PutAutoTerminationPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutAutoTerminationPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putAutoTerminationPolicyResponse_httpStatus' - The response's http status code.
newPutAutoTerminationPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutAutoTerminationPolicyResponse
newPutAutoTerminationPolicyResponse pHttpStatus_ =
  PutAutoTerminationPolicyResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
putAutoTerminationPolicyResponse_httpStatus :: Lens.Lens' PutAutoTerminationPolicyResponse Prelude.Int
putAutoTerminationPolicyResponse_httpStatus = Lens.lens (\PutAutoTerminationPolicyResponse' {httpStatus} -> httpStatus) (\s@PutAutoTerminationPolicyResponse' {} a -> s {httpStatus = a} :: PutAutoTerminationPolicyResponse)

instance
  Prelude.NFData
    PutAutoTerminationPolicyResponse
  where
  rnf PutAutoTerminationPolicyResponse' {..} =
    Prelude.rnf httpStatus
