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
-- Module      : Amazonka.MediaStore.PutMetricPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The metric policy that you want to add to the container. A metric policy
-- allows AWS Elemental MediaStore to send metrics to Amazon CloudWatch. It
-- takes up to 20 minutes for the new policy to take effect.
module Amazonka.MediaStore.PutMetricPolicy
  ( -- * Creating a Request
    PutMetricPolicy (..),
    newPutMetricPolicy,

    -- * Request Lenses
    putMetricPolicy_containerName,
    putMetricPolicy_metricPolicy,

    -- * Destructuring the Response
    PutMetricPolicyResponse (..),
    newPutMetricPolicyResponse,

    -- * Response Lenses
    putMetricPolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaStore.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutMetricPolicy' smart constructor.
data PutMetricPolicy = PutMetricPolicy'
  { -- | The name of the container that you want to add the metric policy to.
    containerName :: Prelude.Text,
    -- | The metric policy that you want to associate with the container. In the
    -- policy, you must indicate whether you want MediaStore to send
    -- container-level metrics. You can also include up to five rules to define
    -- groups of objects that you want MediaStore to send object-level metrics
    -- for. If you include rules in the policy, construct each rule with both
    -- of the following:
    --
    -- -   An object group that defines which objects to include in the group.
    --     The definition can be a path or a file name, but it can\'t have more
    --     than 900 characters. Valid characters are: a-z, A-Z, 0-9, _
    --     (underscore), = (equal), : (colon), . (period), - (hyphen), ~
    --     (tilde), \/ (forward slash), and * (asterisk). Wildcards (*) are
    --     acceptable.
    --
    -- -   An object group name that allows you to refer to the object group.
    --     The name can\'t have more than 30 characters. Valid characters are:
    --     a-z, A-Z, 0-9, and _ (underscore).
    metricPolicy :: MetricPolicy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutMetricPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containerName', 'putMetricPolicy_containerName' - The name of the container that you want to add the metric policy to.
--
-- 'metricPolicy', 'putMetricPolicy_metricPolicy' - The metric policy that you want to associate with the container. In the
-- policy, you must indicate whether you want MediaStore to send
-- container-level metrics. You can also include up to five rules to define
-- groups of objects that you want MediaStore to send object-level metrics
-- for. If you include rules in the policy, construct each rule with both
-- of the following:
--
-- -   An object group that defines which objects to include in the group.
--     The definition can be a path or a file name, but it can\'t have more
--     than 900 characters. Valid characters are: a-z, A-Z, 0-9, _
--     (underscore), = (equal), : (colon), . (period), - (hyphen), ~
--     (tilde), \/ (forward slash), and * (asterisk). Wildcards (*) are
--     acceptable.
--
-- -   An object group name that allows you to refer to the object group.
--     The name can\'t have more than 30 characters. Valid characters are:
--     a-z, A-Z, 0-9, and _ (underscore).
newPutMetricPolicy ::
  -- | 'containerName'
  Prelude.Text ->
  -- | 'metricPolicy'
  MetricPolicy ->
  PutMetricPolicy
newPutMetricPolicy pContainerName_ pMetricPolicy_ =
  PutMetricPolicy'
    { containerName = pContainerName_,
      metricPolicy = pMetricPolicy_
    }

-- | The name of the container that you want to add the metric policy to.
putMetricPolicy_containerName :: Lens.Lens' PutMetricPolicy Prelude.Text
putMetricPolicy_containerName = Lens.lens (\PutMetricPolicy' {containerName} -> containerName) (\s@PutMetricPolicy' {} a -> s {containerName = a} :: PutMetricPolicy)

-- | The metric policy that you want to associate with the container. In the
-- policy, you must indicate whether you want MediaStore to send
-- container-level metrics. You can also include up to five rules to define
-- groups of objects that you want MediaStore to send object-level metrics
-- for. If you include rules in the policy, construct each rule with both
-- of the following:
--
-- -   An object group that defines which objects to include in the group.
--     The definition can be a path or a file name, but it can\'t have more
--     than 900 characters. Valid characters are: a-z, A-Z, 0-9, _
--     (underscore), = (equal), : (colon), . (period), - (hyphen), ~
--     (tilde), \/ (forward slash), and * (asterisk). Wildcards (*) are
--     acceptable.
--
-- -   An object group name that allows you to refer to the object group.
--     The name can\'t have more than 30 characters. Valid characters are:
--     a-z, A-Z, 0-9, and _ (underscore).
putMetricPolicy_metricPolicy :: Lens.Lens' PutMetricPolicy MetricPolicy
putMetricPolicy_metricPolicy = Lens.lens (\PutMetricPolicy' {metricPolicy} -> metricPolicy) (\s@PutMetricPolicy' {} a -> s {metricPolicy = a} :: PutMetricPolicy)

instance Core.AWSRequest PutMetricPolicy where
  type
    AWSResponse PutMetricPolicy =
      PutMetricPolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutMetricPolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutMetricPolicy where
  hashWithSalt _salt PutMetricPolicy' {..} =
    _salt `Prelude.hashWithSalt` containerName
      `Prelude.hashWithSalt` metricPolicy

instance Prelude.NFData PutMetricPolicy where
  rnf PutMetricPolicy' {..} =
    Prelude.rnf containerName
      `Prelude.seq` Prelude.rnf metricPolicy

instance Data.ToHeaders PutMetricPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "MediaStore_20170901.PutMetricPolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutMetricPolicy where
  toJSON PutMetricPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ContainerName" Data..= containerName),
            Prelude.Just ("MetricPolicy" Data..= metricPolicy)
          ]
      )

instance Data.ToPath PutMetricPolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery PutMetricPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutMetricPolicyResponse' smart constructor.
data PutMetricPolicyResponse = PutMetricPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutMetricPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putMetricPolicyResponse_httpStatus' - The response's http status code.
newPutMetricPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutMetricPolicyResponse
newPutMetricPolicyResponse pHttpStatus_ =
  PutMetricPolicyResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
putMetricPolicyResponse_httpStatus :: Lens.Lens' PutMetricPolicyResponse Prelude.Int
putMetricPolicyResponse_httpStatus = Lens.lens (\PutMetricPolicyResponse' {httpStatus} -> httpStatus) (\s@PutMetricPolicyResponse' {} a -> s {httpStatus = a} :: PutMetricPolicyResponse)

instance Prelude.NFData PutMetricPolicyResponse where
  rnf PutMetricPolicyResponse' {..} =
    Prelude.rnf httpStatus
