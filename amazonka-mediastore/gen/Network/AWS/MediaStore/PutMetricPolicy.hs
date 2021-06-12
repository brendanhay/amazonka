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
-- Module      : Network.AWS.MediaStore.PutMetricPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The metric policy that you want to add to the container. A metric policy
-- allows AWS Elemental MediaStore to send metrics to Amazon CloudWatch. It
-- takes up to 20 minutes for the new policy to take effect.
module Network.AWS.MediaStore.PutMetricPolicy
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaStore.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutMetricPolicy' smart constructor.
data PutMetricPolicy = PutMetricPolicy'
  { -- | The name of the container that you want to add the metric policy to.
    containerName :: Core.Text,
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
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'metricPolicy'
  MetricPolicy ->
  PutMetricPolicy
newPutMetricPolicy pContainerName_ pMetricPolicy_ =
  PutMetricPolicy'
    { containerName = pContainerName_,
      metricPolicy = pMetricPolicy_
    }

-- | The name of the container that you want to add the metric policy to.
putMetricPolicy_containerName :: Lens.Lens' PutMetricPolicy Core.Text
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
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutMetricPolicyResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable PutMetricPolicy

instance Core.NFData PutMetricPolicy

instance Core.ToHeaders PutMetricPolicy where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "MediaStore_20170901.PutMetricPolicy" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON PutMetricPolicy where
  toJSON PutMetricPolicy' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ContainerName" Core..= containerName),
            Core.Just ("MetricPolicy" Core..= metricPolicy)
          ]
      )

instance Core.ToPath PutMetricPolicy where
  toPath = Core.const "/"

instance Core.ToQuery PutMetricPolicy where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newPutMetricPolicyResponse' smart constructor.
data PutMetricPolicyResponse = PutMetricPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  PutMetricPolicyResponse
newPutMetricPolicyResponse pHttpStatus_ =
  PutMetricPolicyResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
putMetricPolicyResponse_httpStatus :: Lens.Lens' PutMetricPolicyResponse Core.Int
putMetricPolicyResponse_httpStatus = Lens.lens (\PutMetricPolicyResponse' {httpStatus} -> httpStatus) (\s@PutMetricPolicyResponse' {} a -> s {httpStatus = a} :: PutMetricPolicyResponse)

instance Core.NFData PutMetricPolicyResponse
