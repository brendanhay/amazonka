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
-- Module      : Network.AWS.Synthetics.DeleteCanary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes the specified canary.
--
-- When you delete a canary, resources used and created by the canary are
-- not automatically deleted. After you delete a canary that you do not
-- intend to use again, you should also delete the following:
--
-- -   The Lambda functions and layers used by this canary. These have the
--     prefix @cwsyn-MyCanaryName @.
--
-- -   The CloudWatch alarms created for this canary. These alarms have a
--     name of @Synthetics-SharpDrop-Alarm-MyCanaryName @.
--
-- -   Amazon S3 objects and buckets, such as the canary\'s artifact
--     location.
--
-- -   IAM roles created for the canary. If they were created in the
--     console, these roles have the name
--     @ role\/service-role\/CloudWatchSyntheticsRole-MyCanaryName @.
--
-- -   CloudWatch Logs log groups created for the canary. These logs groups
--     have the name @\/aws\/lambda\/cwsyn-MyCanaryName @.
--
-- Before you delete a canary, you might want to use @GetCanary@ to display
-- the information about this canary. Make note of the information returned
-- by this operation so that you can delete these resources after you
-- delete the canary.
module Network.AWS.Synthetics.DeleteCanary
  ( -- * Creating a Request
    DeleteCanary (..),
    newDeleteCanary,

    -- * Request Lenses
    deleteCanary_name,

    -- * Destructuring the Response
    DeleteCanaryResponse (..),
    newDeleteCanaryResponse,

    -- * Response Lenses
    deleteCanaryResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Synthetics.Types

-- | /See:/ 'newDeleteCanary' smart constructor.
data DeleteCanary = DeleteCanary'
  { -- | The name of the canary that you want to delete. To find the names of
    -- your canaries, use
    -- <https://docs.aws.amazon.com/AmazonSynthetics/latest/APIReference/API_DescribeCanaries.html DescribeCanaries>.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCanary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteCanary_name' - The name of the canary that you want to delete. To find the names of
-- your canaries, use
-- <https://docs.aws.amazon.com/AmazonSynthetics/latest/APIReference/API_DescribeCanaries.html DescribeCanaries>.
newDeleteCanary ::
  -- | 'name'
  Prelude.Text ->
  DeleteCanary
newDeleteCanary pName_ = DeleteCanary' {name = pName_}

-- | The name of the canary that you want to delete. To find the names of
-- your canaries, use
-- <https://docs.aws.amazon.com/AmazonSynthetics/latest/APIReference/API_DescribeCanaries.html DescribeCanaries>.
deleteCanary_name :: Lens.Lens' DeleteCanary Prelude.Text
deleteCanary_name = Lens.lens (\DeleteCanary' {name} -> name) (\s@DeleteCanary' {} a -> s {name = a} :: DeleteCanary)

instance Core.AWSRequest DeleteCanary where
  type AWSResponse DeleteCanary = DeleteCanaryResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteCanaryResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteCanary

instance Prelude.NFData DeleteCanary

instance Core.ToHeaders DeleteCanary where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteCanary where
  toPath DeleteCanary' {..} =
    Prelude.mconcat ["/canary/", Core.toBS name]

instance Core.ToQuery DeleteCanary where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteCanaryResponse' smart constructor.
data DeleteCanaryResponse = DeleteCanaryResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCanaryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteCanaryResponse_httpStatus' - The response's http status code.
newDeleteCanaryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteCanaryResponse
newDeleteCanaryResponse pHttpStatus_ =
  DeleteCanaryResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteCanaryResponse_httpStatus :: Lens.Lens' DeleteCanaryResponse Prelude.Int
deleteCanaryResponse_httpStatus = Lens.lens (\DeleteCanaryResponse' {httpStatus} -> httpStatus) (\s@DeleteCanaryResponse' {} a -> s {httpStatus = a} :: DeleteCanaryResponse)

instance Prelude.NFData DeleteCanaryResponse
