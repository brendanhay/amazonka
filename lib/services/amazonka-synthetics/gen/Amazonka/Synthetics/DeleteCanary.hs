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
-- Module      : Amazonka.Synthetics.DeleteCanary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes the specified canary.
--
-- If you specify @DeleteLambda@ to @true@, CloudWatch Synthetics also
-- deletes the Lambda functions and layers that are used by the canary.
--
-- Other resources used and created by the canary are not automatically
-- deleted. After you delete a canary that you do not intend to use again,
-- you should also delete the following:
--
-- -   The CloudWatch alarms created for this canary. These alarms have a
--     name of @Synthetics-SharpDrop-Alarm-@/@MyCanaryName@/@ @.
--
-- -   Amazon S3 objects and buckets, such as the canary\'s artifact
--     location.
--
-- -   IAM roles created for the canary. If they were created in the
--     console, these roles have the name
--     @ role\/service-role\/CloudWatchSyntheticsRole-@/@MyCanaryName@/@ @.
--
-- -   CloudWatch Logs log groups created for the canary. These logs groups
--     have the name @\/aws\/lambda\/cwsyn-@/@MyCanaryName@/@ @.
--
-- Before you delete a canary, you might want to use @GetCanary@ to display
-- the information about this canary. Make note of the information returned
-- by this operation so that you can delete these resources after you
-- delete the canary.
module Amazonka.Synthetics.DeleteCanary
  ( -- * Creating a Request
    DeleteCanary (..),
    newDeleteCanary,

    -- * Request Lenses
    deleteCanary_deleteLambda,
    deleteCanary_name,

    -- * Destructuring the Response
    DeleteCanaryResponse (..),
    newDeleteCanaryResponse,

    -- * Response Lenses
    deleteCanaryResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Synthetics.Types

-- | /See:/ 'newDeleteCanary' smart constructor.
data DeleteCanary = DeleteCanary'
  { -- | Specifies whether to also delete the Lambda functions and layers used by
    -- this canary. The default is false.
    --
    -- Type: Boolean
    deleteLambda :: Prelude.Maybe Prelude.Bool,
    -- | The name of the canary that you want to delete. To find the names of
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
-- 'deleteLambda', 'deleteCanary_deleteLambda' - Specifies whether to also delete the Lambda functions and layers used by
-- this canary. The default is false.
--
-- Type: Boolean
--
-- 'name', 'deleteCanary_name' - The name of the canary that you want to delete. To find the names of
-- your canaries, use
-- <https://docs.aws.amazon.com/AmazonSynthetics/latest/APIReference/API_DescribeCanaries.html DescribeCanaries>.
newDeleteCanary ::
  -- | 'name'
  Prelude.Text ->
  DeleteCanary
newDeleteCanary pName_ =
  DeleteCanary'
    { deleteLambda = Prelude.Nothing,
      name = pName_
    }

-- | Specifies whether to also delete the Lambda functions and layers used by
-- this canary. The default is false.
--
-- Type: Boolean
deleteCanary_deleteLambda :: Lens.Lens' DeleteCanary (Prelude.Maybe Prelude.Bool)
deleteCanary_deleteLambda = Lens.lens (\DeleteCanary' {deleteLambda} -> deleteLambda) (\s@DeleteCanary' {} a -> s {deleteLambda = a} :: DeleteCanary)

-- | The name of the canary that you want to delete. To find the names of
-- your canaries, use
-- <https://docs.aws.amazon.com/AmazonSynthetics/latest/APIReference/API_DescribeCanaries.html DescribeCanaries>.
deleteCanary_name :: Lens.Lens' DeleteCanary Prelude.Text
deleteCanary_name = Lens.lens (\DeleteCanary' {name} -> name) (\s@DeleteCanary' {} a -> s {name = a} :: DeleteCanary)

instance Core.AWSRequest DeleteCanary where
  type AWSResponse DeleteCanary = DeleteCanaryResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteCanaryResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteCanary where
  hashWithSalt _salt DeleteCanary' {..} =
    _salt
      `Prelude.hashWithSalt` deleteLambda
      `Prelude.hashWithSalt` name

instance Prelude.NFData DeleteCanary where
  rnf DeleteCanary' {..} =
    Prelude.rnf deleteLambda
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders DeleteCanary where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteCanary where
  toPath DeleteCanary' {..} =
    Prelude.mconcat ["/canary/", Data.toBS name]

instance Data.ToQuery DeleteCanary where
  toQuery DeleteCanary' {..} =
    Prelude.mconcat
      ["deleteLambda" Data.=: deleteLambda]

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

instance Prelude.NFData DeleteCanaryResponse where
  rnf DeleteCanaryResponse' {..} =
    Prelude.rnf httpStatus
