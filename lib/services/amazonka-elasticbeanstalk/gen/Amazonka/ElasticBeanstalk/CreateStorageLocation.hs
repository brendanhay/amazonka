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
-- Module      : Amazonka.ElasticBeanstalk.CreateStorageLocation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a bucket in Amazon S3 to store application versions, logs, and
-- other files used by Elastic Beanstalk environments. The Elastic
-- Beanstalk console and EB CLI call this API the first time you create an
-- environment in a region. If the storage location already exists,
-- @CreateStorageLocation@ still returns the bucket name but does not
-- create a new bucket.
module Amazonka.ElasticBeanstalk.CreateStorageLocation
  ( -- * Creating a Request
    CreateStorageLocation (..),
    newCreateStorageLocation,

    -- * Destructuring the Response
    CreateStorageLocationResponse (..),
    newCreateStorageLocationResponse,

    -- * Response Lenses
    createStorageLocationResponse_s3Bucket,
    createStorageLocationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticBeanstalk.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateStorageLocation' smart constructor.
data CreateStorageLocation = CreateStorageLocation'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateStorageLocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newCreateStorageLocation ::
  CreateStorageLocation
newCreateStorageLocation = CreateStorageLocation'

instance Core.AWSRequest CreateStorageLocation where
  type
    AWSResponse CreateStorageLocation =
      CreateStorageLocationResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "CreateStorageLocationResult"
      ( \s h x ->
          CreateStorageLocationResponse'
            Prelude.<$> (x Data..@? "S3Bucket")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateStorageLocation where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData CreateStorageLocation where
  rnf _ = ()

instance Data.ToHeaders CreateStorageLocation where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateStorageLocation where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateStorageLocation where
  toQuery =
    Prelude.const
      ( Prelude.mconcat
          [ "Action"
              Data.=: ("CreateStorageLocation" :: Prelude.ByteString),
            "Version"
              Data.=: ("2010-12-01" :: Prelude.ByteString)
          ]
      )

-- | Results of a CreateStorageLocationResult call.
--
-- /See:/ 'newCreateStorageLocationResponse' smart constructor.
data CreateStorageLocationResponse = CreateStorageLocationResponse'
  { -- | The name of the Amazon S3 bucket created.
    s3Bucket :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateStorageLocationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Bucket', 'createStorageLocationResponse_s3Bucket' - The name of the Amazon S3 bucket created.
--
-- 'httpStatus', 'createStorageLocationResponse_httpStatus' - The response's http status code.
newCreateStorageLocationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateStorageLocationResponse
newCreateStorageLocationResponse pHttpStatus_ =
  CreateStorageLocationResponse'
    { s3Bucket =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the Amazon S3 bucket created.
createStorageLocationResponse_s3Bucket :: Lens.Lens' CreateStorageLocationResponse (Prelude.Maybe Prelude.Text)
createStorageLocationResponse_s3Bucket = Lens.lens (\CreateStorageLocationResponse' {s3Bucket} -> s3Bucket) (\s@CreateStorageLocationResponse' {} a -> s {s3Bucket = a} :: CreateStorageLocationResponse)

-- | The response's http status code.
createStorageLocationResponse_httpStatus :: Lens.Lens' CreateStorageLocationResponse Prelude.Int
createStorageLocationResponse_httpStatus = Lens.lens (\CreateStorageLocationResponse' {httpStatus} -> httpStatus) (\s@CreateStorageLocationResponse' {} a -> s {httpStatus = a} :: CreateStorageLocationResponse)

instance Prelude.NFData CreateStorageLocationResponse where
  rnf CreateStorageLocationResponse' {..} =
    Prelude.rnf s3Bucket
      `Prelude.seq` Prelude.rnf httpStatus
