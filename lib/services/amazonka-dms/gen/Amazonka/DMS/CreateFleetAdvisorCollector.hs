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
-- Module      : Amazonka.DMS.CreateFleetAdvisorCollector
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Fleet Advisor collector using the specified parameters.
module Amazonka.DMS.CreateFleetAdvisorCollector
  ( -- * Creating a Request
    CreateFleetAdvisorCollector (..),
    newCreateFleetAdvisorCollector,

    -- * Request Lenses
    createFleetAdvisorCollector_description,
    createFleetAdvisorCollector_collectorName,
    createFleetAdvisorCollector_serviceAccessRoleArn,
    createFleetAdvisorCollector_s3BucketName,

    -- * Destructuring the Response
    CreateFleetAdvisorCollectorResponse (..),
    newCreateFleetAdvisorCollectorResponse,

    -- * Response Lenses
    createFleetAdvisorCollectorResponse_collectorName,
    createFleetAdvisorCollectorResponse_collectorReferencedId,
    createFleetAdvisorCollectorResponse_description,
    createFleetAdvisorCollectorResponse_s3BucketName,
    createFleetAdvisorCollectorResponse_serviceAccessRoleArn,
    createFleetAdvisorCollectorResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DMS.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateFleetAdvisorCollector' smart constructor.
data CreateFleetAdvisorCollector = CreateFleetAdvisorCollector'
  { -- | A summary description of your Fleet Advisor collector.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of your Fleet Advisor collector (for example,
    -- @sample-collector@).
    collectorName :: Prelude.Text,
    -- | The IAM role that grants permissions to access the specified Amazon S3
    -- bucket.
    serviceAccessRoleArn :: Prelude.Text,
    -- | The Amazon S3 bucket that the Fleet Advisor collector uses to store
    -- inventory metadata.
    s3BucketName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFleetAdvisorCollector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createFleetAdvisorCollector_description' - A summary description of your Fleet Advisor collector.
--
-- 'collectorName', 'createFleetAdvisorCollector_collectorName' - The name of your Fleet Advisor collector (for example,
-- @sample-collector@).
--
-- 'serviceAccessRoleArn', 'createFleetAdvisorCollector_serviceAccessRoleArn' - The IAM role that grants permissions to access the specified Amazon S3
-- bucket.
--
-- 's3BucketName', 'createFleetAdvisorCollector_s3BucketName' - The Amazon S3 bucket that the Fleet Advisor collector uses to store
-- inventory metadata.
newCreateFleetAdvisorCollector ::
  -- | 'collectorName'
  Prelude.Text ->
  -- | 'serviceAccessRoleArn'
  Prelude.Text ->
  -- | 's3BucketName'
  Prelude.Text ->
  CreateFleetAdvisorCollector
newCreateFleetAdvisorCollector
  pCollectorName_
  pServiceAccessRoleArn_
  pS3BucketName_ =
    CreateFleetAdvisorCollector'
      { description =
          Prelude.Nothing,
        collectorName = pCollectorName_,
        serviceAccessRoleArn = pServiceAccessRoleArn_,
        s3BucketName = pS3BucketName_
      }

-- | A summary description of your Fleet Advisor collector.
createFleetAdvisorCollector_description :: Lens.Lens' CreateFleetAdvisorCollector (Prelude.Maybe Prelude.Text)
createFleetAdvisorCollector_description = Lens.lens (\CreateFleetAdvisorCollector' {description} -> description) (\s@CreateFleetAdvisorCollector' {} a -> s {description = a} :: CreateFleetAdvisorCollector)

-- | The name of your Fleet Advisor collector (for example,
-- @sample-collector@).
createFleetAdvisorCollector_collectorName :: Lens.Lens' CreateFleetAdvisorCollector Prelude.Text
createFleetAdvisorCollector_collectorName = Lens.lens (\CreateFleetAdvisorCollector' {collectorName} -> collectorName) (\s@CreateFleetAdvisorCollector' {} a -> s {collectorName = a} :: CreateFleetAdvisorCollector)

-- | The IAM role that grants permissions to access the specified Amazon S3
-- bucket.
createFleetAdvisorCollector_serviceAccessRoleArn :: Lens.Lens' CreateFleetAdvisorCollector Prelude.Text
createFleetAdvisorCollector_serviceAccessRoleArn = Lens.lens (\CreateFleetAdvisorCollector' {serviceAccessRoleArn} -> serviceAccessRoleArn) (\s@CreateFleetAdvisorCollector' {} a -> s {serviceAccessRoleArn = a} :: CreateFleetAdvisorCollector)

-- | The Amazon S3 bucket that the Fleet Advisor collector uses to store
-- inventory metadata.
createFleetAdvisorCollector_s3BucketName :: Lens.Lens' CreateFleetAdvisorCollector Prelude.Text
createFleetAdvisorCollector_s3BucketName = Lens.lens (\CreateFleetAdvisorCollector' {s3BucketName} -> s3BucketName) (\s@CreateFleetAdvisorCollector' {} a -> s {s3BucketName = a} :: CreateFleetAdvisorCollector)

instance Core.AWSRequest CreateFleetAdvisorCollector where
  type
    AWSResponse CreateFleetAdvisorCollector =
      CreateFleetAdvisorCollectorResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateFleetAdvisorCollectorResponse'
            Prelude.<$> (x Data..?> "CollectorName")
            Prelude.<*> (x Data..?> "CollectorReferencedId")
            Prelude.<*> (x Data..?> "Description")
            Prelude.<*> (x Data..?> "S3BucketName")
            Prelude.<*> (x Data..?> "ServiceAccessRoleArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateFleetAdvisorCollector where
  hashWithSalt _salt CreateFleetAdvisorCollector' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` collectorName
      `Prelude.hashWithSalt` serviceAccessRoleArn
      `Prelude.hashWithSalt` s3BucketName

instance Prelude.NFData CreateFleetAdvisorCollector where
  rnf CreateFleetAdvisorCollector' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf collectorName
      `Prelude.seq` Prelude.rnf serviceAccessRoleArn
      `Prelude.seq` Prelude.rnf s3BucketName

instance Data.ToHeaders CreateFleetAdvisorCollector where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonDMSv20160101.CreateFleetAdvisorCollector" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateFleetAdvisorCollector where
  toJSON CreateFleetAdvisorCollector' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            Prelude.Just ("CollectorName" Data..= collectorName),
            Prelude.Just
              ( "ServiceAccessRoleArn"
                  Data..= serviceAccessRoleArn
              ),
            Prelude.Just ("S3BucketName" Data..= s3BucketName)
          ]
      )

instance Data.ToPath CreateFleetAdvisorCollector where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateFleetAdvisorCollector where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateFleetAdvisorCollectorResponse' smart constructor.
data CreateFleetAdvisorCollectorResponse = CreateFleetAdvisorCollectorResponse'
  { -- | The name of the new Fleet Advisor collector.
    collectorName :: Prelude.Maybe Prelude.Text,
    -- | The unique ID of the new Fleet Advisor collector, for example:
    -- @22fda70c-40d5-4acf-b233-a495bd8eb7f5@
    collectorReferencedId :: Prelude.Maybe Prelude.Text,
    -- | A summary description of the Fleet Advisor collector.
    description :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 bucket that the collector uses to store inventory
    -- metadata.
    s3BucketName :: Prelude.Maybe Prelude.Text,
    -- | The IAM role that grants permissions to access the specified Amazon S3
    -- bucket.
    serviceAccessRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFleetAdvisorCollectorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'collectorName', 'createFleetAdvisorCollectorResponse_collectorName' - The name of the new Fleet Advisor collector.
--
-- 'collectorReferencedId', 'createFleetAdvisorCollectorResponse_collectorReferencedId' - The unique ID of the new Fleet Advisor collector, for example:
-- @22fda70c-40d5-4acf-b233-a495bd8eb7f5@
--
-- 'description', 'createFleetAdvisorCollectorResponse_description' - A summary description of the Fleet Advisor collector.
--
-- 's3BucketName', 'createFleetAdvisorCollectorResponse_s3BucketName' - The Amazon S3 bucket that the collector uses to store inventory
-- metadata.
--
-- 'serviceAccessRoleArn', 'createFleetAdvisorCollectorResponse_serviceAccessRoleArn' - The IAM role that grants permissions to access the specified Amazon S3
-- bucket.
--
-- 'httpStatus', 'createFleetAdvisorCollectorResponse_httpStatus' - The response's http status code.
newCreateFleetAdvisorCollectorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateFleetAdvisorCollectorResponse
newCreateFleetAdvisorCollectorResponse pHttpStatus_ =
  CreateFleetAdvisorCollectorResponse'
    { collectorName =
        Prelude.Nothing,
      collectorReferencedId =
        Prelude.Nothing,
      description = Prelude.Nothing,
      s3BucketName = Prelude.Nothing,
      serviceAccessRoleArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the new Fleet Advisor collector.
createFleetAdvisorCollectorResponse_collectorName :: Lens.Lens' CreateFleetAdvisorCollectorResponse (Prelude.Maybe Prelude.Text)
createFleetAdvisorCollectorResponse_collectorName = Lens.lens (\CreateFleetAdvisorCollectorResponse' {collectorName} -> collectorName) (\s@CreateFleetAdvisorCollectorResponse' {} a -> s {collectorName = a} :: CreateFleetAdvisorCollectorResponse)

-- | The unique ID of the new Fleet Advisor collector, for example:
-- @22fda70c-40d5-4acf-b233-a495bd8eb7f5@
createFleetAdvisorCollectorResponse_collectorReferencedId :: Lens.Lens' CreateFleetAdvisorCollectorResponse (Prelude.Maybe Prelude.Text)
createFleetAdvisorCollectorResponse_collectorReferencedId = Lens.lens (\CreateFleetAdvisorCollectorResponse' {collectorReferencedId} -> collectorReferencedId) (\s@CreateFleetAdvisorCollectorResponse' {} a -> s {collectorReferencedId = a} :: CreateFleetAdvisorCollectorResponse)

-- | A summary description of the Fleet Advisor collector.
createFleetAdvisorCollectorResponse_description :: Lens.Lens' CreateFleetAdvisorCollectorResponse (Prelude.Maybe Prelude.Text)
createFleetAdvisorCollectorResponse_description = Lens.lens (\CreateFleetAdvisorCollectorResponse' {description} -> description) (\s@CreateFleetAdvisorCollectorResponse' {} a -> s {description = a} :: CreateFleetAdvisorCollectorResponse)

-- | The Amazon S3 bucket that the collector uses to store inventory
-- metadata.
createFleetAdvisorCollectorResponse_s3BucketName :: Lens.Lens' CreateFleetAdvisorCollectorResponse (Prelude.Maybe Prelude.Text)
createFleetAdvisorCollectorResponse_s3BucketName = Lens.lens (\CreateFleetAdvisorCollectorResponse' {s3BucketName} -> s3BucketName) (\s@CreateFleetAdvisorCollectorResponse' {} a -> s {s3BucketName = a} :: CreateFleetAdvisorCollectorResponse)

-- | The IAM role that grants permissions to access the specified Amazon S3
-- bucket.
createFleetAdvisorCollectorResponse_serviceAccessRoleArn :: Lens.Lens' CreateFleetAdvisorCollectorResponse (Prelude.Maybe Prelude.Text)
createFleetAdvisorCollectorResponse_serviceAccessRoleArn = Lens.lens (\CreateFleetAdvisorCollectorResponse' {serviceAccessRoleArn} -> serviceAccessRoleArn) (\s@CreateFleetAdvisorCollectorResponse' {} a -> s {serviceAccessRoleArn = a} :: CreateFleetAdvisorCollectorResponse)

-- | The response's http status code.
createFleetAdvisorCollectorResponse_httpStatus :: Lens.Lens' CreateFleetAdvisorCollectorResponse Prelude.Int
createFleetAdvisorCollectorResponse_httpStatus = Lens.lens (\CreateFleetAdvisorCollectorResponse' {httpStatus} -> httpStatus) (\s@CreateFleetAdvisorCollectorResponse' {} a -> s {httpStatus = a} :: CreateFleetAdvisorCollectorResponse)

instance
  Prelude.NFData
    CreateFleetAdvisorCollectorResponse
  where
  rnf CreateFleetAdvisorCollectorResponse' {..} =
    Prelude.rnf collectorName
      `Prelude.seq` Prelude.rnf collectorReferencedId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf s3BucketName
      `Prelude.seq` Prelude.rnf serviceAccessRoleArn
      `Prelude.seq` Prelude.rnf httpStatus
