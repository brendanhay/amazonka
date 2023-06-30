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
-- Module      : Amazonka.GuardDuty.CreatePublishingDestination
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a publishing destination to export findings to. The resource to
-- export findings to must exist before you use this operation.
module Amazonka.GuardDuty.CreatePublishingDestination
  ( -- * Creating a Request
    CreatePublishingDestination (..),
    newCreatePublishingDestination,

    -- * Request Lenses
    createPublishingDestination_clientToken,
    createPublishingDestination_detectorId,
    createPublishingDestination_destinationType,
    createPublishingDestination_destinationProperties,

    -- * Destructuring the Response
    CreatePublishingDestinationResponse (..),
    newCreatePublishingDestinationResponse,

    -- * Response Lenses
    createPublishingDestinationResponse_httpStatus,
    createPublishingDestinationResponse_destinationId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreatePublishingDestination' smart constructor.
data CreatePublishingDestination = CreatePublishingDestination'
  { -- | The idempotency token for the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the GuardDuty detector associated with the publishing
    -- destination.
    detectorId :: Prelude.Text,
    -- | The type of resource for the publishing destination. Currently only
    -- Amazon S3 buckets are supported.
    destinationType :: DestinationType,
    -- | The properties of the publishing destination, including the ARNs for the
    -- destination and the KMS key used for encryption.
    destinationProperties :: DestinationProperties
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePublishingDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createPublishingDestination_clientToken' - The idempotency token for the request.
--
-- 'detectorId', 'createPublishingDestination_detectorId' - The ID of the GuardDuty detector associated with the publishing
-- destination.
--
-- 'destinationType', 'createPublishingDestination_destinationType' - The type of resource for the publishing destination. Currently only
-- Amazon S3 buckets are supported.
--
-- 'destinationProperties', 'createPublishingDestination_destinationProperties' - The properties of the publishing destination, including the ARNs for the
-- destination and the KMS key used for encryption.
newCreatePublishingDestination ::
  -- | 'detectorId'
  Prelude.Text ->
  -- | 'destinationType'
  DestinationType ->
  -- | 'destinationProperties'
  DestinationProperties ->
  CreatePublishingDestination
newCreatePublishingDestination
  pDetectorId_
  pDestinationType_
  pDestinationProperties_ =
    CreatePublishingDestination'
      { clientToken =
          Prelude.Nothing,
        detectorId = pDetectorId_,
        destinationType = pDestinationType_,
        destinationProperties =
          pDestinationProperties_
      }

-- | The idempotency token for the request.
createPublishingDestination_clientToken :: Lens.Lens' CreatePublishingDestination (Prelude.Maybe Prelude.Text)
createPublishingDestination_clientToken = Lens.lens (\CreatePublishingDestination' {clientToken} -> clientToken) (\s@CreatePublishingDestination' {} a -> s {clientToken = a} :: CreatePublishingDestination)

-- | The ID of the GuardDuty detector associated with the publishing
-- destination.
createPublishingDestination_detectorId :: Lens.Lens' CreatePublishingDestination Prelude.Text
createPublishingDestination_detectorId = Lens.lens (\CreatePublishingDestination' {detectorId} -> detectorId) (\s@CreatePublishingDestination' {} a -> s {detectorId = a} :: CreatePublishingDestination)

-- | The type of resource for the publishing destination. Currently only
-- Amazon S3 buckets are supported.
createPublishingDestination_destinationType :: Lens.Lens' CreatePublishingDestination DestinationType
createPublishingDestination_destinationType = Lens.lens (\CreatePublishingDestination' {destinationType} -> destinationType) (\s@CreatePublishingDestination' {} a -> s {destinationType = a} :: CreatePublishingDestination)

-- | The properties of the publishing destination, including the ARNs for the
-- destination and the KMS key used for encryption.
createPublishingDestination_destinationProperties :: Lens.Lens' CreatePublishingDestination DestinationProperties
createPublishingDestination_destinationProperties = Lens.lens (\CreatePublishingDestination' {destinationProperties} -> destinationProperties) (\s@CreatePublishingDestination' {} a -> s {destinationProperties = a} :: CreatePublishingDestination)

instance Core.AWSRequest CreatePublishingDestination where
  type
    AWSResponse CreatePublishingDestination =
      CreatePublishingDestinationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePublishingDestinationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "destinationId")
      )

instance Prelude.Hashable CreatePublishingDestination where
  hashWithSalt _salt CreatePublishingDestination' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` detectorId
      `Prelude.hashWithSalt` destinationType
      `Prelude.hashWithSalt` destinationProperties

instance Prelude.NFData CreatePublishingDestination where
  rnf CreatePublishingDestination' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf detectorId
      `Prelude.seq` Prelude.rnf destinationType
      `Prelude.seq` Prelude.rnf destinationProperties

instance Data.ToHeaders CreatePublishingDestination where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreatePublishingDestination where
  toJSON CreatePublishingDestination' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            Prelude.Just
              ("destinationType" Data..= destinationType),
            Prelude.Just
              ( "destinationProperties"
                  Data..= destinationProperties
              )
          ]
      )

instance Data.ToPath CreatePublishingDestination where
  toPath CreatePublishingDestination' {..} =
    Prelude.mconcat
      [ "/detector/",
        Data.toBS detectorId,
        "/publishingDestination"
      ]

instance Data.ToQuery CreatePublishingDestination where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreatePublishingDestinationResponse' smart constructor.
data CreatePublishingDestinationResponse = CreatePublishingDestinationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ID of the publishing destination that is created.
    destinationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePublishingDestinationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createPublishingDestinationResponse_httpStatus' - The response's http status code.
--
-- 'destinationId', 'createPublishingDestinationResponse_destinationId' - The ID of the publishing destination that is created.
newCreatePublishingDestinationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'destinationId'
  Prelude.Text ->
  CreatePublishingDestinationResponse
newCreatePublishingDestinationResponse
  pHttpStatus_
  pDestinationId_ =
    CreatePublishingDestinationResponse'
      { httpStatus =
          pHttpStatus_,
        destinationId = pDestinationId_
      }

-- | The response's http status code.
createPublishingDestinationResponse_httpStatus :: Lens.Lens' CreatePublishingDestinationResponse Prelude.Int
createPublishingDestinationResponse_httpStatus = Lens.lens (\CreatePublishingDestinationResponse' {httpStatus} -> httpStatus) (\s@CreatePublishingDestinationResponse' {} a -> s {httpStatus = a} :: CreatePublishingDestinationResponse)

-- | The ID of the publishing destination that is created.
createPublishingDestinationResponse_destinationId :: Lens.Lens' CreatePublishingDestinationResponse Prelude.Text
createPublishingDestinationResponse_destinationId = Lens.lens (\CreatePublishingDestinationResponse' {destinationId} -> destinationId) (\s@CreatePublishingDestinationResponse' {} a -> s {destinationId = a} :: CreatePublishingDestinationResponse)

instance
  Prelude.NFData
    CreatePublishingDestinationResponse
  where
  rnf CreatePublishingDestinationResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf destinationId
