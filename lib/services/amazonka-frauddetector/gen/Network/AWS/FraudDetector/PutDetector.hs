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
-- Module      : Network.AWS.FraudDetector.PutDetector
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a detector.
module Network.AWS.FraudDetector.PutDetector
  ( -- * Creating a Request
    PutDetector (..),
    newPutDetector,

    -- * Request Lenses
    putDetector_description,
    putDetector_tags,
    putDetector_detectorId,
    putDetector_eventTypeName,

    -- * Destructuring the Response
    PutDetectorResponse (..),
    newPutDetectorResponse,

    -- * Response Lenses
    putDetectorResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.FraudDetector.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutDetector' smart constructor.
data PutDetector = PutDetector'
  { -- | The description of the detector.
    description :: Prelude.Maybe Prelude.Text,
    -- | A collection of key and value pairs.
    tags :: Prelude.Maybe [Tag],
    -- | The detector ID.
    detectorId :: Prelude.Text,
    -- | The name of the event type.
    eventTypeName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutDetector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'putDetector_description' - The description of the detector.
--
-- 'tags', 'putDetector_tags' - A collection of key and value pairs.
--
-- 'detectorId', 'putDetector_detectorId' - The detector ID.
--
-- 'eventTypeName', 'putDetector_eventTypeName' - The name of the event type.
newPutDetector ::
  -- | 'detectorId'
  Prelude.Text ->
  -- | 'eventTypeName'
  Prelude.Text ->
  PutDetector
newPutDetector pDetectorId_ pEventTypeName_ =
  PutDetector'
    { description = Prelude.Nothing,
      tags = Prelude.Nothing,
      detectorId = pDetectorId_,
      eventTypeName = pEventTypeName_
    }

-- | The description of the detector.
putDetector_description :: Lens.Lens' PutDetector (Prelude.Maybe Prelude.Text)
putDetector_description = Lens.lens (\PutDetector' {description} -> description) (\s@PutDetector' {} a -> s {description = a} :: PutDetector)

-- | A collection of key and value pairs.
putDetector_tags :: Lens.Lens' PutDetector (Prelude.Maybe [Tag])
putDetector_tags = Lens.lens (\PutDetector' {tags} -> tags) (\s@PutDetector' {} a -> s {tags = a} :: PutDetector) Prelude.. Lens.mapping Lens.coerced

-- | The detector ID.
putDetector_detectorId :: Lens.Lens' PutDetector Prelude.Text
putDetector_detectorId = Lens.lens (\PutDetector' {detectorId} -> detectorId) (\s@PutDetector' {} a -> s {detectorId = a} :: PutDetector)

-- | The name of the event type.
putDetector_eventTypeName :: Lens.Lens' PutDetector Prelude.Text
putDetector_eventTypeName = Lens.lens (\PutDetector' {eventTypeName} -> eventTypeName) (\s@PutDetector' {} a -> s {eventTypeName = a} :: PutDetector)

instance Core.AWSRequest PutDetector where
  type AWSResponse PutDetector = PutDetectorResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutDetectorResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutDetector

instance Prelude.NFData PutDetector

instance Core.ToHeaders PutDetector where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSHawksNestServiceFacade.PutDetector" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON PutDetector where
  toJSON PutDetector' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("description" Core..=) Prelude.<$> description,
            ("tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("detectorId" Core..= detectorId),
            Prelude.Just
              ("eventTypeName" Core..= eventTypeName)
          ]
      )

instance Core.ToPath PutDetector where
  toPath = Prelude.const "/"

instance Core.ToQuery PutDetector where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutDetectorResponse' smart constructor.
data PutDetectorResponse = PutDetectorResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutDetectorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putDetectorResponse_httpStatus' - The response's http status code.
newPutDetectorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutDetectorResponse
newPutDetectorResponse pHttpStatus_ =
  PutDetectorResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
putDetectorResponse_httpStatus :: Lens.Lens' PutDetectorResponse Prelude.Int
putDetectorResponse_httpStatus = Lens.lens (\PutDetectorResponse' {httpStatus} -> httpStatus) (\s@PutDetectorResponse' {} a -> s {httpStatus = a} :: PutDetectorResponse)

instance Prelude.NFData PutDetectorResponse
