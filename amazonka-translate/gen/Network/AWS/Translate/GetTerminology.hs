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
-- Module      : Network.AWS.Translate.GetTerminology
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a custom terminology.
module Network.AWS.Translate.GetTerminology
  ( -- * Creating a Request
    GetTerminology (..),
    newGetTerminology,

    -- * Request Lenses
    getTerminology_name,
    getTerminology_terminologyDataFormat,

    -- * Destructuring the Response
    GetTerminologyResponse (..),
    newGetTerminologyResponse,

    -- * Response Lenses
    getTerminologyResponse_terminologyDataLocation,
    getTerminologyResponse_terminologyProperties,
    getTerminologyResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Translate.Types

-- | /See:/ 'newGetTerminology' smart constructor.
data GetTerminology = GetTerminology'
  { -- | The name of the custom terminology being retrieved.
    name :: Core.Text,
    -- | The data format of the custom terminology being retrieved, either CSV or
    -- TMX.
    terminologyDataFormat :: TerminologyDataFormat
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetTerminology' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'getTerminology_name' - The name of the custom terminology being retrieved.
--
-- 'terminologyDataFormat', 'getTerminology_terminologyDataFormat' - The data format of the custom terminology being retrieved, either CSV or
-- TMX.
newGetTerminology ::
  -- | 'name'
  Core.Text ->
  -- | 'terminologyDataFormat'
  TerminologyDataFormat ->
  GetTerminology
newGetTerminology pName_ pTerminologyDataFormat_ =
  GetTerminology'
    { name = pName_,
      terminologyDataFormat = pTerminologyDataFormat_
    }

-- | The name of the custom terminology being retrieved.
getTerminology_name :: Lens.Lens' GetTerminology Core.Text
getTerminology_name = Lens.lens (\GetTerminology' {name} -> name) (\s@GetTerminology' {} a -> s {name = a} :: GetTerminology)

-- | The data format of the custom terminology being retrieved, either CSV or
-- TMX.
getTerminology_terminologyDataFormat :: Lens.Lens' GetTerminology TerminologyDataFormat
getTerminology_terminologyDataFormat = Lens.lens (\GetTerminology' {terminologyDataFormat} -> terminologyDataFormat) (\s@GetTerminology' {} a -> s {terminologyDataFormat = a} :: GetTerminology)

instance Core.AWSRequest GetTerminology where
  type
    AWSResponse GetTerminology =
      GetTerminologyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTerminologyResponse'
            Core.<$> (x Core..?> "TerminologyDataLocation")
            Core.<*> (x Core..?> "TerminologyProperties")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetTerminology

instance Core.NFData GetTerminology

instance Core.ToHeaders GetTerminology where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSShineFrontendService_20170701.GetTerminology" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetTerminology where
  toJSON GetTerminology' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            Core.Just
              ( "TerminologyDataFormat"
                  Core..= terminologyDataFormat
              )
          ]
      )

instance Core.ToPath GetTerminology where
  toPath = Core.const "/"

instance Core.ToQuery GetTerminology where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetTerminologyResponse' smart constructor.
data GetTerminologyResponse = GetTerminologyResponse'
  { -- | The data location of the custom terminology being retrieved. The custom
    -- terminology file is returned in a presigned url that has a 30 minute
    -- expiration.
    terminologyDataLocation :: Core.Maybe TerminologyDataLocation,
    -- | The properties of the custom terminology being retrieved.
    terminologyProperties :: Core.Maybe TerminologyProperties,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetTerminologyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'terminologyDataLocation', 'getTerminologyResponse_terminologyDataLocation' - The data location of the custom terminology being retrieved. The custom
-- terminology file is returned in a presigned url that has a 30 minute
-- expiration.
--
-- 'terminologyProperties', 'getTerminologyResponse_terminologyProperties' - The properties of the custom terminology being retrieved.
--
-- 'httpStatus', 'getTerminologyResponse_httpStatus' - The response's http status code.
newGetTerminologyResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetTerminologyResponse
newGetTerminologyResponse pHttpStatus_ =
  GetTerminologyResponse'
    { terminologyDataLocation =
        Core.Nothing,
      terminologyProperties = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The data location of the custom terminology being retrieved. The custom
-- terminology file is returned in a presigned url that has a 30 minute
-- expiration.
getTerminologyResponse_terminologyDataLocation :: Lens.Lens' GetTerminologyResponse (Core.Maybe TerminologyDataLocation)
getTerminologyResponse_terminologyDataLocation = Lens.lens (\GetTerminologyResponse' {terminologyDataLocation} -> terminologyDataLocation) (\s@GetTerminologyResponse' {} a -> s {terminologyDataLocation = a} :: GetTerminologyResponse)

-- | The properties of the custom terminology being retrieved.
getTerminologyResponse_terminologyProperties :: Lens.Lens' GetTerminologyResponse (Core.Maybe TerminologyProperties)
getTerminologyResponse_terminologyProperties = Lens.lens (\GetTerminologyResponse' {terminologyProperties} -> terminologyProperties) (\s@GetTerminologyResponse' {} a -> s {terminologyProperties = a} :: GetTerminologyResponse)

-- | The response's http status code.
getTerminologyResponse_httpStatus :: Lens.Lens' GetTerminologyResponse Core.Int
getTerminologyResponse_httpStatus = Lens.lens (\GetTerminologyResponse' {httpStatus} -> httpStatus) (\s@GetTerminologyResponse' {} a -> s {httpStatus = a} :: GetTerminologyResponse)

instance Core.NFData GetTerminologyResponse
