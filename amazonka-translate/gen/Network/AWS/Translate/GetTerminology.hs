{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Translate.Types

-- | /See:/ 'newGetTerminology' smart constructor.
data GetTerminology = GetTerminology'
  { -- | The name of the custom terminology being retrieved.
    name :: Prelude.Text,
    -- | The data format of the custom terminology being retrieved, either CSV or
    -- TMX.
    terminologyDataFormat :: TerminologyDataFormat
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'terminologyDataFormat'
  TerminologyDataFormat ->
  GetTerminology
newGetTerminology pName_ pTerminologyDataFormat_ =
  GetTerminology'
    { name = pName_,
      terminologyDataFormat = pTerminologyDataFormat_
    }

-- | The name of the custom terminology being retrieved.
getTerminology_name :: Lens.Lens' GetTerminology Prelude.Text
getTerminology_name = Lens.lens (\GetTerminology' {name} -> name) (\s@GetTerminology' {} a -> s {name = a} :: GetTerminology)

-- | The data format of the custom terminology being retrieved, either CSV or
-- TMX.
getTerminology_terminologyDataFormat :: Lens.Lens' GetTerminology TerminologyDataFormat
getTerminology_terminologyDataFormat = Lens.lens (\GetTerminology' {terminologyDataFormat} -> terminologyDataFormat) (\s@GetTerminology' {} a -> s {terminologyDataFormat = a} :: GetTerminology)

instance Prelude.AWSRequest GetTerminology where
  type Rs GetTerminology = GetTerminologyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTerminologyResponse'
            Prelude.<$> (x Prelude..?> "TerminologyDataLocation")
            Prelude.<*> (x Prelude..?> "TerminologyProperties")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetTerminology

instance Prelude.NFData GetTerminology

instance Prelude.ToHeaders GetTerminology where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSShineFrontendService_20170701.GetTerminology" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetTerminology where
  toJSON GetTerminology' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Prelude..= name),
            Prelude.Just
              ( "TerminologyDataFormat"
                  Prelude..= terminologyDataFormat
              )
          ]
      )

instance Prelude.ToPath GetTerminology where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetTerminology where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetTerminologyResponse' smart constructor.
data GetTerminologyResponse = GetTerminologyResponse'
  { -- | The data location of the custom terminology being retrieved. The custom
    -- terminology file is returned in a presigned url that has a 30 minute
    -- expiration.
    terminologyDataLocation :: Prelude.Maybe TerminologyDataLocation,
    -- | The properties of the custom terminology being retrieved.
    terminologyProperties :: Prelude.Maybe TerminologyProperties,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  GetTerminologyResponse
newGetTerminologyResponse pHttpStatus_ =
  GetTerminologyResponse'
    { terminologyDataLocation =
        Prelude.Nothing,
      terminologyProperties = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The data location of the custom terminology being retrieved. The custom
-- terminology file is returned in a presigned url that has a 30 minute
-- expiration.
getTerminologyResponse_terminologyDataLocation :: Lens.Lens' GetTerminologyResponse (Prelude.Maybe TerminologyDataLocation)
getTerminologyResponse_terminologyDataLocation = Lens.lens (\GetTerminologyResponse' {terminologyDataLocation} -> terminologyDataLocation) (\s@GetTerminologyResponse' {} a -> s {terminologyDataLocation = a} :: GetTerminologyResponse)

-- | The properties of the custom terminology being retrieved.
getTerminologyResponse_terminologyProperties :: Lens.Lens' GetTerminologyResponse (Prelude.Maybe TerminologyProperties)
getTerminologyResponse_terminologyProperties = Lens.lens (\GetTerminologyResponse' {terminologyProperties} -> terminologyProperties) (\s@GetTerminologyResponse' {} a -> s {terminologyProperties = a} :: GetTerminologyResponse)

-- | The response's http status code.
getTerminologyResponse_httpStatus :: Lens.Lens' GetTerminologyResponse Prelude.Int
getTerminologyResponse_httpStatus = Lens.lens (\GetTerminologyResponse' {httpStatus} -> httpStatus) (\s@GetTerminologyResponse' {} a -> s {httpStatus = a} :: GetTerminologyResponse)

instance Prelude.NFData GetTerminologyResponse
