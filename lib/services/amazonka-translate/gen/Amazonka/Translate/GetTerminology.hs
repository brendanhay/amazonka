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
-- Module      : Amazonka.Translate.GetTerminology
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a custom terminology.
module Amazonka.Translate.GetTerminology
  ( -- * Creating a Request
    GetTerminology (..),
    newGetTerminology,

    -- * Request Lenses
    getTerminology_terminologyDataFormat,
    getTerminology_name,

    -- * Destructuring the Response
    GetTerminologyResponse (..),
    newGetTerminologyResponse,

    -- * Response Lenses
    getTerminologyResponse_terminologyDataLocation,
    getTerminologyResponse_auxiliaryDataLocation,
    getTerminologyResponse_terminologyProperties,
    getTerminologyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Translate.Types

-- | /See:/ 'newGetTerminology' smart constructor.
data GetTerminology = GetTerminology'
  { -- | The data format of the custom terminology being retrieved.
    --
    -- If you don\'t specify this parameter, Amazon Translate returns a file
    -- with the same format as the file that was imported to create the
    -- terminology.
    --
    -- If you specify this parameter when you retrieve a multi-directional
    -- terminology resource, you must specify the same format as the input file
    -- that was imported to create it. Otherwise, Amazon Translate throws an
    -- error.
    terminologyDataFormat :: Prelude.Maybe TerminologyDataFormat,
    -- | The name of the custom terminology being retrieved.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTerminology' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'terminologyDataFormat', 'getTerminology_terminologyDataFormat' - The data format of the custom terminology being retrieved.
--
-- If you don\'t specify this parameter, Amazon Translate returns a file
-- with the same format as the file that was imported to create the
-- terminology.
--
-- If you specify this parameter when you retrieve a multi-directional
-- terminology resource, you must specify the same format as the input file
-- that was imported to create it. Otherwise, Amazon Translate throws an
-- error.
--
-- 'name', 'getTerminology_name' - The name of the custom terminology being retrieved.
newGetTerminology ::
  -- | 'name'
  Prelude.Text ->
  GetTerminology
newGetTerminology pName_ =
  GetTerminology'
    { terminologyDataFormat =
        Prelude.Nothing,
      name = pName_
    }

-- | The data format of the custom terminology being retrieved.
--
-- If you don\'t specify this parameter, Amazon Translate returns a file
-- with the same format as the file that was imported to create the
-- terminology.
--
-- If you specify this parameter when you retrieve a multi-directional
-- terminology resource, you must specify the same format as the input file
-- that was imported to create it. Otherwise, Amazon Translate throws an
-- error.
getTerminology_terminologyDataFormat :: Lens.Lens' GetTerminology (Prelude.Maybe TerminologyDataFormat)
getTerminology_terminologyDataFormat = Lens.lens (\GetTerminology' {terminologyDataFormat} -> terminologyDataFormat) (\s@GetTerminology' {} a -> s {terminologyDataFormat = a} :: GetTerminology)

-- | The name of the custom terminology being retrieved.
getTerminology_name :: Lens.Lens' GetTerminology Prelude.Text
getTerminology_name = Lens.lens (\GetTerminology' {name} -> name) (\s@GetTerminology' {} a -> s {name = a} :: GetTerminology)

instance Core.AWSRequest GetTerminology where
  type
    AWSResponse GetTerminology =
      GetTerminologyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTerminologyResponse'
            Prelude.<$> (x Data..?> "TerminologyDataLocation")
            Prelude.<*> (x Data..?> "AuxiliaryDataLocation")
            Prelude.<*> (x Data..?> "TerminologyProperties")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetTerminology where
  hashWithSalt _salt GetTerminology' {..} =
    _salt `Prelude.hashWithSalt` terminologyDataFormat
      `Prelude.hashWithSalt` name

instance Prelude.NFData GetTerminology where
  rnf GetTerminology' {..} =
    Prelude.rnf terminologyDataFormat
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders GetTerminology where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSShineFrontendService_20170701.GetTerminology" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetTerminology where
  toJSON GetTerminology' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("TerminologyDataFormat" Data..=)
              Prelude.<$> terminologyDataFormat,
            Prelude.Just ("Name" Data..= name)
          ]
      )

instance Data.ToPath GetTerminology where
  toPath = Prelude.const "/"

instance Data.ToQuery GetTerminology where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetTerminologyResponse' smart constructor.
data GetTerminologyResponse = GetTerminologyResponse'
  { -- | The Amazon S3 location of the most recent custom terminology input file
    -- that was successfully imported into Amazon Translate. The location is
    -- returned as a presigned URL that has a 30-minute expiration.
    --
    -- Amazon Translate doesn\'t scan all input files for the risk of CSV
    -- injection attacks.
    --
    -- CSV injection occurs when a .csv or .tsv file is altered so that a
    -- record contains malicious code. The record begins with a special
    -- character, such as =, +, -, or \@. When the file is opened in a
    -- spreadsheet program, the program might interpret the record as a formula
    -- and run the code within it.
    --
    -- Before you download an input file from Amazon S3, ensure that you
    -- recognize the file and trust its creator.
    terminologyDataLocation :: Prelude.Maybe TerminologyDataLocation,
    -- | The Amazon S3 location of a file that provides any errors or warnings
    -- that were produced by your input file. This file was created when Amazon
    -- Translate attempted to create a terminology resource. The location is
    -- returned as a presigned URL to that has a 30-minute expiration.
    auxiliaryDataLocation :: Prelude.Maybe TerminologyDataLocation,
    -- | The properties of the custom terminology being retrieved.
    terminologyProperties :: Prelude.Maybe TerminologyProperties,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTerminologyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'terminologyDataLocation', 'getTerminologyResponse_terminologyDataLocation' - The Amazon S3 location of the most recent custom terminology input file
-- that was successfully imported into Amazon Translate. The location is
-- returned as a presigned URL that has a 30-minute expiration.
--
-- Amazon Translate doesn\'t scan all input files for the risk of CSV
-- injection attacks.
--
-- CSV injection occurs when a .csv or .tsv file is altered so that a
-- record contains malicious code. The record begins with a special
-- character, such as =, +, -, or \@. When the file is opened in a
-- spreadsheet program, the program might interpret the record as a formula
-- and run the code within it.
--
-- Before you download an input file from Amazon S3, ensure that you
-- recognize the file and trust its creator.
--
-- 'auxiliaryDataLocation', 'getTerminologyResponse_auxiliaryDataLocation' - The Amazon S3 location of a file that provides any errors or warnings
-- that were produced by your input file. This file was created when Amazon
-- Translate attempted to create a terminology resource. The location is
-- returned as a presigned URL to that has a 30-minute expiration.
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
      auxiliaryDataLocation = Prelude.Nothing,
      terminologyProperties = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon S3 location of the most recent custom terminology input file
-- that was successfully imported into Amazon Translate. The location is
-- returned as a presigned URL that has a 30-minute expiration.
--
-- Amazon Translate doesn\'t scan all input files for the risk of CSV
-- injection attacks.
--
-- CSV injection occurs when a .csv or .tsv file is altered so that a
-- record contains malicious code. The record begins with a special
-- character, such as =, +, -, or \@. When the file is opened in a
-- spreadsheet program, the program might interpret the record as a formula
-- and run the code within it.
--
-- Before you download an input file from Amazon S3, ensure that you
-- recognize the file and trust its creator.
getTerminologyResponse_terminologyDataLocation :: Lens.Lens' GetTerminologyResponse (Prelude.Maybe TerminologyDataLocation)
getTerminologyResponse_terminologyDataLocation = Lens.lens (\GetTerminologyResponse' {terminologyDataLocation} -> terminologyDataLocation) (\s@GetTerminologyResponse' {} a -> s {terminologyDataLocation = a} :: GetTerminologyResponse)

-- | The Amazon S3 location of a file that provides any errors or warnings
-- that were produced by your input file. This file was created when Amazon
-- Translate attempted to create a terminology resource. The location is
-- returned as a presigned URL to that has a 30-minute expiration.
getTerminologyResponse_auxiliaryDataLocation :: Lens.Lens' GetTerminologyResponse (Prelude.Maybe TerminologyDataLocation)
getTerminologyResponse_auxiliaryDataLocation = Lens.lens (\GetTerminologyResponse' {auxiliaryDataLocation} -> auxiliaryDataLocation) (\s@GetTerminologyResponse' {} a -> s {auxiliaryDataLocation = a} :: GetTerminologyResponse)

-- | The properties of the custom terminology being retrieved.
getTerminologyResponse_terminologyProperties :: Lens.Lens' GetTerminologyResponse (Prelude.Maybe TerminologyProperties)
getTerminologyResponse_terminologyProperties = Lens.lens (\GetTerminologyResponse' {terminologyProperties} -> terminologyProperties) (\s@GetTerminologyResponse' {} a -> s {terminologyProperties = a} :: GetTerminologyResponse)

-- | The response's http status code.
getTerminologyResponse_httpStatus :: Lens.Lens' GetTerminologyResponse Prelude.Int
getTerminologyResponse_httpStatus = Lens.lens (\GetTerminologyResponse' {httpStatus} -> httpStatus) (\s@GetTerminologyResponse' {} a -> s {httpStatus = a} :: GetTerminologyResponse)

instance Prelude.NFData GetTerminologyResponse where
  rnf GetTerminologyResponse' {..} =
    Prelude.rnf terminologyDataLocation
      `Prelude.seq` Prelude.rnf auxiliaryDataLocation
      `Prelude.seq` Prelude.rnf terminologyProperties
      `Prelude.seq` Prelude.rnf httpStatus
