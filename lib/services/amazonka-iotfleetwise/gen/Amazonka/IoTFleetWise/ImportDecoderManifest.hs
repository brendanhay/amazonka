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
-- Module      : Amazonka.IoTFleetWise.ImportDecoderManifest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a decoder manifest using your existing CAN DBC file from your
-- local device.
module Amazonka.IoTFleetWise.ImportDecoderManifest
  ( -- * Creating a Request
    ImportDecoderManifest (..),
    newImportDecoderManifest,

    -- * Request Lenses
    importDecoderManifest_name,
    importDecoderManifest_networkFileDefinitions,

    -- * Destructuring the Response
    ImportDecoderManifestResponse (..),
    newImportDecoderManifestResponse,

    -- * Response Lenses
    importDecoderManifestResponse_httpStatus,
    importDecoderManifestResponse_name,
    importDecoderManifestResponse_arn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTFleetWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newImportDecoderManifest' smart constructor.
data ImportDecoderManifest = ImportDecoderManifest'
  { -- | The name of the decoder manifest to import.
    name :: Prelude.Text,
    -- | The file to load into an Amazon Web Services account.
    networkFileDefinitions :: [NetworkFileDefinition]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportDecoderManifest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'importDecoderManifest_name' - The name of the decoder manifest to import.
--
-- 'networkFileDefinitions', 'importDecoderManifest_networkFileDefinitions' - The file to load into an Amazon Web Services account.
newImportDecoderManifest ::
  -- | 'name'
  Prelude.Text ->
  ImportDecoderManifest
newImportDecoderManifest pName_ =
  ImportDecoderManifest'
    { name = pName_,
      networkFileDefinitions = Prelude.mempty
    }

-- | The name of the decoder manifest to import.
importDecoderManifest_name :: Lens.Lens' ImportDecoderManifest Prelude.Text
importDecoderManifest_name = Lens.lens (\ImportDecoderManifest' {name} -> name) (\s@ImportDecoderManifest' {} a -> s {name = a} :: ImportDecoderManifest)

-- | The file to load into an Amazon Web Services account.
importDecoderManifest_networkFileDefinitions :: Lens.Lens' ImportDecoderManifest [NetworkFileDefinition]
importDecoderManifest_networkFileDefinitions = Lens.lens (\ImportDecoderManifest' {networkFileDefinitions} -> networkFileDefinitions) (\s@ImportDecoderManifest' {} a -> s {networkFileDefinitions = a} :: ImportDecoderManifest) Prelude.. Lens.coerced

instance Core.AWSRequest ImportDecoderManifest where
  type
    AWSResponse ImportDecoderManifest =
      ImportDecoderManifestResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ImportDecoderManifestResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "name")
            Prelude.<*> (x Data..:> "arn")
      )

instance Prelude.Hashable ImportDecoderManifest where
  hashWithSalt _salt ImportDecoderManifest' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` networkFileDefinitions

instance Prelude.NFData ImportDecoderManifest where
  rnf ImportDecoderManifest' {..} =
    Prelude.rnf name `Prelude.seq`
      Prelude.rnf networkFileDefinitions

instance Data.ToHeaders ImportDecoderManifest where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "IoTAutobahnControlPlane.ImportDecoderManifest" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ImportDecoderManifest where
  toJSON ImportDecoderManifest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("name" Data..= name),
            Prelude.Just
              ( "networkFileDefinitions"
                  Data..= networkFileDefinitions
              )
          ]
      )

instance Data.ToPath ImportDecoderManifest where
  toPath = Prelude.const "/"

instance Data.ToQuery ImportDecoderManifest where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newImportDecoderManifestResponse' smart constructor.
data ImportDecoderManifestResponse = ImportDecoderManifestResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name of the imported decoder manifest.
    name :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the decoder manifest that was
    -- imported.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportDecoderManifestResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'importDecoderManifestResponse_httpStatus' - The response's http status code.
--
-- 'name', 'importDecoderManifestResponse_name' - The name of the imported decoder manifest.
--
-- 'arn', 'importDecoderManifestResponse_arn' - The Amazon Resource Name (ARN) of the decoder manifest that was
-- imported.
newImportDecoderManifestResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'name'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  ImportDecoderManifestResponse
newImportDecoderManifestResponse
  pHttpStatus_
  pName_
  pArn_ =
    ImportDecoderManifestResponse'
      { httpStatus =
          pHttpStatus_,
        name = pName_,
        arn = pArn_
      }

-- | The response's http status code.
importDecoderManifestResponse_httpStatus :: Lens.Lens' ImportDecoderManifestResponse Prelude.Int
importDecoderManifestResponse_httpStatus = Lens.lens (\ImportDecoderManifestResponse' {httpStatus} -> httpStatus) (\s@ImportDecoderManifestResponse' {} a -> s {httpStatus = a} :: ImportDecoderManifestResponse)

-- | The name of the imported decoder manifest.
importDecoderManifestResponse_name :: Lens.Lens' ImportDecoderManifestResponse Prelude.Text
importDecoderManifestResponse_name = Lens.lens (\ImportDecoderManifestResponse' {name} -> name) (\s@ImportDecoderManifestResponse' {} a -> s {name = a} :: ImportDecoderManifestResponse)

-- | The Amazon Resource Name (ARN) of the decoder manifest that was
-- imported.
importDecoderManifestResponse_arn :: Lens.Lens' ImportDecoderManifestResponse Prelude.Text
importDecoderManifestResponse_arn = Lens.lens (\ImportDecoderManifestResponse' {arn} -> arn) (\s@ImportDecoderManifestResponse' {} a -> s {arn = a} :: ImportDecoderManifestResponse)

instance Prelude.NFData ImportDecoderManifestResponse where
  rnf ImportDecoderManifestResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf name `Prelude.seq`
        Prelude.rnf arn
