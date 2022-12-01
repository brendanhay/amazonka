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
-- Module      : Amazonka.IoTFleetWise.CreateDecoderManifest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates the decoder manifest associated with a model manifest. To create
-- a decoder manifest, the following must be true:
--
-- -   Every signal decoder has a unique name.
--
-- -   Each signal decoder is associated with a network interface.
--
-- -   Each network interface has a unique ID.
--
-- -   The signal decoders are specified in the model manifest.
module Amazonka.IoTFleetWise.CreateDecoderManifest
  ( -- * Creating a Request
    CreateDecoderManifest (..),
    newCreateDecoderManifest,

    -- * Request Lenses
    createDecoderManifest_tags,
    createDecoderManifest_signalDecoders,
    createDecoderManifest_description,
    createDecoderManifest_networkInterfaces,
    createDecoderManifest_name,
    createDecoderManifest_modelManifestArn,

    -- * Destructuring the Response
    CreateDecoderManifestResponse (..),
    newCreateDecoderManifestResponse,

    -- * Response Lenses
    createDecoderManifestResponse_httpStatus,
    createDecoderManifestResponse_name,
    createDecoderManifestResponse_arn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTFleetWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateDecoderManifest' smart constructor.
data CreateDecoderManifest = CreateDecoderManifest'
  { -- | Metadata that can be used to manage the decoder manifest.
    tags :: Prelude.Maybe [Tag],
    -- | A list of information about signal decoders.
    signalDecoders :: Prelude.Maybe (Prelude.NonEmpty SignalDecoder),
    -- | A brief description of the decoder manifest.
    description :: Prelude.Maybe Prelude.Text,
    -- | A list of information about available network interfaces.
    networkInterfaces :: Prelude.Maybe (Prelude.NonEmpty NetworkInterface),
    -- | The unique name of the decoder manifest to create.
    name :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the vehicle model (model manifest).
    modelManifestArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDecoderManifest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createDecoderManifest_tags' - Metadata that can be used to manage the decoder manifest.
--
-- 'signalDecoders', 'createDecoderManifest_signalDecoders' - A list of information about signal decoders.
--
-- 'description', 'createDecoderManifest_description' - A brief description of the decoder manifest.
--
-- 'networkInterfaces', 'createDecoderManifest_networkInterfaces' - A list of information about available network interfaces.
--
-- 'name', 'createDecoderManifest_name' - The unique name of the decoder manifest to create.
--
-- 'modelManifestArn', 'createDecoderManifest_modelManifestArn' - The Amazon Resource Name (ARN) of the vehicle model (model manifest).
newCreateDecoderManifest ::
  -- | 'name'
  Prelude.Text ->
  -- | 'modelManifestArn'
  Prelude.Text ->
  CreateDecoderManifest
newCreateDecoderManifest pName_ pModelManifestArn_ =
  CreateDecoderManifest'
    { tags = Prelude.Nothing,
      signalDecoders = Prelude.Nothing,
      description = Prelude.Nothing,
      networkInterfaces = Prelude.Nothing,
      name = pName_,
      modelManifestArn = pModelManifestArn_
    }

-- | Metadata that can be used to manage the decoder manifest.
createDecoderManifest_tags :: Lens.Lens' CreateDecoderManifest (Prelude.Maybe [Tag])
createDecoderManifest_tags = Lens.lens (\CreateDecoderManifest' {tags} -> tags) (\s@CreateDecoderManifest' {} a -> s {tags = a} :: CreateDecoderManifest) Prelude.. Lens.mapping Lens.coerced

-- | A list of information about signal decoders.
createDecoderManifest_signalDecoders :: Lens.Lens' CreateDecoderManifest (Prelude.Maybe (Prelude.NonEmpty SignalDecoder))
createDecoderManifest_signalDecoders = Lens.lens (\CreateDecoderManifest' {signalDecoders} -> signalDecoders) (\s@CreateDecoderManifest' {} a -> s {signalDecoders = a} :: CreateDecoderManifest) Prelude.. Lens.mapping Lens.coerced

-- | A brief description of the decoder manifest.
createDecoderManifest_description :: Lens.Lens' CreateDecoderManifest (Prelude.Maybe Prelude.Text)
createDecoderManifest_description = Lens.lens (\CreateDecoderManifest' {description} -> description) (\s@CreateDecoderManifest' {} a -> s {description = a} :: CreateDecoderManifest)

-- | A list of information about available network interfaces.
createDecoderManifest_networkInterfaces :: Lens.Lens' CreateDecoderManifest (Prelude.Maybe (Prelude.NonEmpty NetworkInterface))
createDecoderManifest_networkInterfaces = Lens.lens (\CreateDecoderManifest' {networkInterfaces} -> networkInterfaces) (\s@CreateDecoderManifest' {} a -> s {networkInterfaces = a} :: CreateDecoderManifest) Prelude.. Lens.mapping Lens.coerced

-- | The unique name of the decoder manifest to create.
createDecoderManifest_name :: Lens.Lens' CreateDecoderManifest Prelude.Text
createDecoderManifest_name = Lens.lens (\CreateDecoderManifest' {name} -> name) (\s@CreateDecoderManifest' {} a -> s {name = a} :: CreateDecoderManifest)

-- | The Amazon Resource Name (ARN) of the vehicle model (model manifest).
createDecoderManifest_modelManifestArn :: Lens.Lens' CreateDecoderManifest Prelude.Text
createDecoderManifest_modelManifestArn = Lens.lens (\CreateDecoderManifest' {modelManifestArn} -> modelManifestArn) (\s@CreateDecoderManifest' {} a -> s {modelManifestArn = a} :: CreateDecoderManifest)

instance Core.AWSRequest CreateDecoderManifest where
  type
    AWSResponse CreateDecoderManifest =
      CreateDecoderManifestResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDecoderManifestResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "name")
            Prelude.<*> (x Core..:> "arn")
      )

instance Prelude.Hashable CreateDecoderManifest where
  hashWithSalt _salt CreateDecoderManifest' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` signalDecoders
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` networkInterfaces
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` modelManifestArn

instance Prelude.NFData CreateDecoderManifest where
  rnf CreateDecoderManifest' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf signalDecoders
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf networkInterfaces
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf modelManifestArn

instance Core.ToHeaders CreateDecoderManifest where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "IoTAutobahnControlPlane.CreateDecoderManifest" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateDecoderManifest where
  toJSON CreateDecoderManifest' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("tags" Core..=) Prelude.<$> tags,
            ("signalDecoders" Core..=)
              Prelude.<$> signalDecoders,
            ("description" Core..=) Prelude.<$> description,
            ("networkInterfaces" Core..=)
              Prelude.<$> networkInterfaces,
            Prelude.Just ("name" Core..= name),
            Prelude.Just
              ("modelManifestArn" Core..= modelManifestArn)
          ]
      )

instance Core.ToPath CreateDecoderManifest where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateDecoderManifest where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDecoderManifestResponse' smart constructor.
data CreateDecoderManifestResponse = CreateDecoderManifestResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name of the created decoder manifest.
    name :: Prelude.Text,
    -- | The ARN of the created decoder manifest.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDecoderManifestResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createDecoderManifestResponse_httpStatus' - The response's http status code.
--
-- 'name', 'createDecoderManifestResponse_name' - The name of the created decoder manifest.
--
-- 'arn', 'createDecoderManifestResponse_arn' - The ARN of the created decoder manifest.
newCreateDecoderManifestResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'name'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  CreateDecoderManifestResponse
newCreateDecoderManifestResponse
  pHttpStatus_
  pName_
  pArn_ =
    CreateDecoderManifestResponse'
      { httpStatus =
          pHttpStatus_,
        name = pName_,
        arn = pArn_
      }

-- | The response's http status code.
createDecoderManifestResponse_httpStatus :: Lens.Lens' CreateDecoderManifestResponse Prelude.Int
createDecoderManifestResponse_httpStatus = Lens.lens (\CreateDecoderManifestResponse' {httpStatus} -> httpStatus) (\s@CreateDecoderManifestResponse' {} a -> s {httpStatus = a} :: CreateDecoderManifestResponse)

-- | The name of the created decoder manifest.
createDecoderManifestResponse_name :: Lens.Lens' CreateDecoderManifestResponse Prelude.Text
createDecoderManifestResponse_name = Lens.lens (\CreateDecoderManifestResponse' {name} -> name) (\s@CreateDecoderManifestResponse' {} a -> s {name = a} :: CreateDecoderManifestResponse)

-- | The ARN of the created decoder manifest.
createDecoderManifestResponse_arn :: Lens.Lens' CreateDecoderManifestResponse Prelude.Text
createDecoderManifestResponse_arn = Lens.lens (\CreateDecoderManifestResponse' {arn} -> arn) (\s@CreateDecoderManifestResponse' {} a -> s {arn = a} :: CreateDecoderManifestResponse)

instance Prelude.NFData CreateDecoderManifestResponse where
  rnf CreateDecoderManifestResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf arn
