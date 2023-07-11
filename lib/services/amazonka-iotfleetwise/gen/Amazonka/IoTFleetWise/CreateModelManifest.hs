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
-- Module      : Amazonka.IoTFleetWise.CreateModelManifest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a vehicle model (model manifest) that specifies signals
-- (attributes, branches, sensors, and actuators).
--
-- For more information, see
-- <https://docs.aws.amazon.com/iot-fleetwise/latest/developerguide/vehicle-models.html Vehicle models>
-- in the /Amazon Web Services IoT FleetWise Developer Guide/.
module Amazonka.IoTFleetWise.CreateModelManifest
  ( -- * Creating a Request
    CreateModelManifest (..),
    newCreateModelManifest,

    -- * Request Lenses
    createModelManifest_description,
    createModelManifest_tags,
    createModelManifest_name,
    createModelManifest_nodes,
    createModelManifest_signalCatalogArn,

    -- * Destructuring the Response
    CreateModelManifestResponse (..),
    newCreateModelManifestResponse,

    -- * Response Lenses
    createModelManifestResponse_httpStatus,
    createModelManifestResponse_name,
    createModelManifestResponse_arn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTFleetWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateModelManifest' smart constructor.
data CreateModelManifest = CreateModelManifest'
  { -- | A brief description of the vehicle model.
    description :: Prelude.Maybe Prelude.Text,
    -- | Metadata that can be used to manage the vehicle model.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the vehicle model to create.
    name :: Prelude.Text,
    -- | A list of nodes, which are a general abstraction of signals.
    nodes :: [Prelude.Text],
    -- | The Amazon Resource Name (ARN) of a signal catalog.
    signalCatalogArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateModelManifest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createModelManifest_description' - A brief description of the vehicle model.
--
-- 'tags', 'createModelManifest_tags' - Metadata that can be used to manage the vehicle model.
--
-- 'name', 'createModelManifest_name' - The name of the vehicle model to create.
--
-- 'nodes', 'createModelManifest_nodes' - A list of nodes, which are a general abstraction of signals.
--
-- 'signalCatalogArn', 'createModelManifest_signalCatalogArn' - The Amazon Resource Name (ARN) of a signal catalog.
newCreateModelManifest ::
  -- | 'name'
  Prelude.Text ->
  -- | 'signalCatalogArn'
  Prelude.Text ->
  CreateModelManifest
newCreateModelManifest pName_ pSignalCatalogArn_ =
  CreateModelManifest'
    { description = Prelude.Nothing,
      tags = Prelude.Nothing,
      name = pName_,
      nodes = Prelude.mempty,
      signalCatalogArn = pSignalCatalogArn_
    }

-- | A brief description of the vehicle model.
createModelManifest_description :: Lens.Lens' CreateModelManifest (Prelude.Maybe Prelude.Text)
createModelManifest_description = Lens.lens (\CreateModelManifest' {description} -> description) (\s@CreateModelManifest' {} a -> s {description = a} :: CreateModelManifest)

-- | Metadata that can be used to manage the vehicle model.
createModelManifest_tags :: Lens.Lens' CreateModelManifest (Prelude.Maybe [Tag])
createModelManifest_tags = Lens.lens (\CreateModelManifest' {tags} -> tags) (\s@CreateModelManifest' {} a -> s {tags = a} :: CreateModelManifest) Prelude.. Lens.mapping Lens.coerced

-- | The name of the vehicle model to create.
createModelManifest_name :: Lens.Lens' CreateModelManifest Prelude.Text
createModelManifest_name = Lens.lens (\CreateModelManifest' {name} -> name) (\s@CreateModelManifest' {} a -> s {name = a} :: CreateModelManifest)

-- | A list of nodes, which are a general abstraction of signals.
createModelManifest_nodes :: Lens.Lens' CreateModelManifest [Prelude.Text]
createModelManifest_nodes = Lens.lens (\CreateModelManifest' {nodes} -> nodes) (\s@CreateModelManifest' {} a -> s {nodes = a} :: CreateModelManifest) Prelude.. Lens.coerced

-- | The Amazon Resource Name (ARN) of a signal catalog.
createModelManifest_signalCatalogArn :: Lens.Lens' CreateModelManifest Prelude.Text
createModelManifest_signalCatalogArn = Lens.lens (\CreateModelManifest' {signalCatalogArn} -> signalCatalogArn) (\s@CreateModelManifest' {} a -> s {signalCatalogArn = a} :: CreateModelManifest)

instance Core.AWSRequest CreateModelManifest where
  type
    AWSResponse CreateModelManifest =
      CreateModelManifestResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateModelManifestResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "name")
            Prelude.<*> (x Data..:> "arn")
      )

instance Prelude.Hashable CreateModelManifest where
  hashWithSalt _salt CreateModelManifest' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` nodes
      `Prelude.hashWithSalt` signalCatalogArn

instance Prelude.NFData CreateModelManifest where
  rnf CreateModelManifest' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf nodes
      `Prelude.seq` Prelude.rnf signalCatalogArn

instance Data.ToHeaders CreateModelManifest where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "IoTAutobahnControlPlane.CreateModelManifest" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateModelManifest where
  toJSON CreateModelManifest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("name" Data..= name),
            Prelude.Just ("nodes" Data..= nodes),
            Prelude.Just
              ("signalCatalogArn" Data..= signalCatalogArn)
          ]
      )

instance Data.ToPath CreateModelManifest where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateModelManifest where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateModelManifestResponse' smart constructor.
data CreateModelManifestResponse = CreateModelManifestResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name of the created vehicle model.
    name :: Prelude.Text,
    -- | The ARN of the created vehicle model.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateModelManifestResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createModelManifestResponse_httpStatus' - The response's http status code.
--
-- 'name', 'createModelManifestResponse_name' - The name of the created vehicle model.
--
-- 'arn', 'createModelManifestResponse_arn' - The ARN of the created vehicle model.
newCreateModelManifestResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'name'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  CreateModelManifestResponse
newCreateModelManifestResponse
  pHttpStatus_
  pName_
  pArn_ =
    CreateModelManifestResponse'
      { httpStatus =
          pHttpStatus_,
        name = pName_,
        arn = pArn_
      }

-- | The response's http status code.
createModelManifestResponse_httpStatus :: Lens.Lens' CreateModelManifestResponse Prelude.Int
createModelManifestResponse_httpStatus = Lens.lens (\CreateModelManifestResponse' {httpStatus} -> httpStatus) (\s@CreateModelManifestResponse' {} a -> s {httpStatus = a} :: CreateModelManifestResponse)

-- | The name of the created vehicle model.
createModelManifestResponse_name :: Lens.Lens' CreateModelManifestResponse Prelude.Text
createModelManifestResponse_name = Lens.lens (\CreateModelManifestResponse' {name} -> name) (\s@CreateModelManifestResponse' {} a -> s {name = a} :: CreateModelManifestResponse)

-- | The ARN of the created vehicle model.
createModelManifestResponse_arn :: Lens.Lens' CreateModelManifestResponse Prelude.Text
createModelManifestResponse_arn = Lens.lens (\CreateModelManifestResponse' {arn} -> arn) (\s@CreateModelManifestResponse' {} a -> s {arn = a} :: CreateModelManifestResponse)

instance Prelude.NFData CreateModelManifestResponse where
  rnf CreateModelManifestResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf arn
