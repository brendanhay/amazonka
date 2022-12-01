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
-- Module      : Amazonka.APIGateway.CreateDocumentationPart
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a documentation part.
module Amazonka.APIGateway.CreateDocumentationPart
  ( -- * Creating a Request
    CreateDocumentationPart (..),
    newCreateDocumentationPart,

    -- * Request Lenses
    createDocumentationPart_restApiId,
    createDocumentationPart_location,
    createDocumentationPart_properties,

    -- * Destructuring the Response
    DocumentationPart (..),
    newDocumentationPart,

    -- * Response Lenses
    documentationPart_properties,
    documentationPart_id,
    documentationPart_location,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Creates a new documentation part of a given API.
--
-- /See:/ 'newCreateDocumentationPart' smart constructor.
data CreateDocumentationPart = CreateDocumentationPart'
  { -- | The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | The location of the targeted API entity of the to-be-created
    -- documentation part.
    location :: DocumentationPartLocation,
    -- | The new documentation content map of the targeted API entity. Enclosed
    -- key-value pairs are API-specific, but only OpenAPI-compliant key-value
    -- pairs can be exported and, hence, published.
    properties :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDocumentationPart' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'restApiId', 'createDocumentationPart_restApiId' - The string identifier of the associated RestApi.
--
-- 'location', 'createDocumentationPart_location' - The location of the targeted API entity of the to-be-created
-- documentation part.
--
-- 'properties', 'createDocumentationPart_properties' - The new documentation content map of the targeted API entity. Enclosed
-- key-value pairs are API-specific, but only OpenAPI-compliant key-value
-- pairs can be exported and, hence, published.
newCreateDocumentationPart ::
  -- | 'restApiId'
  Prelude.Text ->
  -- | 'location'
  DocumentationPartLocation ->
  -- | 'properties'
  Prelude.Text ->
  CreateDocumentationPart
newCreateDocumentationPart
  pRestApiId_
  pLocation_
  pProperties_ =
    CreateDocumentationPart'
      { restApiId = pRestApiId_,
        location = pLocation_,
        properties = pProperties_
      }

-- | The string identifier of the associated RestApi.
createDocumentationPart_restApiId :: Lens.Lens' CreateDocumentationPart Prelude.Text
createDocumentationPart_restApiId = Lens.lens (\CreateDocumentationPart' {restApiId} -> restApiId) (\s@CreateDocumentationPart' {} a -> s {restApiId = a} :: CreateDocumentationPart)

-- | The location of the targeted API entity of the to-be-created
-- documentation part.
createDocumentationPart_location :: Lens.Lens' CreateDocumentationPart DocumentationPartLocation
createDocumentationPart_location = Lens.lens (\CreateDocumentationPart' {location} -> location) (\s@CreateDocumentationPart' {} a -> s {location = a} :: CreateDocumentationPart)

-- | The new documentation content map of the targeted API entity. Enclosed
-- key-value pairs are API-specific, but only OpenAPI-compliant key-value
-- pairs can be exported and, hence, published.
createDocumentationPart_properties :: Lens.Lens' CreateDocumentationPart Prelude.Text
createDocumentationPart_properties = Lens.lens (\CreateDocumentationPart' {properties} -> properties) (\s@CreateDocumentationPart' {} a -> s {properties = a} :: CreateDocumentationPart)

instance Core.AWSRequest CreateDocumentationPart where
  type
    AWSResponse CreateDocumentationPart =
      DocumentationPart
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Prelude.Hashable CreateDocumentationPart where
  hashWithSalt _salt CreateDocumentationPart' {..} =
    _salt `Prelude.hashWithSalt` restApiId
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` properties

instance Prelude.NFData CreateDocumentationPart where
  rnf CreateDocumentationPart' {..} =
    Prelude.rnf restApiId
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf properties

instance Core.ToHeaders CreateDocumentationPart where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Core.ToJSON CreateDocumentationPart where
  toJSON CreateDocumentationPart' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("location" Core..= location),
            Prelude.Just ("properties" Core..= properties)
          ]
      )

instance Core.ToPath CreateDocumentationPart where
  toPath CreateDocumentationPart' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Core.toBS restApiId,
        "/documentation/parts"
      ]

instance Core.ToQuery CreateDocumentationPart where
  toQuery = Prelude.const Prelude.mempty
