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
-- Module      : Network.AWS.APIGateway.CreateDocumentationPart
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- -- | Undocumented operation.
module Network.AWS.APIGateway.CreateDocumentationPart
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
    documentationPart_id,
    documentationPart_properties,
    documentationPart_location,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Creates a new documentation part of a given API.
--
-- /See:/ 'newCreateDocumentationPart' smart constructor.
data CreateDocumentationPart = CreateDocumentationPart'
  { -- | [Required] The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | [Required] The location of the targeted API entity of the to-be-created
    -- documentation part.
    location :: DocumentationPartLocation,
    -- | [Required] The new documentation content map of the targeted API entity.
    -- Enclosed key-value pairs are API-specific, but only OpenAPI-compliant
    -- key-value pairs can be exported and, hence, published.
    properties :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateDocumentationPart' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'restApiId', 'createDocumentationPart_restApiId' - [Required] The string identifier of the associated RestApi.
--
-- 'location', 'createDocumentationPart_location' - [Required] The location of the targeted API entity of the to-be-created
-- documentation part.
--
-- 'properties', 'createDocumentationPart_properties' - [Required] The new documentation content map of the targeted API entity.
-- Enclosed key-value pairs are API-specific, but only OpenAPI-compliant
-- key-value pairs can be exported and, hence, published.
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

-- | [Required] The string identifier of the associated RestApi.
createDocumentationPart_restApiId :: Lens.Lens' CreateDocumentationPart Prelude.Text
createDocumentationPart_restApiId = Lens.lens (\CreateDocumentationPart' {restApiId} -> restApiId) (\s@CreateDocumentationPart' {} a -> s {restApiId = a} :: CreateDocumentationPart)

-- | [Required] The location of the targeted API entity of the to-be-created
-- documentation part.
createDocumentationPart_location :: Lens.Lens' CreateDocumentationPart DocumentationPartLocation
createDocumentationPart_location = Lens.lens (\CreateDocumentationPart' {location} -> location) (\s@CreateDocumentationPart' {} a -> s {location = a} :: CreateDocumentationPart)

-- | [Required] The new documentation content map of the targeted API entity.
-- Enclosed key-value pairs are API-specific, but only OpenAPI-compliant
-- key-value pairs can be exported and, hence, published.
createDocumentationPart_properties :: Lens.Lens' CreateDocumentationPart Prelude.Text
createDocumentationPart_properties = Lens.lens (\CreateDocumentationPart' {properties} -> properties) (\s@CreateDocumentationPart' {} a -> s {properties = a} :: CreateDocumentationPart)

instance Prelude.AWSRequest CreateDocumentationPart where
  type Rs CreateDocumentationPart = DocumentationPart
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Prelude.eitherParseJSON x)

instance Prelude.Hashable CreateDocumentationPart

instance Prelude.NFData CreateDocumentationPart

instance Prelude.ToHeaders CreateDocumentationPart where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Prelude.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Prelude.ToJSON CreateDocumentationPart where
  toJSON CreateDocumentationPart' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("location" Prelude..= location),
            Prelude.Just ("properties" Prelude..= properties)
          ]
      )

instance Prelude.ToPath CreateDocumentationPart where
  toPath CreateDocumentationPart' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Prelude.toBS restApiId,
        "/documentation/parts"
      ]

instance Prelude.ToQuery CreateDocumentationPart where
  toQuery = Prelude.const Prelude.mempty
