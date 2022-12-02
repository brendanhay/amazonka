{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.APIGateway.Types.DocumentationPartLocation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.APIGateway.Types.DocumentationPartLocation where

import Amazonka.APIGateway.Types.DocumentationPartType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the target API entity to which the documentation applies.
--
-- /See:/ 'newDocumentationPartLocation' smart constructor.
data DocumentationPartLocation = DocumentationPartLocation'
  { -- | The name of the targeted API entity. It is a valid and required field
    -- for the API entity types of @AUTHORIZER@, @MODEL@, @PATH_PARAMETER@,
    -- @QUERY_PARAMETER@, @REQUEST_HEADER@, @REQUEST_BODY@ and
    -- @RESPONSE_HEADER@. It is an invalid field for any other entity type.
    name :: Prelude.Maybe Prelude.Text,
    -- | The HTTP verb of a method. It is a valid field for the API entity types
    -- of @METHOD@, @PATH_PARAMETER@, @QUERY_PARAMETER@, @REQUEST_HEADER@,
    -- @REQUEST_BODY@, @RESPONSE@, @RESPONSE_HEADER@, and @RESPONSE_BODY@. The
    -- default value is @*@ for any method. When an applicable child entity
    -- inherits the content of an entity of the same type with more general
    -- specifications of the other @location@ attributes, the child entity\'s
    -- @method@ attribute must match that of the parent entity exactly.
    method :: Prelude.Maybe Prelude.Text,
    -- | The URL path of the target. It is a valid field for the API entity types
    -- of @RESOURCE@, @METHOD@, @PATH_PARAMETER@, @QUERY_PARAMETER@,
    -- @REQUEST_HEADER@, @REQUEST_BODY@, @RESPONSE@, @RESPONSE_HEADER@, and
    -- @RESPONSE_BODY@. The default value is @\/@ for the root resource. When
    -- an applicable child entity inherits the content of another entity of the
    -- same type with more general specifications of the other @location@
    -- attributes, the child entity\'s @path@ attribute must match that of the
    -- parent entity as a prefix.
    path :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status code of a response. It is a valid field for the API
    -- entity types of @RESPONSE@, @RESPONSE_HEADER@, and @RESPONSE_BODY@. The
    -- default value is @*@ for any status code. When an applicable child
    -- entity inherits the content of an entity of the same type with more
    -- general specifications of the other @location@ attributes, the child
    -- entity\'s @statusCode@ attribute must match that of the parent entity
    -- exactly.
    statusCode :: Prelude.Maybe Prelude.Text,
    -- | The type of API entity to which the documentation content applies. Valid
    -- values are @API@, @AUTHORIZER@, @MODEL@, @RESOURCE@, @METHOD@,
    -- @PATH_PARAMETER@, @QUERY_PARAMETER@, @REQUEST_HEADER@, @REQUEST_BODY@,
    -- @RESPONSE@, @RESPONSE_HEADER@, and @RESPONSE_BODY@. Content inheritance
    -- does not apply to any entity of the @API@, @AUTHORIZER@, @METHOD@,
    -- @MODEL@, @REQUEST_BODY@, or @RESOURCE@ type.
    type' :: DocumentationPartType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DocumentationPartLocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'documentationPartLocation_name' - The name of the targeted API entity. It is a valid and required field
-- for the API entity types of @AUTHORIZER@, @MODEL@, @PATH_PARAMETER@,
-- @QUERY_PARAMETER@, @REQUEST_HEADER@, @REQUEST_BODY@ and
-- @RESPONSE_HEADER@. It is an invalid field for any other entity type.
--
-- 'method', 'documentationPartLocation_method' - The HTTP verb of a method. It is a valid field for the API entity types
-- of @METHOD@, @PATH_PARAMETER@, @QUERY_PARAMETER@, @REQUEST_HEADER@,
-- @REQUEST_BODY@, @RESPONSE@, @RESPONSE_HEADER@, and @RESPONSE_BODY@. The
-- default value is @*@ for any method. When an applicable child entity
-- inherits the content of an entity of the same type with more general
-- specifications of the other @location@ attributes, the child entity\'s
-- @method@ attribute must match that of the parent entity exactly.
--
-- 'path', 'documentationPartLocation_path' - The URL path of the target. It is a valid field for the API entity types
-- of @RESOURCE@, @METHOD@, @PATH_PARAMETER@, @QUERY_PARAMETER@,
-- @REQUEST_HEADER@, @REQUEST_BODY@, @RESPONSE@, @RESPONSE_HEADER@, and
-- @RESPONSE_BODY@. The default value is @\/@ for the root resource. When
-- an applicable child entity inherits the content of another entity of the
-- same type with more general specifications of the other @location@
-- attributes, the child entity\'s @path@ attribute must match that of the
-- parent entity as a prefix.
--
-- 'statusCode', 'documentationPartLocation_statusCode' - The HTTP status code of a response. It is a valid field for the API
-- entity types of @RESPONSE@, @RESPONSE_HEADER@, and @RESPONSE_BODY@. The
-- default value is @*@ for any status code. When an applicable child
-- entity inherits the content of an entity of the same type with more
-- general specifications of the other @location@ attributes, the child
-- entity\'s @statusCode@ attribute must match that of the parent entity
-- exactly.
--
-- 'type'', 'documentationPartLocation_type' - The type of API entity to which the documentation content applies. Valid
-- values are @API@, @AUTHORIZER@, @MODEL@, @RESOURCE@, @METHOD@,
-- @PATH_PARAMETER@, @QUERY_PARAMETER@, @REQUEST_HEADER@, @REQUEST_BODY@,
-- @RESPONSE@, @RESPONSE_HEADER@, and @RESPONSE_BODY@. Content inheritance
-- does not apply to any entity of the @API@, @AUTHORIZER@, @METHOD@,
-- @MODEL@, @REQUEST_BODY@, or @RESOURCE@ type.
newDocumentationPartLocation ::
  -- | 'type''
  DocumentationPartType ->
  DocumentationPartLocation
newDocumentationPartLocation pType_ =
  DocumentationPartLocation'
    { name = Prelude.Nothing,
      method = Prelude.Nothing,
      path = Prelude.Nothing,
      statusCode = Prelude.Nothing,
      type' = pType_
    }

-- | The name of the targeted API entity. It is a valid and required field
-- for the API entity types of @AUTHORIZER@, @MODEL@, @PATH_PARAMETER@,
-- @QUERY_PARAMETER@, @REQUEST_HEADER@, @REQUEST_BODY@ and
-- @RESPONSE_HEADER@. It is an invalid field for any other entity type.
documentationPartLocation_name :: Lens.Lens' DocumentationPartLocation (Prelude.Maybe Prelude.Text)
documentationPartLocation_name = Lens.lens (\DocumentationPartLocation' {name} -> name) (\s@DocumentationPartLocation' {} a -> s {name = a} :: DocumentationPartLocation)

-- | The HTTP verb of a method. It is a valid field for the API entity types
-- of @METHOD@, @PATH_PARAMETER@, @QUERY_PARAMETER@, @REQUEST_HEADER@,
-- @REQUEST_BODY@, @RESPONSE@, @RESPONSE_HEADER@, and @RESPONSE_BODY@. The
-- default value is @*@ for any method. When an applicable child entity
-- inherits the content of an entity of the same type with more general
-- specifications of the other @location@ attributes, the child entity\'s
-- @method@ attribute must match that of the parent entity exactly.
documentationPartLocation_method :: Lens.Lens' DocumentationPartLocation (Prelude.Maybe Prelude.Text)
documentationPartLocation_method = Lens.lens (\DocumentationPartLocation' {method} -> method) (\s@DocumentationPartLocation' {} a -> s {method = a} :: DocumentationPartLocation)

-- | The URL path of the target. It is a valid field for the API entity types
-- of @RESOURCE@, @METHOD@, @PATH_PARAMETER@, @QUERY_PARAMETER@,
-- @REQUEST_HEADER@, @REQUEST_BODY@, @RESPONSE@, @RESPONSE_HEADER@, and
-- @RESPONSE_BODY@. The default value is @\/@ for the root resource. When
-- an applicable child entity inherits the content of another entity of the
-- same type with more general specifications of the other @location@
-- attributes, the child entity\'s @path@ attribute must match that of the
-- parent entity as a prefix.
documentationPartLocation_path :: Lens.Lens' DocumentationPartLocation (Prelude.Maybe Prelude.Text)
documentationPartLocation_path = Lens.lens (\DocumentationPartLocation' {path} -> path) (\s@DocumentationPartLocation' {} a -> s {path = a} :: DocumentationPartLocation)

-- | The HTTP status code of a response. It is a valid field for the API
-- entity types of @RESPONSE@, @RESPONSE_HEADER@, and @RESPONSE_BODY@. The
-- default value is @*@ for any status code. When an applicable child
-- entity inherits the content of an entity of the same type with more
-- general specifications of the other @location@ attributes, the child
-- entity\'s @statusCode@ attribute must match that of the parent entity
-- exactly.
documentationPartLocation_statusCode :: Lens.Lens' DocumentationPartLocation (Prelude.Maybe Prelude.Text)
documentationPartLocation_statusCode = Lens.lens (\DocumentationPartLocation' {statusCode} -> statusCode) (\s@DocumentationPartLocation' {} a -> s {statusCode = a} :: DocumentationPartLocation)

-- | The type of API entity to which the documentation content applies. Valid
-- values are @API@, @AUTHORIZER@, @MODEL@, @RESOURCE@, @METHOD@,
-- @PATH_PARAMETER@, @QUERY_PARAMETER@, @REQUEST_HEADER@, @REQUEST_BODY@,
-- @RESPONSE@, @RESPONSE_HEADER@, and @RESPONSE_BODY@. Content inheritance
-- does not apply to any entity of the @API@, @AUTHORIZER@, @METHOD@,
-- @MODEL@, @REQUEST_BODY@, or @RESOURCE@ type.
documentationPartLocation_type :: Lens.Lens' DocumentationPartLocation DocumentationPartType
documentationPartLocation_type = Lens.lens (\DocumentationPartLocation' {type'} -> type') (\s@DocumentationPartLocation' {} a -> s {type' = a} :: DocumentationPartLocation)

instance Data.FromJSON DocumentationPartLocation where
  parseJSON =
    Data.withObject
      "DocumentationPartLocation"
      ( \x ->
          DocumentationPartLocation'
            Prelude.<$> (x Data..:? "name")
            Prelude.<*> (x Data..:? "method")
            Prelude.<*> (x Data..:? "path")
            Prelude.<*> (x Data..:? "statusCode")
            Prelude.<*> (x Data..: "type")
      )

instance Prelude.Hashable DocumentationPartLocation where
  hashWithSalt _salt DocumentationPartLocation' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` method
      `Prelude.hashWithSalt` path
      `Prelude.hashWithSalt` statusCode
      `Prelude.hashWithSalt` type'

instance Prelude.NFData DocumentationPartLocation where
  rnf DocumentationPartLocation' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf method
      `Prelude.seq` Prelude.rnf path
      `Prelude.seq` Prelude.rnf statusCode
      `Prelude.seq` Prelude.rnf type'

instance Data.ToJSON DocumentationPartLocation where
  toJSON DocumentationPartLocation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("name" Data..=) Prelude.<$> name,
            ("method" Data..=) Prelude.<$> method,
            ("path" Data..=) Prelude.<$> path,
            ("statusCode" Data..=) Prelude.<$> statusCode,
            Prelude.Just ("type" Data..= type')
          ]
      )
