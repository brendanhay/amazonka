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
-- Module      : Network.AWS.CloudDirectory.RemoveFacetFromObject
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified facet from the specified object.
module Network.AWS.CloudDirectory.RemoveFacetFromObject
  ( -- * Creating a Request
    RemoveFacetFromObject (..),
    newRemoveFacetFromObject,

    -- * Request Lenses
    removeFacetFromObject_directoryArn,
    removeFacetFromObject_schemaFacet,
    removeFacetFromObject_objectReference,

    -- * Destructuring the Response
    RemoveFacetFromObjectResponse (..),
    newRemoveFacetFromObjectResponse,

    -- * Response Lenses
    removeFacetFromObjectResponse_httpStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRemoveFacetFromObject' smart constructor.
data RemoveFacetFromObject = RemoveFacetFromObject'
  { -- | The ARN of the directory in which the object resides.
    directoryArn :: Prelude.Text,
    -- | The facet to remove. See SchemaFacet for details.
    schemaFacet :: SchemaFacet,
    -- | A reference to the object to remove the facet from.
    objectReference :: ObjectReference
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RemoveFacetFromObject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryArn', 'removeFacetFromObject_directoryArn' - The ARN of the directory in which the object resides.
--
-- 'schemaFacet', 'removeFacetFromObject_schemaFacet' - The facet to remove. See SchemaFacet for details.
--
-- 'objectReference', 'removeFacetFromObject_objectReference' - A reference to the object to remove the facet from.
newRemoveFacetFromObject ::
  -- | 'directoryArn'
  Prelude.Text ->
  -- | 'schemaFacet'
  SchemaFacet ->
  -- | 'objectReference'
  ObjectReference ->
  RemoveFacetFromObject
newRemoveFacetFromObject
  pDirectoryArn_
  pSchemaFacet_
  pObjectReference_ =
    RemoveFacetFromObject'
      { directoryArn =
          pDirectoryArn_,
        schemaFacet = pSchemaFacet_,
        objectReference = pObjectReference_
      }

-- | The ARN of the directory in which the object resides.
removeFacetFromObject_directoryArn :: Lens.Lens' RemoveFacetFromObject Prelude.Text
removeFacetFromObject_directoryArn = Lens.lens (\RemoveFacetFromObject' {directoryArn} -> directoryArn) (\s@RemoveFacetFromObject' {} a -> s {directoryArn = a} :: RemoveFacetFromObject)

-- | The facet to remove. See SchemaFacet for details.
removeFacetFromObject_schemaFacet :: Lens.Lens' RemoveFacetFromObject SchemaFacet
removeFacetFromObject_schemaFacet = Lens.lens (\RemoveFacetFromObject' {schemaFacet} -> schemaFacet) (\s@RemoveFacetFromObject' {} a -> s {schemaFacet = a} :: RemoveFacetFromObject)

-- | A reference to the object to remove the facet from.
removeFacetFromObject_objectReference :: Lens.Lens' RemoveFacetFromObject ObjectReference
removeFacetFromObject_objectReference = Lens.lens (\RemoveFacetFromObject' {objectReference} -> objectReference) (\s@RemoveFacetFromObject' {} a -> s {objectReference = a} :: RemoveFacetFromObject)

instance Prelude.AWSRequest RemoveFacetFromObject where
  type
    Rs RemoveFacetFromObject =
      RemoveFacetFromObjectResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          RemoveFacetFromObjectResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RemoveFacetFromObject

instance Prelude.NFData RemoveFacetFromObject

instance Prelude.ToHeaders RemoveFacetFromObject where
  toHeaders RemoveFacetFromObject' {..} =
    Prelude.mconcat
      ["x-amz-data-partition" Prelude.=# directoryArn]

instance Prelude.ToJSON RemoveFacetFromObject where
  toJSON RemoveFacetFromObject' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("SchemaFacet" Prelude..= schemaFacet),
            Prelude.Just
              ("ObjectReference" Prelude..= objectReference)
          ]
      )

instance Prelude.ToPath RemoveFacetFromObject where
  toPath =
    Prelude.const
      "/amazonclouddirectory/2017-01-11/object/facets/delete"

instance Prelude.ToQuery RemoveFacetFromObject where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRemoveFacetFromObjectResponse' smart constructor.
data RemoveFacetFromObjectResponse = RemoveFacetFromObjectResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RemoveFacetFromObjectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'removeFacetFromObjectResponse_httpStatus' - The response's http status code.
newRemoveFacetFromObjectResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RemoveFacetFromObjectResponse
newRemoveFacetFromObjectResponse pHttpStatus_ =
  RemoveFacetFromObjectResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
removeFacetFromObjectResponse_httpStatus :: Lens.Lens' RemoveFacetFromObjectResponse Prelude.Int
removeFacetFromObjectResponse_httpStatus = Lens.lens (\RemoveFacetFromObjectResponse' {httpStatus} -> httpStatus) (\s@RemoveFacetFromObjectResponse' {} a -> s {httpStatus = a} :: RemoveFacetFromObjectResponse)

instance Prelude.NFData RemoveFacetFromObjectResponse
