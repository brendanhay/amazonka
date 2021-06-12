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
-- Module      : Network.AWS.CloudDirectory.GetObjectInformation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves metadata about an object.
module Network.AWS.CloudDirectory.GetObjectInformation
  ( -- * Creating a Request
    GetObjectInformation (..),
    newGetObjectInformation,

    -- * Request Lenses
    getObjectInformation_consistencyLevel,
    getObjectInformation_directoryArn,
    getObjectInformation_objectReference,

    -- * Destructuring the Response
    GetObjectInformationResponse (..),
    newGetObjectInformationResponse,

    -- * Response Lenses
    getObjectInformationResponse_schemaFacets,
    getObjectInformationResponse_objectIdentifier,
    getObjectInformationResponse_httpStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetObjectInformation' smart constructor.
data GetObjectInformation = GetObjectInformation'
  { -- | The consistency level at which to retrieve the object information.
    consistencyLevel :: Core.Maybe ConsistencyLevel,
    -- | The ARN of the directory being retrieved.
    directoryArn :: Core.Text,
    -- | A reference to the object.
    objectReference :: ObjectReference
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetObjectInformation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'consistencyLevel', 'getObjectInformation_consistencyLevel' - The consistency level at which to retrieve the object information.
--
-- 'directoryArn', 'getObjectInformation_directoryArn' - The ARN of the directory being retrieved.
--
-- 'objectReference', 'getObjectInformation_objectReference' - A reference to the object.
newGetObjectInformation ::
  -- | 'directoryArn'
  Core.Text ->
  -- | 'objectReference'
  ObjectReference ->
  GetObjectInformation
newGetObjectInformation
  pDirectoryArn_
  pObjectReference_ =
    GetObjectInformation'
      { consistencyLevel =
          Core.Nothing,
        directoryArn = pDirectoryArn_,
        objectReference = pObjectReference_
      }

-- | The consistency level at which to retrieve the object information.
getObjectInformation_consistencyLevel :: Lens.Lens' GetObjectInformation (Core.Maybe ConsistencyLevel)
getObjectInformation_consistencyLevel = Lens.lens (\GetObjectInformation' {consistencyLevel} -> consistencyLevel) (\s@GetObjectInformation' {} a -> s {consistencyLevel = a} :: GetObjectInformation)

-- | The ARN of the directory being retrieved.
getObjectInformation_directoryArn :: Lens.Lens' GetObjectInformation Core.Text
getObjectInformation_directoryArn = Lens.lens (\GetObjectInformation' {directoryArn} -> directoryArn) (\s@GetObjectInformation' {} a -> s {directoryArn = a} :: GetObjectInformation)

-- | A reference to the object.
getObjectInformation_objectReference :: Lens.Lens' GetObjectInformation ObjectReference
getObjectInformation_objectReference = Lens.lens (\GetObjectInformation' {objectReference} -> objectReference) (\s@GetObjectInformation' {} a -> s {objectReference = a} :: GetObjectInformation)

instance Core.AWSRequest GetObjectInformation where
  type
    AWSResponse GetObjectInformation =
      GetObjectInformationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetObjectInformationResponse'
            Core.<$> (x Core..?> "SchemaFacets" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "ObjectIdentifier")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetObjectInformation

instance Core.NFData GetObjectInformation

instance Core.ToHeaders GetObjectInformation where
  toHeaders GetObjectInformation' {..} =
    Core.mconcat
      [ "x-amz-consistency-level" Core.=# consistencyLevel,
        "x-amz-data-partition" Core.=# directoryArn
      ]

instance Core.ToJSON GetObjectInformation where
  toJSON GetObjectInformation' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("ObjectReference" Core..= objectReference)
          ]
      )

instance Core.ToPath GetObjectInformation where
  toPath =
    Core.const
      "/amazonclouddirectory/2017-01-11/object/information"

instance Core.ToQuery GetObjectInformation where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetObjectInformationResponse' smart constructor.
data GetObjectInformationResponse = GetObjectInformationResponse'
  { -- | The facets attached to the specified object. Although the response does
    -- not include minor version information, the most recently applied minor
    -- version of each Facet is in effect. See GetAppliedSchemaVersion for
    -- details.
    schemaFacets :: Core.Maybe [SchemaFacet],
    -- | The @ObjectIdentifier@ of the specified object.
    objectIdentifier :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetObjectInformationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schemaFacets', 'getObjectInformationResponse_schemaFacets' - The facets attached to the specified object. Although the response does
-- not include minor version information, the most recently applied minor
-- version of each Facet is in effect. See GetAppliedSchemaVersion for
-- details.
--
-- 'objectIdentifier', 'getObjectInformationResponse_objectIdentifier' - The @ObjectIdentifier@ of the specified object.
--
-- 'httpStatus', 'getObjectInformationResponse_httpStatus' - The response's http status code.
newGetObjectInformationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetObjectInformationResponse
newGetObjectInformationResponse pHttpStatus_ =
  GetObjectInformationResponse'
    { schemaFacets =
        Core.Nothing,
      objectIdentifier = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The facets attached to the specified object. Although the response does
-- not include minor version information, the most recently applied minor
-- version of each Facet is in effect. See GetAppliedSchemaVersion for
-- details.
getObjectInformationResponse_schemaFacets :: Lens.Lens' GetObjectInformationResponse (Core.Maybe [SchemaFacet])
getObjectInformationResponse_schemaFacets = Lens.lens (\GetObjectInformationResponse' {schemaFacets} -> schemaFacets) (\s@GetObjectInformationResponse' {} a -> s {schemaFacets = a} :: GetObjectInformationResponse) Core.. Lens.mapping Lens._Coerce

-- | The @ObjectIdentifier@ of the specified object.
getObjectInformationResponse_objectIdentifier :: Lens.Lens' GetObjectInformationResponse (Core.Maybe Core.Text)
getObjectInformationResponse_objectIdentifier = Lens.lens (\GetObjectInformationResponse' {objectIdentifier} -> objectIdentifier) (\s@GetObjectInformationResponse' {} a -> s {objectIdentifier = a} :: GetObjectInformationResponse)

-- | The response's http status code.
getObjectInformationResponse_httpStatus :: Lens.Lens' GetObjectInformationResponse Core.Int
getObjectInformationResponse_httpStatus = Lens.lens (\GetObjectInformationResponse' {httpStatus} -> httpStatus) (\s@GetObjectInformationResponse' {} a -> s {httpStatus = a} :: GetObjectInformationResponse)

instance Core.NFData GetObjectInformationResponse
