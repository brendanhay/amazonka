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
-- Module      : Amazonka.CloudDirectory.GetObjectInformation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves metadata about an object.
module Amazonka.CloudDirectory.GetObjectInformation
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
    getObjectInformationResponse_objectIdentifier,
    getObjectInformationResponse_schemaFacets,
    getObjectInformationResponse_httpStatus,
  )
where

import Amazonka.CloudDirectory.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetObjectInformation' smart constructor.
data GetObjectInformation = GetObjectInformation'
  { -- | The consistency level at which to retrieve the object information.
    consistencyLevel :: Prelude.Maybe ConsistencyLevel,
    -- | The ARN of the directory being retrieved.
    directoryArn :: Prelude.Text,
    -- | A reference to the object.
    objectReference :: ObjectReference
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'objectReference'
  ObjectReference ->
  GetObjectInformation
newGetObjectInformation
  pDirectoryArn_
  pObjectReference_ =
    GetObjectInformation'
      { consistencyLevel =
          Prelude.Nothing,
        directoryArn = pDirectoryArn_,
        objectReference = pObjectReference_
      }

-- | The consistency level at which to retrieve the object information.
getObjectInformation_consistencyLevel :: Lens.Lens' GetObjectInformation (Prelude.Maybe ConsistencyLevel)
getObjectInformation_consistencyLevel = Lens.lens (\GetObjectInformation' {consistencyLevel} -> consistencyLevel) (\s@GetObjectInformation' {} a -> s {consistencyLevel = a} :: GetObjectInformation)

-- | The ARN of the directory being retrieved.
getObjectInformation_directoryArn :: Lens.Lens' GetObjectInformation Prelude.Text
getObjectInformation_directoryArn = Lens.lens (\GetObjectInformation' {directoryArn} -> directoryArn) (\s@GetObjectInformation' {} a -> s {directoryArn = a} :: GetObjectInformation)

-- | A reference to the object.
getObjectInformation_objectReference :: Lens.Lens' GetObjectInformation ObjectReference
getObjectInformation_objectReference = Lens.lens (\GetObjectInformation' {objectReference} -> objectReference) (\s@GetObjectInformation' {} a -> s {objectReference = a} :: GetObjectInformation)

instance Core.AWSRequest GetObjectInformation where
  type
    AWSResponse GetObjectInformation =
      GetObjectInformationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetObjectInformationResponse'
            Prelude.<$> (x Data..?> "ObjectIdentifier")
            Prelude.<*> (x Data..?> "SchemaFacets" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetObjectInformation where
  hashWithSalt _salt GetObjectInformation' {..} =
    _salt
      `Prelude.hashWithSalt` consistencyLevel
      `Prelude.hashWithSalt` directoryArn
      `Prelude.hashWithSalt` objectReference

instance Prelude.NFData GetObjectInformation where
  rnf GetObjectInformation' {..} =
    Prelude.rnf consistencyLevel `Prelude.seq`
      Prelude.rnf directoryArn `Prelude.seq`
        Prelude.rnf objectReference

instance Data.ToHeaders GetObjectInformation where
  toHeaders GetObjectInformation' {..} =
    Prelude.mconcat
      [ "x-amz-consistency-level" Data.=# consistencyLevel,
        "x-amz-data-partition" Data.=# directoryArn
      ]

instance Data.ToJSON GetObjectInformation where
  toJSON GetObjectInformation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ObjectReference" Data..= objectReference)
          ]
      )

instance Data.ToPath GetObjectInformation where
  toPath =
    Prelude.const
      "/amazonclouddirectory/2017-01-11/object/information"

instance Data.ToQuery GetObjectInformation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetObjectInformationResponse' smart constructor.
data GetObjectInformationResponse = GetObjectInformationResponse'
  { -- | The @ObjectIdentifier@ of the specified object.
    objectIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The facets attached to the specified object. Although the response does
    -- not include minor version information, the most recently applied minor
    -- version of each Facet is in effect. See GetAppliedSchemaVersion for
    -- details.
    schemaFacets :: Prelude.Maybe [SchemaFacet],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetObjectInformationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'objectIdentifier', 'getObjectInformationResponse_objectIdentifier' - The @ObjectIdentifier@ of the specified object.
--
-- 'schemaFacets', 'getObjectInformationResponse_schemaFacets' - The facets attached to the specified object. Although the response does
-- not include minor version information, the most recently applied minor
-- version of each Facet is in effect. See GetAppliedSchemaVersion for
-- details.
--
-- 'httpStatus', 'getObjectInformationResponse_httpStatus' - The response's http status code.
newGetObjectInformationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetObjectInformationResponse
newGetObjectInformationResponse pHttpStatus_ =
  GetObjectInformationResponse'
    { objectIdentifier =
        Prelude.Nothing,
      schemaFacets = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @ObjectIdentifier@ of the specified object.
getObjectInformationResponse_objectIdentifier :: Lens.Lens' GetObjectInformationResponse (Prelude.Maybe Prelude.Text)
getObjectInformationResponse_objectIdentifier = Lens.lens (\GetObjectInformationResponse' {objectIdentifier} -> objectIdentifier) (\s@GetObjectInformationResponse' {} a -> s {objectIdentifier = a} :: GetObjectInformationResponse)

-- | The facets attached to the specified object. Although the response does
-- not include minor version information, the most recently applied minor
-- version of each Facet is in effect. See GetAppliedSchemaVersion for
-- details.
getObjectInformationResponse_schemaFacets :: Lens.Lens' GetObjectInformationResponse (Prelude.Maybe [SchemaFacet])
getObjectInformationResponse_schemaFacets = Lens.lens (\GetObjectInformationResponse' {schemaFacets} -> schemaFacets) (\s@GetObjectInformationResponse' {} a -> s {schemaFacets = a} :: GetObjectInformationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getObjectInformationResponse_httpStatus :: Lens.Lens' GetObjectInformationResponse Prelude.Int
getObjectInformationResponse_httpStatus = Lens.lens (\GetObjectInformationResponse' {httpStatus} -> httpStatus) (\s@GetObjectInformationResponse' {} a -> s {httpStatus = a} :: GetObjectInformationResponse)

instance Prelude.NFData GetObjectInformationResponse where
  rnf GetObjectInformationResponse' {..} =
    Prelude.rnf objectIdentifier `Prelude.seq`
      Prelude.rnf schemaFacets `Prelude.seq`
        Prelude.rnf httpStatus
