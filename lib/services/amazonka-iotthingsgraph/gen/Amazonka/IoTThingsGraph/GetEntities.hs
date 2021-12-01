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
-- Module      : Amazonka.IoTThingsGraph.GetEntities
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets definitions of the specified entities. Uses the latest version of
-- the user\'s namespace by default. This API returns the following TDM
-- entities.
--
-- -   Properties
--
-- -   States
--
-- -   Events
--
-- -   Actions
--
-- -   Capabilities
--
-- -   Mappings
--
-- -   Devices
--
-- -   Device Models
--
-- -   Services
--
-- This action doesn\'t return definitions for systems, flows, and
-- deployments.
module Amazonka.IoTThingsGraph.GetEntities
  ( -- * Creating a Request
    GetEntities (..),
    newGetEntities,

    -- * Request Lenses
    getEntities_namespaceVersion,
    getEntities_ids,

    -- * Destructuring the Response
    GetEntitiesResponse (..),
    newGetEntitiesResponse,

    -- * Response Lenses
    getEntitiesResponse_descriptions,
    getEntitiesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.IoTThingsGraph.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetEntities' smart constructor.
data GetEntities = GetEntities'
  { -- | The version of the user\'s namespace. Defaults to the latest version of
    -- the user\'s namespace.
    namespaceVersion :: Prelude.Maybe Prelude.Integer,
    -- | An array of entity IDs.
    --
    -- The IDs should be in the following format.
    --
    -- @urn:tdm:REGION\/ACCOUNT ID\/default:device:DEVICENAME@
    ids :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEntities' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'namespaceVersion', 'getEntities_namespaceVersion' - The version of the user\'s namespace. Defaults to the latest version of
-- the user\'s namespace.
--
-- 'ids', 'getEntities_ids' - An array of entity IDs.
--
-- The IDs should be in the following format.
--
-- @urn:tdm:REGION\/ACCOUNT ID\/default:device:DEVICENAME@
newGetEntities ::
  GetEntities
newGetEntities =
  GetEntities'
    { namespaceVersion = Prelude.Nothing,
      ids = Prelude.mempty
    }

-- | The version of the user\'s namespace. Defaults to the latest version of
-- the user\'s namespace.
getEntities_namespaceVersion :: Lens.Lens' GetEntities (Prelude.Maybe Prelude.Integer)
getEntities_namespaceVersion = Lens.lens (\GetEntities' {namespaceVersion} -> namespaceVersion) (\s@GetEntities' {} a -> s {namespaceVersion = a} :: GetEntities)

-- | An array of entity IDs.
--
-- The IDs should be in the following format.
--
-- @urn:tdm:REGION\/ACCOUNT ID\/default:device:DEVICENAME@
getEntities_ids :: Lens.Lens' GetEntities [Prelude.Text]
getEntities_ids = Lens.lens (\GetEntities' {ids} -> ids) (\s@GetEntities' {} a -> s {ids = a} :: GetEntities) Prelude.. Lens.coerced

instance Core.AWSRequest GetEntities where
  type AWSResponse GetEntities = GetEntitiesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetEntitiesResponse'
            Prelude.<$> (x Core..?> "descriptions" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetEntities where
  hashWithSalt salt' GetEntities' {..} =
    salt' `Prelude.hashWithSalt` ids
      `Prelude.hashWithSalt` namespaceVersion

instance Prelude.NFData GetEntities where
  rnf GetEntities' {..} =
    Prelude.rnf namespaceVersion
      `Prelude.seq` Prelude.rnf ids

instance Core.ToHeaders GetEntities where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "IotThingsGraphFrontEndService.GetEntities" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetEntities where
  toJSON GetEntities' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("namespaceVersion" Core..=)
              Prelude.<$> namespaceVersion,
            Prelude.Just ("ids" Core..= ids)
          ]
      )

instance Core.ToPath GetEntities where
  toPath = Prelude.const "/"

instance Core.ToQuery GetEntities where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetEntitiesResponse' smart constructor.
data GetEntitiesResponse = GetEntitiesResponse'
  { -- | An array of descriptions for the specified entities.
    descriptions :: Prelude.Maybe [EntityDescription],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEntitiesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'descriptions', 'getEntitiesResponse_descriptions' - An array of descriptions for the specified entities.
--
-- 'httpStatus', 'getEntitiesResponse_httpStatus' - The response's http status code.
newGetEntitiesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetEntitiesResponse
newGetEntitiesResponse pHttpStatus_ =
  GetEntitiesResponse'
    { descriptions =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of descriptions for the specified entities.
getEntitiesResponse_descriptions :: Lens.Lens' GetEntitiesResponse (Prelude.Maybe [EntityDescription])
getEntitiesResponse_descriptions = Lens.lens (\GetEntitiesResponse' {descriptions} -> descriptions) (\s@GetEntitiesResponse' {} a -> s {descriptions = a} :: GetEntitiesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getEntitiesResponse_httpStatus :: Lens.Lens' GetEntitiesResponse Prelude.Int
getEntitiesResponse_httpStatus = Lens.lens (\GetEntitiesResponse' {httpStatus} -> httpStatus) (\s@GetEntitiesResponse' {} a -> s {httpStatus = a} :: GetEntitiesResponse)

instance Prelude.NFData GetEntitiesResponse where
  rnf GetEntitiesResponse' {..} =
    Prelude.rnf descriptions
      `Prelude.seq` Prelude.rnf httpStatus
