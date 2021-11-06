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
-- Module      : Amazonka.IoTThingsGraph.DissociateEntityFromThing
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Dissociates a device entity from a concrete thing. The action takes only
-- the type of the entity that you need to dissociate because only one
-- entity of a particular type can be associated with a thing.
module Amazonka.IoTThingsGraph.DissociateEntityFromThing
  ( -- * Creating a Request
    DissociateEntityFromThing (..),
    newDissociateEntityFromThing,

    -- * Request Lenses
    dissociateEntityFromThing_thingName,
    dissociateEntityFromThing_entityType,

    -- * Destructuring the Response
    DissociateEntityFromThingResponse (..),
    newDissociateEntityFromThingResponse,

    -- * Response Lenses
    dissociateEntityFromThingResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.IoTThingsGraph.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDissociateEntityFromThing' smart constructor.
data DissociateEntityFromThing = DissociateEntityFromThing'
  { -- | The name of the thing to disassociate.
    thingName :: Prelude.Text,
    -- | The entity type from which to disassociate the thing.
    entityType :: EntityType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DissociateEntityFromThing' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'thingName', 'dissociateEntityFromThing_thingName' - The name of the thing to disassociate.
--
-- 'entityType', 'dissociateEntityFromThing_entityType' - The entity type from which to disassociate the thing.
newDissociateEntityFromThing ::
  -- | 'thingName'
  Prelude.Text ->
  -- | 'entityType'
  EntityType ->
  DissociateEntityFromThing
newDissociateEntityFromThing pThingName_ pEntityType_ =
  DissociateEntityFromThing'
    { thingName = pThingName_,
      entityType = pEntityType_
    }

-- | The name of the thing to disassociate.
dissociateEntityFromThing_thingName :: Lens.Lens' DissociateEntityFromThing Prelude.Text
dissociateEntityFromThing_thingName = Lens.lens (\DissociateEntityFromThing' {thingName} -> thingName) (\s@DissociateEntityFromThing' {} a -> s {thingName = a} :: DissociateEntityFromThing)

-- | The entity type from which to disassociate the thing.
dissociateEntityFromThing_entityType :: Lens.Lens' DissociateEntityFromThing EntityType
dissociateEntityFromThing_entityType = Lens.lens (\DissociateEntityFromThing' {entityType} -> entityType) (\s@DissociateEntityFromThing' {} a -> s {entityType = a} :: DissociateEntityFromThing)

instance Core.AWSRequest DissociateEntityFromThing where
  type
    AWSResponse DissociateEntityFromThing =
      DissociateEntityFromThingResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DissociateEntityFromThingResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DissociateEntityFromThing

instance Prelude.NFData DissociateEntityFromThing

instance Core.ToHeaders DissociateEntityFromThing where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "IotThingsGraphFrontEndService.DissociateEntityFromThing" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DissociateEntityFromThing where
  toJSON DissociateEntityFromThing' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("thingName" Core..= thingName),
            Prelude.Just ("entityType" Core..= entityType)
          ]
      )

instance Core.ToPath DissociateEntityFromThing where
  toPath = Prelude.const "/"

instance Core.ToQuery DissociateEntityFromThing where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDissociateEntityFromThingResponse' smart constructor.
data DissociateEntityFromThingResponse = DissociateEntityFromThingResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DissociateEntityFromThingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'dissociateEntityFromThingResponse_httpStatus' - The response's http status code.
newDissociateEntityFromThingResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DissociateEntityFromThingResponse
newDissociateEntityFromThingResponse pHttpStatus_ =
  DissociateEntityFromThingResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
dissociateEntityFromThingResponse_httpStatus :: Lens.Lens' DissociateEntityFromThingResponse Prelude.Int
dissociateEntityFromThingResponse_httpStatus = Lens.lens (\DissociateEntityFromThingResponse' {httpStatus} -> httpStatus) (\s@DissociateEntityFromThingResponse' {} a -> s {httpStatus = a} :: DissociateEntityFromThingResponse)

instance
  Prelude.NFData
    DissociateEntityFromThingResponse
