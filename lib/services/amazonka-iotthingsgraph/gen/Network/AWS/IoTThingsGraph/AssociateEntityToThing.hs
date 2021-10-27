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
-- Module      : Network.AWS.IoTThingsGraph.AssociateEntityToThing
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a device with a concrete thing that is in the user\'s
-- registry.
--
-- A thing can be associated with only one device at a time. If you
-- associate a thing with a new device id, its previous association will be
-- removed.
module Network.AWS.IoTThingsGraph.AssociateEntityToThing
  ( -- * Creating a Request
    AssociateEntityToThing (..),
    newAssociateEntityToThing,

    -- * Request Lenses
    associateEntityToThing_namespaceVersion,
    associateEntityToThing_thingName,
    associateEntityToThing_entityId,

    -- * Destructuring the Response
    AssociateEntityToThingResponse (..),
    newAssociateEntityToThingResponse,

    -- * Response Lenses
    associateEntityToThingResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTThingsGraph.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAssociateEntityToThing' smart constructor.
data AssociateEntityToThing = AssociateEntityToThing'
  { -- | The version of the user\'s namespace. Defaults to the latest version of
    -- the user\'s namespace.
    namespaceVersion :: Prelude.Maybe Prelude.Integer,
    -- | The name of the thing to which the entity is to be associated.
    thingName :: Prelude.Text,
    -- | The ID of the device to be associated with the thing.
    --
    -- The ID should be in the following format.
    --
    -- @urn:tdm:REGION\/ACCOUNT ID\/default:device:DEVICENAME@
    entityId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateEntityToThing' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'namespaceVersion', 'associateEntityToThing_namespaceVersion' - The version of the user\'s namespace. Defaults to the latest version of
-- the user\'s namespace.
--
-- 'thingName', 'associateEntityToThing_thingName' - The name of the thing to which the entity is to be associated.
--
-- 'entityId', 'associateEntityToThing_entityId' - The ID of the device to be associated with the thing.
--
-- The ID should be in the following format.
--
-- @urn:tdm:REGION\/ACCOUNT ID\/default:device:DEVICENAME@
newAssociateEntityToThing ::
  -- | 'thingName'
  Prelude.Text ->
  -- | 'entityId'
  Prelude.Text ->
  AssociateEntityToThing
newAssociateEntityToThing pThingName_ pEntityId_ =
  AssociateEntityToThing'
    { namespaceVersion =
        Prelude.Nothing,
      thingName = pThingName_,
      entityId = pEntityId_
    }

-- | The version of the user\'s namespace. Defaults to the latest version of
-- the user\'s namespace.
associateEntityToThing_namespaceVersion :: Lens.Lens' AssociateEntityToThing (Prelude.Maybe Prelude.Integer)
associateEntityToThing_namespaceVersion = Lens.lens (\AssociateEntityToThing' {namespaceVersion} -> namespaceVersion) (\s@AssociateEntityToThing' {} a -> s {namespaceVersion = a} :: AssociateEntityToThing)

-- | The name of the thing to which the entity is to be associated.
associateEntityToThing_thingName :: Lens.Lens' AssociateEntityToThing Prelude.Text
associateEntityToThing_thingName = Lens.lens (\AssociateEntityToThing' {thingName} -> thingName) (\s@AssociateEntityToThing' {} a -> s {thingName = a} :: AssociateEntityToThing)

-- | The ID of the device to be associated with the thing.
--
-- The ID should be in the following format.
--
-- @urn:tdm:REGION\/ACCOUNT ID\/default:device:DEVICENAME@
associateEntityToThing_entityId :: Lens.Lens' AssociateEntityToThing Prelude.Text
associateEntityToThing_entityId = Lens.lens (\AssociateEntityToThing' {entityId} -> entityId) (\s@AssociateEntityToThing' {} a -> s {entityId = a} :: AssociateEntityToThing)

instance Core.AWSRequest AssociateEntityToThing where
  type
    AWSResponse AssociateEntityToThing =
      AssociateEntityToThingResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateEntityToThingResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociateEntityToThing

instance Prelude.NFData AssociateEntityToThing

instance Core.ToHeaders AssociateEntityToThing where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "IotThingsGraphFrontEndService.AssociateEntityToThing" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON AssociateEntityToThing where
  toJSON AssociateEntityToThing' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("namespaceVersion" Core..=)
              Prelude.<$> namespaceVersion,
            Prelude.Just ("thingName" Core..= thingName),
            Prelude.Just ("entityId" Core..= entityId)
          ]
      )

instance Core.ToPath AssociateEntityToThing where
  toPath = Prelude.const "/"

instance Core.ToQuery AssociateEntityToThing where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateEntityToThingResponse' smart constructor.
data AssociateEntityToThingResponse = AssociateEntityToThingResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateEntityToThingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'associateEntityToThingResponse_httpStatus' - The response's http status code.
newAssociateEntityToThingResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateEntityToThingResponse
newAssociateEntityToThingResponse pHttpStatus_ =
  AssociateEntityToThingResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
associateEntityToThingResponse_httpStatus :: Lens.Lens' AssociateEntityToThingResponse Prelude.Int
associateEntityToThingResponse_httpStatus = Lens.lens (\AssociateEntityToThingResponse' {httpStatus} -> httpStatus) (\s@AssociateEntityToThingResponse' {} a -> s {httpStatus = a} :: AssociateEntityToThingResponse)

instance
  Prelude.NFData
    AssociateEntityToThingResponse
