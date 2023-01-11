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
-- Module      : Amazonka.EC2.RegisterInstanceEventNotificationAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a set of tag keys to include in scheduled event notifications
-- for your resources.
--
-- To remove tags, use
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DeregisterInstanceEventNotificationAttributes.html DeregisterInstanceEventNotificationAttributes>.
module Amazonka.EC2.RegisterInstanceEventNotificationAttributes
  ( -- * Creating a Request
    RegisterInstanceEventNotificationAttributes (..),
    newRegisterInstanceEventNotificationAttributes,

    -- * Request Lenses
    registerInstanceEventNotificationAttributes_dryRun,
    registerInstanceEventNotificationAttributes_instanceTagAttribute,

    -- * Destructuring the Response
    RegisterInstanceEventNotificationAttributesResponse (..),
    newRegisterInstanceEventNotificationAttributesResponse,

    -- * Response Lenses
    registerInstanceEventNotificationAttributesResponse_instanceTagAttribute,
    registerInstanceEventNotificationAttributesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRegisterInstanceEventNotificationAttributes' smart constructor.
data RegisterInstanceEventNotificationAttributes = RegisterInstanceEventNotificationAttributes'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | Information about the tag keys to register.
    instanceTagAttribute :: Prelude.Maybe RegisterInstanceTagAttributeRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterInstanceEventNotificationAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'registerInstanceEventNotificationAttributes_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'instanceTagAttribute', 'registerInstanceEventNotificationAttributes_instanceTagAttribute' - Information about the tag keys to register.
newRegisterInstanceEventNotificationAttributes ::
  RegisterInstanceEventNotificationAttributes
newRegisterInstanceEventNotificationAttributes =
  RegisterInstanceEventNotificationAttributes'
    { dryRun =
        Prelude.Nothing,
      instanceTagAttribute =
        Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
registerInstanceEventNotificationAttributes_dryRun :: Lens.Lens' RegisterInstanceEventNotificationAttributes (Prelude.Maybe Prelude.Bool)
registerInstanceEventNotificationAttributes_dryRun = Lens.lens (\RegisterInstanceEventNotificationAttributes' {dryRun} -> dryRun) (\s@RegisterInstanceEventNotificationAttributes' {} a -> s {dryRun = a} :: RegisterInstanceEventNotificationAttributes)

-- | Information about the tag keys to register.
registerInstanceEventNotificationAttributes_instanceTagAttribute :: Lens.Lens' RegisterInstanceEventNotificationAttributes (Prelude.Maybe RegisterInstanceTagAttributeRequest)
registerInstanceEventNotificationAttributes_instanceTagAttribute = Lens.lens (\RegisterInstanceEventNotificationAttributes' {instanceTagAttribute} -> instanceTagAttribute) (\s@RegisterInstanceEventNotificationAttributes' {} a -> s {instanceTagAttribute = a} :: RegisterInstanceEventNotificationAttributes)

instance
  Core.AWSRequest
    RegisterInstanceEventNotificationAttributes
  where
  type
    AWSResponse
      RegisterInstanceEventNotificationAttributes =
      RegisterInstanceEventNotificationAttributesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          RegisterInstanceEventNotificationAttributesResponse'
            Prelude.<$> (x Data..@? "instanceTagAttribute")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    RegisterInstanceEventNotificationAttributes
  where
  hashWithSalt
    _salt
    RegisterInstanceEventNotificationAttributes' {..} =
      _salt `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` instanceTagAttribute

instance
  Prelude.NFData
    RegisterInstanceEventNotificationAttributes
  where
  rnf RegisterInstanceEventNotificationAttributes' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf instanceTagAttribute

instance
  Data.ToHeaders
    RegisterInstanceEventNotificationAttributes
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    RegisterInstanceEventNotificationAttributes
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    RegisterInstanceEventNotificationAttributes
  where
  toQuery
    RegisterInstanceEventNotificationAttributes' {..} =
      Prelude.mconcat
        [ "Action"
            Data.=: ( "RegisterInstanceEventNotificationAttributes" ::
                        Prelude.ByteString
                    ),
          "Version"
            Data.=: ("2016-11-15" :: Prelude.ByteString),
          "DryRun" Data.=: dryRun,
          "InstanceTagAttribute" Data.=: instanceTagAttribute
        ]

-- | /See:/ 'newRegisterInstanceEventNotificationAttributesResponse' smart constructor.
data RegisterInstanceEventNotificationAttributesResponse = RegisterInstanceEventNotificationAttributesResponse'
  { -- | The resulting set of tag keys.
    instanceTagAttribute :: Prelude.Maybe InstanceTagNotificationAttribute,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterInstanceEventNotificationAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceTagAttribute', 'registerInstanceEventNotificationAttributesResponse_instanceTagAttribute' - The resulting set of tag keys.
--
-- 'httpStatus', 'registerInstanceEventNotificationAttributesResponse_httpStatus' - The response's http status code.
newRegisterInstanceEventNotificationAttributesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RegisterInstanceEventNotificationAttributesResponse
newRegisterInstanceEventNotificationAttributesResponse
  pHttpStatus_ =
    RegisterInstanceEventNotificationAttributesResponse'
      { instanceTagAttribute =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | The resulting set of tag keys.
registerInstanceEventNotificationAttributesResponse_instanceTagAttribute :: Lens.Lens' RegisterInstanceEventNotificationAttributesResponse (Prelude.Maybe InstanceTagNotificationAttribute)
registerInstanceEventNotificationAttributesResponse_instanceTagAttribute = Lens.lens (\RegisterInstanceEventNotificationAttributesResponse' {instanceTagAttribute} -> instanceTagAttribute) (\s@RegisterInstanceEventNotificationAttributesResponse' {} a -> s {instanceTagAttribute = a} :: RegisterInstanceEventNotificationAttributesResponse)

-- | The response's http status code.
registerInstanceEventNotificationAttributesResponse_httpStatus :: Lens.Lens' RegisterInstanceEventNotificationAttributesResponse Prelude.Int
registerInstanceEventNotificationAttributesResponse_httpStatus = Lens.lens (\RegisterInstanceEventNotificationAttributesResponse' {httpStatus} -> httpStatus) (\s@RegisterInstanceEventNotificationAttributesResponse' {} a -> s {httpStatus = a} :: RegisterInstanceEventNotificationAttributesResponse)

instance
  Prelude.NFData
    RegisterInstanceEventNotificationAttributesResponse
  where
  rnf
    RegisterInstanceEventNotificationAttributesResponse' {..} =
      Prelude.rnf instanceTagAttribute
        `Prelude.seq` Prelude.rnf httpStatus
