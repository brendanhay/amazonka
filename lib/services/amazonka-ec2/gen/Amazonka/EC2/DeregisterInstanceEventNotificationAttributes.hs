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
-- Module      : Amazonka.EC2.DeregisterInstanceEventNotificationAttributes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters tag keys to prevent tags that have the specified tag keys
-- from being included in scheduled event notifications for resources in
-- the Region.
module Amazonka.EC2.DeregisterInstanceEventNotificationAttributes
  ( -- * Creating a Request
    DeregisterInstanceEventNotificationAttributes (..),
    newDeregisterInstanceEventNotificationAttributes,

    -- * Request Lenses
    deregisterInstanceEventNotificationAttributes_instanceTagAttribute,
    deregisterInstanceEventNotificationAttributes_dryRun,

    -- * Destructuring the Response
    DeregisterInstanceEventNotificationAttributesResponse (..),
    newDeregisterInstanceEventNotificationAttributesResponse,

    -- * Response Lenses
    deregisterInstanceEventNotificationAttributesResponse_instanceTagAttribute,
    deregisterInstanceEventNotificationAttributesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeregisterInstanceEventNotificationAttributes' smart constructor.
data DeregisterInstanceEventNotificationAttributes = DeregisterInstanceEventNotificationAttributes'
  { -- | Information about the tag keys to deregister.
    instanceTagAttribute :: Prelude.Maybe DeregisterInstanceTagAttributeRequest,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeregisterInstanceEventNotificationAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceTagAttribute', 'deregisterInstanceEventNotificationAttributes_instanceTagAttribute' - Information about the tag keys to deregister.
--
-- 'dryRun', 'deregisterInstanceEventNotificationAttributes_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
newDeregisterInstanceEventNotificationAttributes ::
  DeregisterInstanceEventNotificationAttributes
newDeregisterInstanceEventNotificationAttributes =
  DeregisterInstanceEventNotificationAttributes'
    { instanceTagAttribute =
        Prelude.Nothing,
      dryRun = Prelude.Nothing
    }

-- | Information about the tag keys to deregister.
deregisterInstanceEventNotificationAttributes_instanceTagAttribute :: Lens.Lens' DeregisterInstanceEventNotificationAttributes (Prelude.Maybe DeregisterInstanceTagAttributeRequest)
deregisterInstanceEventNotificationAttributes_instanceTagAttribute = Lens.lens (\DeregisterInstanceEventNotificationAttributes' {instanceTagAttribute} -> instanceTagAttribute) (\s@DeregisterInstanceEventNotificationAttributes' {} a -> s {instanceTagAttribute = a} :: DeregisterInstanceEventNotificationAttributes)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deregisterInstanceEventNotificationAttributes_dryRun :: Lens.Lens' DeregisterInstanceEventNotificationAttributes (Prelude.Maybe Prelude.Bool)
deregisterInstanceEventNotificationAttributes_dryRun = Lens.lens (\DeregisterInstanceEventNotificationAttributes' {dryRun} -> dryRun) (\s@DeregisterInstanceEventNotificationAttributes' {} a -> s {dryRun = a} :: DeregisterInstanceEventNotificationAttributes)

instance
  Core.AWSRequest
    DeregisterInstanceEventNotificationAttributes
  where
  type
    AWSResponse
      DeregisterInstanceEventNotificationAttributes =
      DeregisterInstanceEventNotificationAttributesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DeregisterInstanceEventNotificationAttributesResponse'
            Prelude.<$> (x Core..@? "instanceTagAttribute")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeregisterInstanceEventNotificationAttributes
  where
  hashWithSalt
    _salt
    DeregisterInstanceEventNotificationAttributes' {..} =
      _salt `Prelude.hashWithSalt` instanceTagAttribute
        `Prelude.hashWithSalt` dryRun

instance
  Prelude.NFData
    DeregisterInstanceEventNotificationAttributes
  where
  rnf
    DeregisterInstanceEventNotificationAttributes' {..} =
      Prelude.rnf instanceTagAttribute
        `Prelude.seq` Prelude.rnf dryRun

instance
  Core.ToHeaders
    DeregisterInstanceEventNotificationAttributes
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    DeregisterInstanceEventNotificationAttributes
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DeregisterInstanceEventNotificationAttributes
  where
  toQuery
    DeregisterInstanceEventNotificationAttributes' {..} =
      Prelude.mconcat
        [ "Action"
            Core.=: ( "DeregisterInstanceEventNotificationAttributes" ::
                        Prelude.ByteString
                    ),
          "Version"
            Core.=: ("2016-11-15" :: Prelude.ByteString),
          "InstanceTagAttribute" Core.=: instanceTagAttribute,
          "DryRun" Core.=: dryRun
        ]

-- | /See:/ 'newDeregisterInstanceEventNotificationAttributesResponse' smart constructor.
data DeregisterInstanceEventNotificationAttributesResponse = DeregisterInstanceEventNotificationAttributesResponse'
  { -- | The resulting set of tag keys.
    instanceTagAttribute :: Prelude.Maybe InstanceTagNotificationAttribute,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeregisterInstanceEventNotificationAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceTagAttribute', 'deregisterInstanceEventNotificationAttributesResponse_instanceTagAttribute' - The resulting set of tag keys.
--
-- 'httpStatus', 'deregisterInstanceEventNotificationAttributesResponse_httpStatus' - The response's http status code.
newDeregisterInstanceEventNotificationAttributesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeregisterInstanceEventNotificationAttributesResponse
newDeregisterInstanceEventNotificationAttributesResponse
  pHttpStatus_ =
    DeregisterInstanceEventNotificationAttributesResponse'
      { instanceTagAttribute =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | The resulting set of tag keys.
deregisterInstanceEventNotificationAttributesResponse_instanceTagAttribute :: Lens.Lens' DeregisterInstanceEventNotificationAttributesResponse (Prelude.Maybe InstanceTagNotificationAttribute)
deregisterInstanceEventNotificationAttributesResponse_instanceTagAttribute = Lens.lens (\DeregisterInstanceEventNotificationAttributesResponse' {instanceTagAttribute} -> instanceTagAttribute) (\s@DeregisterInstanceEventNotificationAttributesResponse' {} a -> s {instanceTagAttribute = a} :: DeregisterInstanceEventNotificationAttributesResponse)

-- | The response's http status code.
deregisterInstanceEventNotificationAttributesResponse_httpStatus :: Lens.Lens' DeregisterInstanceEventNotificationAttributesResponse Prelude.Int
deregisterInstanceEventNotificationAttributesResponse_httpStatus = Lens.lens (\DeregisterInstanceEventNotificationAttributesResponse' {httpStatus} -> httpStatus) (\s@DeregisterInstanceEventNotificationAttributesResponse' {} a -> s {httpStatus = a} :: DeregisterInstanceEventNotificationAttributesResponse)

instance
  Prelude.NFData
    DeregisterInstanceEventNotificationAttributesResponse
  where
  rnf
    DeregisterInstanceEventNotificationAttributesResponse' {..} =
      Prelude.rnf instanceTagAttribute
        `Prelude.seq` Prelude.rnf httpStatus
