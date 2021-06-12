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
-- Module      : Network.AWS.Lightsail.EnableAddOn
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables or modifies an add-on for an Amazon Lightsail resource. For more
-- information, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Lightsail Dev Guide>.
module Network.AWS.Lightsail.EnableAddOn
  ( -- * Creating a Request
    EnableAddOn (..),
    newEnableAddOn,

    -- * Request Lenses
    enableAddOn_resourceName,
    enableAddOn_addOnRequest,

    -- * Destructuring the Response
    EnableAddOnResponse (..),
    newEnableAddOnResponse,

    -- * Response Lenses
    enableAddOnResponse_operations,
    enableAddOnResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newEnableAddOn' smart constructor.
data EnableAddOn = EnableAddOn'
  { -- | The name of the source resource for which to enable or modify the
    -- add-on.
    resourceName :: Core.Text,
    -- | An array of strings representing the add-on to enable or modify.
    addOnRequest :: AddOnRequest
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EnableAddOn' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceName', 'enableAddOn_resourceName' - The name of the source resource for which to enable or modify the
-- add-on.
--
-- 'addOnRequest', 'enableAddOn_addOnRequest' - An array of strings representing the add-on to enable or modify.
newEnableAddOn ::
  -- | 'resourceName'
  Core.Text ->
  -- | 'addOnRequest'
  AddOnRequest ->
  EnableAddOn
newEnableAddOn pResourceName_ pAddOnRequest_ =
  EnableAddOn'
    { resourceName = pResourceName_,
      addOnRequest = pAddOnRequest_
    }

-- | The name of the source resource for which to enable or modify the
-- add-on.
enableAddOn_resourceName :: Lens.Lens' EnableAddOn Core.Text
enableAddOn_resourceName = Lens.lens (\EnableAddOn' {resourceName} -> resourceName) (\s@EnableAddOn' {} a -> s {resourceName = a} :: EnableAddOn)

-- | An array of strings representing the add-on to enable or modify.
enableAddOn_addOnRequest :: Lens.Lens' EnableAddOn AddOnRequest
enableAddOn_addOnRequest = Lens.lens (\EnableAddOn' {addOnRequest} -> addOnRequest) (\s@EnableAddOn' {} a -> s {addOnRequest = a} :: EnableAddOn)

instance Core.AWSRequest EnableAddOn where
  type AWSResponse EnableAddOn = EnableAddOnResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          EnableAddOnResponse'
            Core.<$> (x Core..?> "operations" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable EnableAddOn

instance Core.NFData EnableAddOn

instance Core.ToHeaders EnableAddOn where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.EnableAddOn" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON EnableAddOn where
  toJSON EnableAddOn' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("resourceName" Core..= resourceName),
            Core.Just ("addOnRequest" Core..= addOnRequest)
          ]
      )

instance Core.ToPath EnableAddOn where
  toPath = Core.const "/"

instance Core.ToQuery EnableAddOn where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newEnableAddOnResponse' smart constructor.
data EnableAddOnResponse = EnableAddOnResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Core.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EnableAddOnResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operations', 'enableAddOnResponse_operations' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'httpStatus', 'enableAddOnResponse_httpStatus' - The response's http status code.
newEnableAddOnResponse ::
  -- | 'httpStatus'
  Core.Int ->
  EnableAddOnResponse
newEnableAddOnResponse pHttpStatus_ =
  EnableAddOnResponse'
    { operations = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
enableAddOnResponse_operations :: Lens.Lens' EnableAddOnResponse (Core.Maybe [Operation])
enableAddOnResponse_operations = Lens.lens (\EnableAddOnResponse' {operations} -> operations) (\s@EnableAddOnResponse' {} a -> s {operations = a} :: EnableAddOnResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
enableAddOnResponse_httpStatus :: Lens.Lens' EnableAddOnResponse Core.Int
enableAddOnResponse_httpStatus = Lens.lens (\EnableAddOnResponse' {httpStatus} -> httpStatus) (\s@EnableAddOnResponse' {} a -> s {httpStatus = a} :: EnableAddOnResponse)

instance Core.NFData EnableAddOnResponse
