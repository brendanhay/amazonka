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
-- Module      : Network.AWS.Lightsail.DisableAddOn
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables an add-on for an Amazon Lightsail resource. For more
-- information, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Lightsail Dev Guide>.
module Network.AWS.Lightsail.DisableAddOn
  ( -- * Creating a Request
    DisableAddOn (..),
    newDisableAddOn,

    -- * Request Lenses
    disableAddOn_addOnType,
    disableAddOn_resourceName,

    -- * Destructuring the Response
    DisableAddOnResponse (..),
    newDisableAddOnResponse,

    -- * Response Lenses
    disableAddOnResponse_operations,
    disableAddOnResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDisableAddOn' smart constructor.
data DisableAddOn = DisableAddOn'
  { -- | The add-on type to disable.
    addOnType :: AddOnType,
    -- | The name of the source resource for which to disable the add-on.
    resourceName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DisableAddOn' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addOnType', 'disableAddOn_addOnType' - The add-on type to disable.
--
-- 'resourceName', 'disableAddOn_resourceName' - The name of the source resource for which to disable the add-on.
newDisableAddOn ::
  -- | 'addOnType'
  AddOnType ->
  -- | 'resourceName'
  Core.Text ->
  DisableAddOn
newDisableAddOn pAddOnType_ pResourceName_ =
  DisableAddOn'
    { addOnType = pAddOnType_,
      resourceName = pResourceName_
    }

-- | The add-on type to disable.
disableAddOn_addOnType :: Lens.Lens' DisableAddOn AddOnType
disableAddOn_addOnType = Lens.lens (\DisableAddOn' {addOnType} -> addOnType) (\s@DisableAddOn' {} a -> s {addOnType = a} :: DisableAddOn)

-- | The name of the source resource for which to disable the add-on.
disableAddOn_resourceName :: Lens.Lens' DisableAddOn Core.Text
disableAddOn_resourceName = Lens.lens (\DisableAddOn' {resourceName} -> resourceName) (\s@DisableAddOn' {} a -> s {resourceName = a} :: DisableAddOn)

instance Core.AWSRequest DisableAddOn where
  type AWSResponse DisableAddOn = DisableAddOnResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DisableAddOnResponse'
            Core.<$> (x Core..?> "operations" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DisableAddOn

instance Core.NFData DisableAddOn

instance Core.ToHeaders DisableAddOn where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.DisableAddOn" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DisableAddOn where
  toJSON DisableAddOn' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("addOnType" Core..= addOnType),
            Core.Just ("resourceName" Core..= resourceName)
          ]
      )

instance Core.ToPath DisableAddOn where
  toPath = Core.const "/"

instance Core.ToQuery DisableAddOn where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDisableAddOnResponse' smart constructor.
data DisableAddOnResponse = DisableAddOnResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Core.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DisableAddOnResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operations', 'disableAddOnResponse_operations' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'httpStatus', 'disableAddOnResponse_httpStatus' - The response's http status code.
newDisableAddOnResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DisableAddOnResponse
newDisableAddOnResponse pHttpStatus_ =
  DisableAddOnResponse'
    { operations = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
disableAddOnResponse_operations :: Lens.Lens' DisableAddOnResponse (Core.Maybe [Operation])
disableAddOnResponse_operations = Lens.lens (\DisableAddOnResponse' {operations} -> operations) (\s@DisableAddOnResponse' {} a -> s {operations = a} :: DisableAddOnResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
disableAddOnResponse_httpStatus :: Lens.Lens' DisableAddOnResponse Core.Int
disableAddOnResponse_httpStatus = Lens.lens (\DisableAddOnResponse' {httpStatus} -> httpStatus) (\s@DisableAddOnResponse' {} a -> s {httpStatus = a} :: DisableAddOnResponse)

instance Core.NFData DisableAddOnResponse
