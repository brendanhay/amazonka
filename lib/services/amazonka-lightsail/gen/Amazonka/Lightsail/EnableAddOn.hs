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
-- Module      : Amazonka.Lightsail.EnableAddOn
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables or modifies an add-on for an Amazon Lightsail resource. For more
-- information, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Amazon Lightsail Developer Guide>.
module Amazonka.Lightsail.EnableAddOn
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newEnableAddOn' smart constructor.
data EnableAddOn = EnableAddOn'
  { -- | The name of the source resource for which to enable or modify the
    -- add-on.
    resourceName :: Prelude.Text,
    -- | An array of strings representing the add-on to enable or modify.
    addOnRequest :: AddOnRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
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
enableAddOn_resourceName :: Lens.Lens' EnableAddOn Prelude.Text
enableAddOn_resourceName = Lens.lens (\EnableAddOn' {resourceName} -> resourceName) (\s@EnableAddOn' {} a -> s {resourceName = a} :: EnableAddOn)

-- | An array of strings representing the add-on to enable or modify.
enableAddOn_addOnRequest :: Lens.Lens' EnableAddOn AddOnRequest
enableAddOn_addOnRequest = Lens.lens (\EnableAddOn' {addOnRequest} -> addOnRequest) (\s@EnableAddOn' {} a -> s {addOnRequest = a} :: EnableAddOn)

instance Core.AWSRequest EnableAddOn where
  type AWSResponse EnableAddOn = EnableAddOnResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          EnableAddOnResponse'
            Prelude.<$> (x Core..?> "operations" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable EnableAddOn where
  hashWithSalt _salt EnableAddOn' {..} =
    _salt `Prelude.hashWithSalt` resourceName
      `Prelude.hashWithSalt` addOnRequest

instance Prelude.NFData EnableAddOn where
  rnf EnableAddOn' {..} =
    Prelude.rnf resourceName
      `Prelude.seq` Prelude.rnf addOnRequest

instance Core.ToHeaders EnableAddOn where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.EnableAddOn" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON EnableAddOn where
  toJSON EnableAddOn' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("resourceName" Core..= resourceName),
            Prelude.Just ("addOnRequest" Core..= addOnRequest)
          ]
      )

instance Core.ToPath EnableAddOn where
  toPath = Prelude.const "/"

instance Core.ToQuery EnableAddOn where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newEnableAddOnResponse' smart constructor.
data EnableAddOnResponse = EnableAddOnResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Prelude.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  EnableAddOnResponse
newEnableAddOnResponse pHttpStatus_ =
  EnableAddOnResponse'
    { operations = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
enableAddOnResponse_operations :: Lens.Lens' EnableAddOnResponse (Prelude.Maybe [Operation])
enableAddOnResponse_operations = Lens.lens (\EnableAddOnResponse' {operations} -> operations) (\s@EnableAddOnResponse' {} a -> s {operations = a} :: EnableAddOnResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
enableAddOnResponse_httpStatus :: Lens.Lens' EnableAddOnResponse Prelude.Int
enableAddOnResponse_httpStatus = Lens.lens (\EnableAddOnResponse' {httpStatus} -> httpStatus) (\s@EnableAddOnResponse' {} a -> s {httpStatus = a} :: EnableAddOnResponse)

instance Prelude.NFData EnableAddOnResponse where
  rnf EnableAddOnResponse' {..} =
    Prelude.rnf operations
      `Prelude.seq` Prelude.rnf httpStatus
