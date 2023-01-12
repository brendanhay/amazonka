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
-- Module      : Amazonka.Lightsail.DisableAddOn
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables an add-on for an Amazon Lightsail resource. For more
-- information, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Amazon Lightsail Developer Guide>.
module Amazonka.Lightsail.DisableAddOn
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisableAddOn' smart constructor.
data DisableAddOn = DisableAddOn'
  { -- | The add-on type to disable.
    addOnType :: AddOnType,
    -- | The name of the source resource for which to disable the add-on.
    resourceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
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
disableAddOn_resourceName :: Lens.Lens' DisableAddOn Prelude.Text
disableAddOn_resourceName = Lens.lens (\DisableAddOn' {resourceName} -> resourceName) (\s@DisableAddOn' {} a -> s {resourceName = a} :: DisableAddOn)

instance Core.AWSRequest DisableAddOn where
  type AWSResponse DisableAddOn = DisableAddOnResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DisableAddOnResponse'
            Prelude.<$> (x Data..?> "operations" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisableAddOn where
  hashWithSalt _salt DisableAddOn' {..} =
    _salt `Prelude.hashWithSalt` addOnType
      `Prelude.hashWithSalt` resourceName

instance Prelude.NFData DisableAddOn where
  rnf DisableAddOn' {..} =
    Prelude.rnf addOnType
      `Prelude.seq` Prelude.rnf resourceName

instance Data.ToHeaders DisableAddOn where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.DisableAddOn" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DisableAddOn where
  toJSON DisableAddOn' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("addOnType" Data..= addOnType),
            Prelude.Just ("resourceName" Data..= resourceName)
          ]
      )

instance Data.ToPath DisableAddOn where
  toPath = Prelude.const "/"

instance Data.ToQuery DisableAddOn where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisableAddOnResponse' smart constructor.
data DisableAddOnResponse = DisableAddOnResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Prelude.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DisableAddOnResponse
newDisableAddOnResponse pHttpStatus_ =
  DisableAddOnResponse'
    { operations = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
disableAddOnResponse_operations :: Lens.Lens' DisableAddOnResponse (Prelude.Maybe [Operation])
disableAddOnResponse_operations = Lens.lens (\DisableAddOnResponse' {operations} -> operations) (\s@DisableAddOnResponse' {} a -> s {operations = a} :: DisableAddOnResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
disableAddOnResponse_httpStatus :: Lens.Lens' DisableAddOnResponse Prelude.Int
disableAddOnResponse_httpStatus = Lens.lens (\DisableAddOnResponse' {httpStatus} -> httpStatus) (\s@DisableAddOnResponse' {} a -> s {httpStatus = a} :: DisableAddOnResponse)

instance Prelude.NFData DisableAddOnResponse where
  rnf DisableAddOnResponse' {..} =
    Prelude.rnf operations
      `Prelude.seq` Prelude.rnf httpStatus
