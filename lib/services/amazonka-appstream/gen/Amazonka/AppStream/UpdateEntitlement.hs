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
-- Module      : Amazonka.AppStream.UpdateEntitlement
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified entitlement.
module Amazonka.AppStream.UpdateEntitlement
  ( -- * Creating a Request
    UpdateEntitlement (..),
    newUpdateEntitlement,

    -- * Request Lenses
    updateEntitlement_appVisibility,
    updateEntitlement_attributes,
    updateEntitlement_description,
    updateEntitlement_name,
    updateEntitlement_stackName,

    -- * Destructuring the Response
    UpdateEntitlementResponse (..),
    newUpdateEntitlementResponse,

    -- * Response Lenses
    updateEntitlementResponse_entitlement,
    updateEntitlementResponse_httpStatus,
  )
where

import Amazonka.AppStream.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateEntitlement' smart constructor.
data UpdateEntitlement = UpdateEntitlement'
  { -- | Specifies whether all or only selected apps are entitled.
    appVisibility :: Prelude.Maybe AppVisibility,
    -- | The attributes of the entitlement.
    attributes :: Prelude.Maybe (Prelude.NonEmpty EntitlementAttribute),
    -- | The description of the entitlement.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the entitlement.
    name :: Prelude.Text,
    -- | The name of the stack with which the entitlement is associated.
    stackName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateEntitlement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appVisibility', 'updateEntitlement_appVisibility' - Specifies whether all or only selected apps are entitled.
--
-- 'attributes', 'updateEntitlement_attributes' - The attributes of the entitlement.
--
-- 'description', 'updateEntitlement_description' - The description of the entitlement.
--
-- 'name', 'updateEntitlement_name' - The name of the entitlement.
--
-- 'stackName', 'updateEntitlement_stackName' - The name of the stack with which the entitlement is associated.
newUpdateEntitlement ::
  -- | 'name'
  Prelude.Text ->
  -- | 'stackName'
  Prelude.Text ->
  UpdateEntitlement
newUpdateEntitlement pName_ pStackName_ =
  UpdateEntitlement'
    { appVisibility = Prelude.Nothing,
      attributes = Prelude.Nothing,
      description = Prelude.Nothing,
      name = pName_,
      stackName = pStackName_
    }

-- | Specifies whether all or only selected apps are entitled.
updateEntitlement_appVisibility :: Lens.Lens' UpdateEntitlement (Prelude.Maybe AppVisibility)
updateEntitlement_appVisibility = Lens.lens (\UpdateEntitlement' {appVisibility} -> appVisibility) (\s@UpdateEntitlement' {} a -> s {appVisibility = a} :: UpdateEntitlement)

-- | The attributes of the entitlement.
updateEntitlement_attributes :: Lens.Lens' UpdateEntitlement (Prelude.Maybe (Prelude.NonEmpty EntitlementAttribute))
updateEntitlement_attributes = Lens.lens (\UpdateEntitlement' {attributes} -> attributes) (\s@UpdateEntitlement' {} a -> s {attributes = a} :: UpdateEntitlement) Prelude.. Lens.mapping Lens.coerced

-- | The description of the entitlement.
updateEntitlement_description :: Lens.Lens' UpdateEntitlement (Prelude.Maybe Prelude.Text)
updateEntitlement_description = Lens.lens (\UpdateEntitlement' {description} -> description) (\s@UpdateEntitlement' {} a -> s {description = a} :: UpdateEntitlement)

-- | The name of the entitlement.
updateEntitlement_name :: Lens.Lens' UpdateEntitlement Prelude.Text
updateEntitlement_name = Lens.lens (\UpdateEntitlement' {name} -> name) (\s@UpdateEntitlement' {} a -> s {name = a} :: UpdateEntitlement)

-- | The name of the stack with which the entitlement is associated.
updateEntitlement_stackName :: Lens.Lens' UpdateEntitlement Prelude.Text
updateEntitlement_stackName = Lens.lens (\UpdateEntitlement' {stackName} -> stackName) (\s@UpdateEntitlement' {} a -> s {stackName = a} :: UpdateEntitlement)

instance Core.AWSRequest UpdateEntitlement where
  type
    AWSResponse UpdateEntitlement =
      UpdateEntitlementResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateEntitlementResponse'
            Prelude.<$> (x Data..?> "Entitlement")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateEntitlement where
  hashWithSalt _salt UpdateEntitlement' {..} =
    _salt
      `Prelude.hashWithSalt` appVisibility
      `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` stackName

instance Prelude.NFData UpdateEntitlement where
  rnf UpdateEntitlement' {..} =
    Prelude.rnf appVisibility `Prelude.seq`
      Prelude.rnf attributes `Prelude.seq`
        Prelude.rnf description `Prelude.seq`
          Prelude.rnf name `Prelude.seq`
            Prelude.rnf stackName

instance Data.ToHeaders UpdateEntitlement where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PhotonAdminProxyService.UpdateEntitlement" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateEntitlement where
  toJSON UpdateEntitlement' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AppVisibility" Data..=) Prelude.<$> appVisibility,
            ("Attributes" Data..=) Prelude.<$> attributes,
            ("Description" Data..=) Prelude.<$> description,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("StackName" Data..= stackName)
          ]
      )

instance Data.ToPath UpdateEntitlement where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateEntitlement where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateEntitlementResponse' smart constructor.
data UpdateEntitlementResponse = UpdateEntitlementResponse'
  { -- | The entitlement.
    entitlement :: Prelude.Maybe Entitlement,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateEntitlementResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entitlement', 'updateEntitlementResponse_entitlement' - The entitlement.
--
-- 'httpStatus', 'updateEntitlementResponse_httpStatus' - The response's http status code.
newUpdateEntitlementResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateEntitlementResponse
newUpdateEntitlementResponse pHttpStatus_ =
  UpdateEntitlementResponse'
    { entitlement =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The entitlement.
updateEntitlementResponse_entitlement :: Lens.Lens' UpdateEntitlementResponse (Prelude.Maybe Entitlement)
updateEntitlementResponse_entitlement = Lens.lens (\UpdateEntitlementResponse' {entitlement} -> entitlement) (\s@UpdateEntitlementResponse' {} a -> s {entitlement = a} :: UpdateEntitlementResponse)

-- | The response's http status code.
updateEntitlementResponse_httpStatus :: Lens.Lens' UpdateEntitlementResponse Prelude.Int
updateEntitlementResponse_httpStatus = Lens.lens (\UpdateEntitlementResponse' {httpStatus} -> httpStatus) (\s@UpdateEntitlementResponse' {} a -> s {httpStatus = a} :: UpdateEntitlementResponse)

instance Prelude.NFData UpdateEntitlementResponse where
  rnf UpdateEntitlementResponse' {..} =
    Prelude.rnf entitlement `Prelude.seq`
      Prelude.rnf httpStatus
