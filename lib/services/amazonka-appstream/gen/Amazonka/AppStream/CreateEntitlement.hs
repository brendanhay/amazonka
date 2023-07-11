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
-- Module      : Amazonka.AppStream.CreateEntitlement
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new entitlement. Entitlements control access to specific
-- applications within a stack, based on user attributes. Entitlements
-- apply to SAML 2.0 federated user identities. Amazon AppStream 2.0 user
-- pool and streaming URL users are entitled to all applications in a
-- stack. Entitlements don\'t apply to the desktop stream view application,
-- or to applications managed by a dynamic app provider using the Dynamic
-- Application Framework.
module Amazonka.AppStream.CreateEntitlement
  ( -- * Creating a Request
    CreateEntitlement (..),
    newCreateEntitlement,

    -- * Request Lenses
    createEntitlement_description,
    createEntitlement_name,
    createEntitlement_stackName,
    createEntitlement_appVisibility,
    createEntitlement_attributes,

    -- * Destructuring the Response
    CreateEntitlementResponse (..),
    newCreateEntitlementResponse,

    -- * Response Lenses
    createEntitlementResponse_entitlement,
    createEntitlementResponse_httpStatus,
  )
where

import Amazonka.AppStream.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateEntitlement' smart constructor.
data CreateEntitlement = CreateEntitlement'
  { -- | The description of the entitlement.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the entitlement.
    name :: Prelude.Text,
    -- | The name of the stack with which the entitlement is associated.
    stackName :: Prelude.Text,
    -- | Specifies whether all or selected apps are entitled.
    appVisibility :: AppVisibility,
    -- | The attributes of the entitlement.
    attributes :: Prelude.NonEmpty EntitlementAttribute
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateEntitlement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createEntitlement_description' - The description of the entitlement.
--
-- 'name', 'createEntitlement_name' - The name of the entitlement.
--
-- 'stackName', 'createEntitlement_stackName' - The name of the stack with which the entitlement is associated.
--
-- 'appVisibility', 'createEntitlement_appVisibility' - Specifies whether all or selected apps are entitled.
--
-- 'attributes', 'createEntitlement_attributes' - The attributes of the entitlement.
newCreateEntitlement ::
  -- | 'name'
  Prelude.Text ->
  -- | 'stackName'
  Prelude.Text ->
  -- | 'appVisibility'
  AppVisibility ->
  -- | 'attributes'
  Prelude.NonEmpty EntitlementAttribute ->
  CreateEntitlement
newCreateEntitlement
  pName_
  pStackName_
  pAppVisibility_
  pAttributes_ =
    CreateEntitlement'
      { description = Prelude.Nothing,
        name = pName_,
        stackName = pStackName_,
        appVisibility = pAppVisibility_,
        attributes = Lens.coerced Lens.# pAttributes_
      }

-- | The description of the entitlement.
createEntitlement_description :: Lens.Lens' CreateEntitlement (Prelude.Maybe Prelude.Text)
createEntitlement_description = Lens.lens (\CreateEntitlement' {description} -> description) (\s@CreateEntitlement' {} a -> s {description = a} :: CreateEntitlement)

-- | The name of the entitlement.
createEntitlement_name :: Lens.Lens' CreateEntitlement Prelude.Text
createEntitlement_name = Lens.lens (\CreateEntitlement' {name} -> name) (\s@CreateEntitlement' {} a -> s {name = a} :: CreateEntitlement)

-- | The name of the stack with which the entitlement is associated.
createEntitlement_stackName :: Lens.Lens' CreateEntitlement Prelude.Text
createEntitlement_stackName = Lens.lens (\CreateEntitlement' {stackName} -> stackName) (\s@CreateEntitlement' {} a -> s {stackName = a} :: CreateEntitlement)

-- | Specifies whether all or selected apps are entitled.
createEntitlement_appVisibility :: Lens.Lens' CreateEntitlement AppVisibility
createEntitlement_appVisibility = Lens.lens (\CreateEntitlement' {appVisibility} -> appVisibility) (\s@CreateEntitlement' {} a -> s {appVisibility = a} :: CreateEntitlement)

-- | The attributes of the entitlement.
createEntitlement_attributes :: Lens.Lens' CreateEntitlement (Prelude.NonEmpty EntitlementAttribute)
createEntitlement_attributes = Lens.lens (\CreateEntitlement' {attributes} -> attributes) (\s@CreateEntitlement' {} a -> s {attributes = a} :: CreateEntitlement) Prelude.. Lens.coerced

instance Core.AWSRequest CreateEntitlement where
  type
    AWSResponse CreateEntitlement =
      CreateEntitlementResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateEntitlementResponse'
            Prelude.<$> (x Data..?> "Entitlement")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateEntitlement where
  hashWithSalt _salt CreateEntitlement' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` stackName
      `Prelude.hashWithSalt` appVisibility
      `Prelude.hashWithSalt` attributes

instance Prelude.NFData CreateEntitlement where
  rnf CreateEntitlement' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf stackName
      `Prelude.seq` Prelude.rnf appVisibility
      `Prelude.seq` Prelude.rnf attributes

instance Data.ToHeaders CreateEntitlement where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PhotonAdminProxyService.CreateEntitlement" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateEntitlement where
  toJSON CreateEntitlement' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("StackName" Data..= stackName),
            Prelude.Just ("AppVisibility" Data..= appVisibility),
            Prelude.Just ("Attributes" Data..= attributes)
          ]
      )

instance Data.ToPath CreateEntitlement where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateEntitlement where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateEntitlementResponse' smart constructor.
data CreateEntitlementResponse = CreateEntitlementResponse'
  { -- | The entitlement.
    entitlement :: Prelude.Maybe Entitlement,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateEntitlementResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entitlement', 'createEntitlementResponse_entitlement' - The entitlement.
--
-- 'httpStatus', 'createEntitlementResponse_httpStatus' - The response's http status code.
newCreateEntitlementResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateEntitlementResponse
newCreateEntitlementResponse pHttpStatus_ =
  CreateEntitlementResponse'
    { entitlement =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The entitlement.
createEntitlementResponse_entitlement :: Lens.Lens' CreateEntitlementResponse (Prelude.Maybe Entitlement)
createEntitlementResponse_entitlement = Lens.lens (\CreateEntitlementResponse' {entitlement} -> entitlement) (\s@CreateEntitlementResponse' {} a -> s {entitlement = a} :: CreateEntitlementResponse)

-- | The response's http status code.
createEntitlementResponse_httpStatus :: Lens.Lens' CreateEntitlementResponse Prelude.Int
createEntitlementResponse_httpStatus = Lens.lens (\CreateEntitlementResponse' {httpStatus} -> httpStatus) (\s@CreateEntitlementResponse' {} a -> s {httpStatus = a} :: CreateEntitlementResponse)

instance Prelude.NFData CreateEntitlementResponse where
  rnf CreateEntitlementResponse' {..} =
    Prelude.rnf entitlement
      `Prelude.seq` Prelude.rnf httpStatus
