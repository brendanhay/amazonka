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
-- Module      : Amazonka.QuickSight.CreateAccountCustomization
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates Amazon QuickSight customizations for the current Amazon Web
-- Services Region. Currently, you can add a custom default theme by using
-- the @CreateAccountCustomization@ or @UpdateAccountCustomization@ API
-- operation. To further customize Amazon QuickSight by removing Amazon
-- QuickSight sample assets and videos for all new users, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/customizing-quicksight.html Customizing Amazon QuickSight>
-- in the /Amazon QuickSight User Guide./
--
-- You can create customizations for your Amazon Web Services account or,
-- if you specify a namespace, for a QuickSight namespace instead.
-- Customizations that apply to a namespace always override customizations
-- that apply to an Amazon Web Services account. To find out which
-- customizations apply, use the @DescribeAccountCustomization@ API
-- operation.
--
-- Before you use the @CreateAccountCustomization@ API operation to add a
-- theme as the namespace default, make sure that you first share the theme
-- with the namespace. If you don\'t share it with the namespace, the theme
-- isn\'t visible to your users even if you make it the default theme. To
-- check if the theme is shared, view the current permissions by using the
-- @ DescribeThemePermissions @ API operation. To share the theme, grant
-- permissions by using the @ UpdateThemePermissions @ API operation.
module Amazonka.QuickSight.CreateAccountCustomization
  ( -- * Creating a Request
    CreateAccountCustomization (..),
    newCreateAccountCustomization,

    -- * Request Lenses
    createAccountCustomization_tags,
    createAccountCustomization_namespace,
    createAccountCustomization_awsAccountId,
    createAccountCustomization_accountCustomization,

    -- * Destructuring the Response
    CreateAccountCustomizationResponse (..),
    newCreateAccountCustomizationResponse,

    -- * Response Lenses
    createAccountCustomizationResponse_awsAccountId,
    createAccountCustomizationResponse_requestId,
    createAccountCustomizationResponse_arn,
    createAccountCustomizationResponse_namespace,
    createAccountCustomizationResponse_accountCustomization,
    createAccountCustomizationResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateAccountCustomization' smart constructor.
data CreateAccountCustomization = CreateAccountCustomization'
  { -- | A list of the tags that you want to attach to this resource.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The Amazon QuickSight namespace that you want to add customizations to.
    namespace :: Prelude.Maybe Prelude.Text,
    -- | The ID for the Amazon Web Services account that you want to customize
    -- Amazon QuickSight for.
    awsAccountId :: Prelude.Text,
    -- | The Amazon QuickSight customizations you\'re adding in the current
    -- Amazon Web Services Region. You can add these to an Amazon Web Services
    -- account and a QuickSight namespace.
    --
    -- For example, you can add a default theme by setting
    -- @AccountCustomization@ to the midnight theme:
    -- @\"AccountCustomization\": { \"DefaultTheme\": \"arn:aws:quicksight::aws:theme\/MIDNIGHT\" }@.
    -- Or, you can add a custom theme by specifying
    -- @\"AccountCustomization\": { \"DefaultTheme\": \"arn:aws:quicksight:us-west-2:111122223333:theme\/bdb844d0-0fe9-4d9d-b520-0fe602d93639\" }@.
    accountCustomization :: AccountCustomization
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAccountCustomization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createAccountCustomization_tags' - A list of the tags that you want to attach to this resource.
--
-- 'namespace', 'createAccountCustomization_namespace' - The Amazon QuickSight namespace that you want to add customizations to.
--
-- 'awsAccountId', 'createAccountCustomization_awsAccountId' - The ID for the Amazon Web Services account that you want to customize
-- Amazon QuickSight for.
--
-- 'accountCustomization', 'createAccountCustomization_accountCustomization' - The Amazon QuickSight customizations you\'re adding in the current
-- Amazon Web Services Region. You can add these to an Amazon Web Services
-- account and a QuickSight namespace.
--
-- For example, you can add a default theme by setting
-- @AccountCustomization@ to the midnight theme:
-- @\"AccountCustomization\": { \"DefaultTheme\": \"arn:aws:quicksight::aws:theme\/MIDNIGHT\" }@.
-- Or, you can add a custom theme by specifying
-- @\"AccountCustomization\": { \"DefaultTheme\": \"arn:aws:quicksight:us-west-2:111122223333:theme\/bdb844d0-0fe9-4d9d-b520-0fe602d93639\" }@.
newCreateAccountCustomization ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'accountCustomization'
  AccountCustomization ->
  CreateAccountCustomization
newCreateAccountCustomization
  pAwsAccountId_
  pAccountCustomization_ =
    CreateAccountCustomization'
      { tags = Prelude.Nothing,
        namespace = Prelude.Nothing,
        awsAccountId = pAwsAccountId_,
        accountCustomization = pAccountCustomization_
      }

-- | A list of the tags that you want to attach to this resource.
createAccountCustomization_tags :: Lens.Lens' CreateAccountCustomization (Prelude.Maybe (Prelude.NonEmpty Tag))
createAccountCustomization_tags = Lens.lens (\CreateAccountCustomization' {tags} -> tags) (\s@CreateAccountCustomization' {} a -> s {tags = a} :: CreateAccountCustomization) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon QuickSight namespace that you want to add customizations to.
createAccountCustomization_namespace :: Lens.Lens' CreateAccountCustomization (Prelude.Maybe Prelude.Text)
createAccountCustomization_namespace = Lens.lens (\CreateAccountCustomization' {namespace} -> namespace) (\s@CreateAccountCustomization' {} a -> s {namespace = a} :: CreateAccountCustomization)

-- | The ID for the Amazon Web Services account that you want to customize
-- Amazon QuickSight for.
createAccountCustomization_awsAccountId :: Lens.Lens' CreateAccountCustomization Prelude.Text
createAccountCustomization_awsAccountId = Lens.lens (\CreateAccountCustomization' {awsAccountId} -> awsAccountId) (\s@CreateAccountCustomization' {} a -> s {awsAccountId = a} :: CreateAccountCustomization)

-- | The Amazon QuickSight customizations you\'re adding in the current
-- Amazon Web Services Region. You can add these to an Amazon Web Services
-- account and a QuickSight namespace.
--
-- For example, you can add a default theme by setting
-- @AccountCustomization@ to the midnight theme:
-- @\"AccountCustomization\": { \"DefaultTheme\": \"arn:aws:quicksight::aws:theme\/MIDNIGHT\" }@.
-- Or, you can add a custom theme by specifying
-- @\"AccountCustomization\": { \"DefaultTheme\": \"arn:aws:quicksight:us-west-2:111122223333:theme\/bdb844d0-0fe9-4d9d-b520-0fe602d93639\" }@.
createAccountCustomization_accountCustomization :: Lens.Lens' CreateAccountCustomization AccountCustomization
createAccountCustomization_accountCustomization = Lens.lens (\CreateAccountCustomization' {accountCustomization} -> accountCustomization) (\s@CreateAccountCustomization' {} a -> s {accountCustomization = a} :: CreateAccountCustomization)

instance Core.AWSRequest CreateAccountCustomization where
  type
    AWSResponse CreateAccountCustomization =
      CreateAccountCustomizationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAccountCustomizationResponse'
            Prelude.<$> (x Core..?> "AwsAccountId")
            Prelude.<*> (x Core..?> "RequestId")
            Prelude.<*> (x Core..?> "Arn")
            Prelude.<*> (x Core..?> "Namespace")
            Prelude.<*> (x Core..?> "AccountCustomization")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateAccountCustomization where
  hashWithSalt _salt CreateAccountCustomization' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` namespace
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` accountCustomization

instance Prelude.NFData CreateAccountCustomization where
  rnf CreateAccountCustomization' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf namespace
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf accountCustomization

instance Core.ToHeaders CreateAccountCustomization where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateAccountCustomization where
  toJSON CreateAccountCustomization' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Tags" Core..=) Prelude.<$> tags,
            Prelude.Just
              ( "AccountCustomization"
                  Core..= accountCustomization
              )
          ]
      )

instance Core.ToPath CreateAccountCustomization where
  toPath CreateAccountCustomization' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Core.toBS awsAccountId,
        "/customizations"
      ]

instance Core.ToQuery CreateAccountCustomization where
  toQuery CreateAccountCustomization' {..} =
    Prelude.mconcat ["namespace" Core.=: namespace]

-- | /See:/ 'newCreateAccountCustomizationResponse' smart constructor.
data CreateAccountCustomizationResponse = CreateAccountCustomizationResponse'
  { -- | The ID for the Amazon Web Services account that you want to customize
    -- Amazon QuickSight for.
    awsAccountId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the customization that you created
    -- for this Amazon Web Services account.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The namespace associated with the customization you\'re creating.
    namespace :: Prelude.Maybe Prelude.Text,
    -- | The Amazon QuickSight customizations you\'re adding in the current
    -- Amazon Web Services Region.
    accountCustomization :: Prelude.Maybe AccountCustomization,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAccountCustomizationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsAccountId', 'createAccountCustomizationResponse_awsAccountId' - The ID for the Amazon Web Services account that you want to customize
-- Amazon QuickSight for.
--
-- 'requestId', 'createAccountCustomizationResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'arn', 'createAccountCustomizationResponse_arn' - The Amazon Resource Name (ARN) for the customization that you created
-- for this Amazon Web Services account.
--
-- 'namespace', 'createAccountCustomizationResponse_namespace' - The namespace associated with the customization you\'re creating.
--
-- 'accountCustomization', 'createAccountCustomizationResponse_accountCustomization' - The Amazon QuickSight customizations you\'re adding in the current
-- Amazon Web Services Region.
--
-- 'status', 'createAccountCustomizationResponse_status' - The HTTP status of the request.
newCreateAccountCustomizationResponse ::
  -- | 'status'
  Prelude.Int ->
  CreateAccountCustomizationResponse
newCreateAccountCustomizationResponse pStatus_ =
  CreateAccountCustomizationResponse'
    { awsAccountId =
        Prelude.Nothing,
      requestId = Prelude.Nothing,
      arn = Prelude.Nothing,
      namespace = Prelude.Nothing,
      accountCustomization = Prelude.Nothing,
      status = pStatus_
    }

-- | The ID for the Amazon Web Services account that you want to customize
-- Amazon QuickSight for.
createAccountCustomizationResponse_awsAccountId :: Lens.Lens' CreateAccountCustomizationResponse (Prelude.Maybe Prelude.Text)
createAccountCustomizationResponse_awsAccountId = Lens.lens (\CreateAccountCustomizationResponse' {awsAccountId} -> awsAccountId) (\s@CreateAccountCustomizationResponse' {} a -> s {awsAccountId = a} :: CreateAccountCustomizationResponse)

-- | The Amazon Web Services request ID for this operation.
createAccountCustomizationResponse_requestId :: Lens.Lens' CreateAccountCustomizationResponse (Prelude.Maybe Prelude.Text)
createAccountCustomizationResponse_requestId = Lens.lens (\CreateAccountCustomizationResponse' {requestId} -> requestId) (\s@CreateAccountCustomizationResponse' {} a -> s {requestId = a} :: CreateAccountCustomizationResponse)

-- | The Amazon Resource Name (ARN) for the customization that you created
-- for this Amazon Web Services account.
createAccountCustomizationResponse_arn :: Lens.Lens' CreateAccountCustomizationResponse (Prelude.Maybe Prelude.Text)
createAccountCustomizationResponse_arn = Lens.lens (\CreateAccountCustomizationResponse' {arn} -> arn) (\s@CreateAccountCustomizationResponse' {} a -> s {arn = a} :: CreateAccountCustomizationResponse)

-- | The namespace associated with the customization you\'re creating.
createAccountCustomizationResponse_namespace :: Lens.Lens' CreateAccountCustomizationResponse (Prelude.Maybe Prelude.Text)
createAccountCustomizationResponse_namespace = Lens.lens (\CreateAccountCustomizationResponse' {namespace} -> namespace) (\s@CreateAccountCustomizationResponse' {} a -> s {namespace = a} :: CreateAccountCustomizationResponse)

-- | The Amazon QuickSight customizations you\'re adding in the current
-- Amazon Web Services Region.
createAccountCustomizationResponse_accountCustomization :: Lens.Lens' CreateAccountCustomizationResponse (Prelude.Maybe AccountCustomization)
createAccountCustomizationResponse_accountCustomization = Lens.lens (\CreateAccountCustomizationResponse' {accountCustomization} -> accountCustomization) (\s@CreateAccountCustomizationResponse' {} a -> s {accountCustomization = a} :: CreateAccountCustomizationResponse)

-- | The HTTP status of the request.
createAccountCustomizationResponse_status :: Lens.Lens' CreateAccountCustomizationResponse Prelude.Int
createAccountCustomizationResponse_status = Lens.lens (\CreateAccountCustomizationResponse' {status} -> status) (\s@CreateAccountCustomizationResponse' {} a -> s {status = a} :: CreateAccountCustomizationResponse)

instance
  Prelude.NFData
    CreateAccountCustomizationResponse
  where
  rnf CreateAccountCustomizationResponse' {..} =
    Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf namespace
      `Prelude.seq` Prelude.rnf accountCustomization
      `Prelude.seq` Prelude.rnf status
