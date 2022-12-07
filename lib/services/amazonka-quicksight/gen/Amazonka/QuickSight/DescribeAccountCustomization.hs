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
-- Module      : Amazonka.QuickSight.DescribeAccountCustomization
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the customizations associated with the provided Amazon Web
-- Services account and Amazon Amazon QuickSight namespace in an Amazon Web
-- Services Region. The Amazon QuickSight console evaluates which
-- customizations to apply by running this API operation with the
-- @Resolved@ flag included.
--
-- To determine what customizations display when you run this command, it
-- can help to visualize the relationship of the entities involved.
--
-- -   @Amazon Web Services account@ - The Amazon Web Services account
--     exists at the top of the hierarchy. It has the potential to use all
--     of the Amazon Web Services Regions and Amazon Web Services Services.
--     When you subscribe to Amazon QuickSight, you choose one Amazon Web
--     Services Region to use as your home Region. That\'s where your free
--     SPICE capacity is located. You can use Amazon QuickSight in any
--     supported Amazon Web Services Region.
--
-- -   @Amazon Web Services Region@ - In each Amazon Web Services Region
--     where you sign in to Amazon QuickSight at least once, Amazon
--     QuickSight acts as a separate instance of the same service. If you
--     have a user directory, it resides in us-east-1, which is the US East
--     (N. Virginia). Generally speaking, these users have access to Amazon
--     QuickSight in any Amazon Web Services Region, unless they are
--     constrained to a namespace.
--
--     To run the command in a different Amazon Web Services Region, you
--     change your Region settings. If you\'re using the CLI, you can use
--     one of the following options:
--
--     -   Use
--         <https://docs.aws.amazon.com/cli/latest/userguide/cli-configure-options.html command line options>.
--
--     -   Use
--         <https://docs.aws.amazon.com/cli/latest/userguide/cli-configure-profiles.html named profiles>.
--
--     -   Run @aws configure@ to change your default Amazon Web Services
--         Region. Use Enter to key the same settings for your keys. For
--         more information, see
--         <https://docs.aws.amazon.com/cli/latest/userguide/cli-chap-configure.html Configuring the CLI>.
--
-- -   @Namespace@ - A QuickSight namespace is a partition that contains
--     users and assets (data sources, datasets, dashboards, and so on). To
--     access assets that are in a specific namespace, users and groups
--     must also be part of the same namespace. People who share a
--     namespace are completely isolated from users and assets in other
--     namespaces, even if they are in the same Amazon Web Services account
--     and Amazon Web Services Region.
--
-- -   @Applied customizations@ - Within an Amazon Web Services Region, a
--     set of Amazon QuickSight customizations can apply to an Amazon Web
--     Services account or to a namespace. Settings that you apply to a
--     namespace override settings that you apply to an Amazon Web Services
--     account. All settings are isolated to a single Amazon Web Services
--     Region. To apply them in other Amazon Web Services Regions, run the
--     @CreateAccountCustomization@ command in each Amazon Web Services
--     Region where you want to apply the same customizations.
module Amazonka.QuickSight.DescribeAccountCustomization
  ( -- * Creating a Request
    DescribeAccountCustomization (..),
    newDescribeAccountCustomization,

    -- * Request Lenses
    describeAccountCustomization_resolved,
    describeAccountCustomization_namespace,
    describeAccountCustomization_awsAccountId,

    -- * Destructuring the Response
    DescribeAccountCustomizationResponse (..),
    newDescribeAccountCustomizationResponse,

    -- * Response Lenses
    describeAccountCustomizationResponse_awsAccountId,
    describeAccountCustomizationResponse_requestId,
    describeAccountCustomizationResponse_arn,
    describeAccountCustomizationResponse_namespace,
    describeAccountCustomizationResponse_accountCustomization,
    describeAccountCustomizationResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeAccountCustomization' smart constructor.
data DescribeAccountCustomization = DescribeAccountCustomization'
  { -- | The @Resolved@ flag works with the other parameters to determine which
    -- view of Amazon QuickSight customizations is returned. You can add this
    -- flag to your command to use the same view that Amazon QuickSight uses to
    -- identify which customizations to apply to the console. Omit this flag,
    -- or set it to @no-resolved@, to reveal customizations that are configured
    -- at different levels.
    resolved :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon QuickSight namespace that you want to describe Amazon
    -- QuickSight customizations for.
    namespace :: Prelude.Maybe Prelude.Text,
    -- | The ID for the Amazon Web Services account that you want to describe
    -- Amazon QuickSight customizations for.
    awsAccountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAccountCustomization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resolved', 'describeAccountCustomization_resolved' - The @Resolved@ flag works with the other parameters to determine which
-- view of Amazon QuickSight customizations is returned. You can add this
-- flag to your command to use the same view that Amazon QuickSight uses to
-- identify which customizations to apply to the console. Omit this flag,
-- or set it to @no-resolved@, to reveal customizations that are configured
-- at different levels.
--
-- 'namespace', 'describeAccountCustomization_namespace' - The Amazon QuickSight namespace that you want to describe Amazon
-- QuickSight customizations for.
--
-- 'awsAccountId', 'describeAccountCustomization_awsAccountId' - The ID for the Amazon Web Services account that you want to describe
-- Amazon QuickSight customizations for.
newDescribeAccountCustomization ::
  -- | 'awsAccountId'
  Prelude.Text ->
  DescribeAccountCustomization
newDescribeAccountCustomization pAwsAccountId_ =
  DescribeAccountCustomization'
    { resolved =
        Prelude.Nothing,
      namespace = Prelude.Nothing,
      awsAccountId = pAwsAccountId_
    }

-- | The @Resolved@ flag works with the other parameters to determine which
-- view of Amazon QuickSight customizations is returned. You can add this
-- flag to your command to use the same view that Amazon QuickSight uses to
-- identify which customizations to apply to the console. Omit this flag,
-- or set it to @no-resolved@, to reveal customizations that are configured
-- at different levels.
describeAccountCustomization_resolved :: Lens.Lens' DescribeAccountCustomization (Prelude.Maybe Prelude.Bool)
describeAccountCustomization_resolved = Lens.lens (\DescribeAccountCustomization' {resolved} -> resolved) (\s@DescribeAccountCustomization' {} a -> s {resolved = a} :: DescribeAccountCustomization)

-- | The Amazon QuickSight namespace that you want to describe Amazon
-- QuickSight customizations for.
describeAccountCustomization_namespace :: Lens.Lens' DescribeAccountCustomization (Prelude.Maybe Prelude.Text)
describeAccountCustomization_namespace = Lens.lens (\DescribeAccountCustomization' {namespace} -> namespace) (\s@DescribeAccountCustomization' {} a -> s {namespace = a} :: DescribeAccountCustomization)

-- | The ID for the Amazon Web Services account that you want to describe
-- Amazon QuickSight customizations for.
describeAccountCustomization_awsAccountId :: Lens.Lens' DescribeAccountCustomization Prelude.Text
describeAccountCustomization_awsAccountId = Lens.lens (\DescribeAccountCustomization' {awsAccountId} -> awsAccountId) (\s@DescribeAccountCustomization' {} a -> s {awsAccountId = a} :: DescribeAccountCustomization)

instance Core.AWSRequest DescribeAccountCustomization where
  type
    AWSResponse DescribeAccountCustomization =
      DescribeAccountCustomizationResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAccountCustomizationResponse'
            Prelude.<$> (x Data..?> "AwsAccountId")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "Namespace")
            Prelude.<*> (x Data..?> "AccountCustomization")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeAccountCustomization
  where
  hashWithSalt _salt DescribeAccountCustomization' {..} =
    _salt `Prelude.hashWithSalt` resolved
      `Prelude.hashWithSalt` namespace
      `Prelude.hashWithSalt` awsAccountId

instance Prelude.NFData DescribeAccountCustomization where
  rnf DescribeAccountCustomization' {..} =
    Prelude.rnf resolved
      `Prelude.seq` Prelude.rnf namespace
      `Prelude.seq` Prelude.rnf awsAccountId

instance Data.ToHeaders DescribeAccountCustomization where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeAccountCustomization where
  toPath DescribeAccountCustomization' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/customizations"
      ]

instance Data.ToQuery DescribeAccountCustomization where
  toQuery DescribeAccountCustomization' {..} =
    Prelude.mconcat
      [ "resolved" Data.=: resolved,
        "namespace" Data.=: namespace
      ]

-- | /See:/ 'newDescribeAccountCustomizationResponse' smart constructor.
data DescribeAccountCustomizationResponse = DescribeAccountCustomizationResponse'
  { -- | The ID for the Amazon Web Services account that you\'re describing.
    awsAccountId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the customization that\'s associated
    -- with this Amazon Web Services account.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon QuickSight namespace that you\'re describing.
    namespace :: Prelude.Maybe Prelude.Text,
    -- | The Amazon QuickSight customizations that exist in the current Amazon
    -- Web Services Region.
    accountCustomization :: Prelude.Maybe AccountCustomization,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAccountCustomizationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsAccountId', 'describeAccountCustomizationResponse_awsAccountId' - The ID for the Amazon Web Services account that you\'re describing.
--
-- 'requestId', 'describeAccountCustomizationResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'arn', 'describeAccountCustomizationResponse_arn' - The Amazon Resource Name (ARN) of the customization that\'s associated
-- with this Amazon Web Services account.
--
-- 'namespace', 'describeAccountCustomizationResponse_namespace' - The Amazon QuickSight namespace that you\'re describing.
--
-- 'accountCustomization', 'describeAccountCustomizationResponse_accountCustomization' - The Amazon QuickSight customizations that exist in the current Amazon
-- Web Services Region.
--
-- 'status', 'describeAccountCustomizationResponse_status' - The HTTP status of the request.
newDescribeAccountCustomizationResponse ::
  -- | 'status'
  Prelude.Int ->
  DescribeAccountCustomizationResponse
newDescribeAccountCustomizationResponse pStatus_ =
  DescribeAccountCustomizationResponse'
    { awsAccountId =
        Prelude.Nothing,
      requestId = Prelude.Nothing,
      arn = Prelude.Nothing,
      namespace = Prelude.Nothing,
      accountCustomization =
        Prelude.Nothing,
      status = pStatus_
    }

-- | The ID for the Amazon Web Services account that you\'re describing.
describeAccountCustomizationResponse_awsAccountId :: Lens.Lens' DescribeAccountCustomizationResponse (Prelude.Maybe Prelude.Text)
describeAccountCustomizationResponse_awsAccountId = Lens.lens (\DescribeAccountCustomizationResponse' {awsAccountId} -> awsAccountId) (\s@DescribeAccountCustomizationResponse' {} a -> s {awsAccountId = a} :: DescribeAccountCustomizationResponse)

-- | The Amazon Web Services request ID for this operation.
describeAccountCustomizationResponse_requestId :: Lens.Lens' DescribeAccountCustomizationResponse (Prelude.Maybe Prelude.Text)
describeAccountCustomizationResponse_requestId = Lens.lens (\DescribeAccountCustomizationResponse' {requestId} -> requestId) (\s@DescribeAccountCustomizationResponse' {} a -> s {requestId = a} :: DescribeAccountCustomizationResponse)

-- | The Amazon Resource Name (ARN) of the customization that\'s associated
-- with this Amazon Web Services account.
describeAccountCustomizationResponse_arn :: Lens.Lens' DescribeAccountCustomizationResponse (Prelude.Maybe Prelude.Text)
describeAccountCustomizationResponse_arn = Lens.lens (\DescribeAccountCustomizationResponse' {arn} -> arn) (\s@DescribeAccountCustomizationResponse' {} a -> s {arn = a} :: DescribeAccountCustomizationResponse)

-- | The Amazon QuickSight namespace that you\'re describing.
describeAccountCustomizationResponse_namespace :: Lens.Lens' DescribeAccountCustomizationResponse (Prelude.Maybe Prelude.Text)
describeAccountCustomizationResponse_namespace = Lens.lens (\DescribeAccountCustomizationResponse' {namespace} -> namespace) (\s@DescribeAccountCustomizationResponse' {} a -> s {namespace = a} :: DescribeAccountCustomizationResponse)

-- | The Amazon QuickSight customizations that exist in the current Amazon
-- Web Services Region.
describeAccountCustomizationResponse_accountCustomization :: Lens.Lens' DescribeAccountCustomizationResponse (Prelude.Maybe AccountCustomization)
describeAccountCustomizationResponse_accountCustomization = Lens.lens (\DescribeAccountCustomizationResponse' {accountCustomization} -> accountCustomization) (\s@DescribeAccountCustomizationResponse' {} a -> s {accountCustomization = a} :: DescribeAccountCustomizationResponse)

-- | The HTTP status of the request.
describeAccountCustomizationResponse_status :: Lens.Lens' DescribeAccountCustomizationResponse Prelude.Int
describeAccountCustomizationResponse_status = Lens.lens (\DescribeAccountCustomizationResponse' {status} -> status) (\s@DescribeAccountCustomizationResponse' {} a -> s {status = a} :: DescribeAccountCustomizationResponse)

instance
  Prelude.NFData
    DescribeAccountCustomizationResponse
  where
  rnf DescribeAccountCustomizationResponse' {..} =
    Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf namespace
      `Prelude.seq` Prelude.rnf accountCustomization
      `Prelude.seq` Prelude.rnf status
