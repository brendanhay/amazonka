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
-- Module      : Amazonka.QuickSight.DescribeNamespace
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the current namespace.
module Amazonka.QuickSight.DescribeNamespace
  ( -- * Creating a Request
    DescribeNamespace (..),
    newDescribeNamespace,

    -- * Request Lenses
    describeNamespace_awsAccountId,
    describeNamespace_namespace,

    -- * Destructuring the Response
    DescribeNamespaceResponse (..),
    newDescribeNamespaceResponse,

    -- * Response Lenses
    describeNamespaceResponse_requestId,
    describeNamespaceResponse_namespace,
    describeNamespaceResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeNamespace' smart constructor.
data DescribeNamespace = DescribeNamespace'
  { -- | The ID for the Amazon Web Services account that contains the Amazon
    -- QuickSight namespace that you want to describe.
    awsAccountId :: Prelude.Text,
    -- | The namespace that you want to describe.
    namespace :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeNamespace' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsAccountId', 'describeNamespace_awsAccountId' - The ID for the Amazon Web Services account that contains the Amazon
-- QuickSight namespace that you want to describe.
--
-- 'namespace', 'describeNamespace_namespace' - The namespace that you want to describe.
newDescribeNamespace ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'namespace'
  Prelude.Text ->
  DescribeNamespace
newDescribeNamespace pAwsAccountId_ pNamespace_ =
  DescribeNamespace'
    { awsAccountId = pAwsAccountId_,
      namespace = pNamespace_
    }

-- | The ID for the Amazon Web Services account that contains the Amazon
-- QuickSight namespace that you want to describe.
describeNamespace_awsAccountId :: Lens.Lens' DescribeNamespace Prelude.Text
describeNamespace_awsAccountId = Lens.lens (\DescribeNamespace' {awsAccountId} -> awsAccountId) (\s@DescribeNamespace' {} a -> s {awsAccountId = a} :: DescribeNamespace)

-- | The namespace that you want to describe.
describeNamespace_namespace :: Lens.Lens' DescribeNamespace Prelude.Text
describeNamespace_namespace = Lens.lens (\DescribeNamespace' {namespace} -> namespace) (\s@DescribeNamespace' {} a -> s {namespace = a} :: DescribeNamespace)

instance Core.AWSRequest DescribeNamespace where
  type
    AWSResponse DescribeNamespace =
      DescribeNamespaceResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeNamespaceResponse'
            Prelude.<$> (x Core..?> "RequestId")
            Prelude.<*> (x Core..?> "Namespace")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeNamespace where
  hashWithSalt _salt DescribeNamespace' {..} =
    _salt `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` namespace

instance Prelude.NFData DescribeNamespace where
  rnf DescribeNamespace' {..} =
    Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf namespace

instance Core.ToHeaders DescribeNamespace where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DescribeNamespace where
  toPath DescribeNamespace' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Core.toBS awsAccountId,
        "/namespaces/",
        Core.toBS namespace
      ]

instance Core.ToQuery DescribeNamespace where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeNamespaceResponse' smart constructor.
data DescribeNamespaceResponse = DescribeNamespaceResponse'
  { -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The information about the namespace that you\'re describing. The
    -- response includes the namespace ARN, name, Amazon Web Services Region,
    -- creation status, and identity store. @DescribeNamespace@ also works for
    -- namespaces that are in the process of being created. For incomplete
    -- namespaces, this API operation lists the namespace error types and
    -- messages associated with the creation process.
    namespace :: Prelude.Maybe NamespaceInfoV2,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeNamespaceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestId', 'describeNamespaceResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'namespace', 'describeNamespaceResponse_namespace' - The information about the namespace that you\'re describing. The
-- response includes the namespace ARN, name, Amazon Web Services Region,
-- creation status, and identity store. @DescribeNamespace@ also works for
-- namespaces that are in the process of being created. For incomplete
-- namespaces, this API operation lists the namespace error types and
-- messages associated with the creation process.
--
-- 'status', 'describeNamespaceResponse_status' - The HTTP status of the request.
newDescribeNamespaceResponse ::
  -- | 'status'
  Prelude.Int ->
  DescribeNamespaceResponse
newDescribeNamespaceResponse pStatus_ =
  DescribeNamespaceResponse'
    { requestId =
        Prelude.Nothing,
      namespace = Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Web Services request ID for this operation.
describeNamespaceResponse_requestId :: Lens.Lens' DescribeNamespaceResponse (Prelude.Maybe Prelude.Text)
describeNamespaceResponse_requestId = Lens.lens (\DescribeNamespaceResponse' {requestId} -> requestId) (\s@DescribeNamespaceResponse' {} a -> s {requestId = a} :: DescribeNamespaceResponse)

-- | The information about the namespace that you\'re describing. The
-- response includes the namespace ARN, name, Amazon Web Services Region,
-- creation status, and identity store. @DescribeNamespace@ also works for
-- namespaces that are in the process of being created. For incomplete
-- namespaces, this API operation lists the namespace error types and
-- messages associated with the creation process.
describeNamespaceResponse_namespace :: Lens.Lens' DescribeNamespaceResponse (Prelude.Maybe NamespaceInfoV2)
describeNamespaceResponse_namespace = Lens.lens (\DescribeNamespaceResponse' {namespace} -> namespace) (\s@DescribeNamespaceResponse' {} a -> s {namespace = a} :: DescribeNamespaceResponse)

-- | The HTTP status of the request.
describeNamespaceResponse_status :: Lens.Lens' DescribeNamespaceResponse Prelude.Int
describeNamespaceResponse_status = Lens.lens (\DescribeNamespaceResponse' {status} -> status) (\s@DescribeNamespaceResponse' {} a -> s {status = a} :: DescribeNamespaceResponse)

instance Prelude.NFData DescribeNamespaceResponse where
  rnf DescribeNamespaceResponse' {..} =
    Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf namespace
      `Prelude.seq` Prelude.rnf status
