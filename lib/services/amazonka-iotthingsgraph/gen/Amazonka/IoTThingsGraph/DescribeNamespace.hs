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
-- Module      : Amazonka.IoTThingsGraph.DescribeNamespace
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the latest version of the user\'s namespace and the public version
-- that it is tracking.
module Amazonka.IoTThingsGraph.DescribeNamespace
  ( -- * Creating a Request
    DescribeNamespace (..),
    newDescribeNamespace,

    -- * Request Lenses
    describeNamespace_namespaceName,

    -- * Destructuring the Response
    DescribeNamespaceResponse (..),
    newDescribeNamespaceResponse,

    -- * Response Lenses
    describeNamespaceResponse_namespaceArn,
    describeNamespaceResponse_trackingNamespaceVersion,
    describeNamespaceResponse_namespaceVersion,
    describeNamespaceResponse_namespaceName,
    describeNamespaceResponse_trackingNamespaceName,
    describeNamespaceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.IoTThingsGraph.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeNamespace' smart constructor.
data DescribeNamespace = DescribeNamespace'
  { -- | The name of the user\'s namespace. Set this to @aws@ to get the public
    -- namespace.
    namespaceName :: Prelude.Maybe Prelude.Text
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
-- 'namespaceName', 'describeNamespace_namespaceName' - The name of the user\'s namespace. Set this to @aws@ to get the public
-- namespace.
newDescribeNamespace ::
  DescribeNamespace
newDescribeNamespace =
  DescribeNamespace' {namespaceName = Prelude.Nothing}

-- | The name of the user\'s namespace. Set this to @aws@ to get the public
-- namespace.
describeNamespace_namespaceName :: Lens.Lens' DescribeNamespace (Prelude.Maybe Prelude.Text)
describeNamespace_namespaceName = Lens.lens (\DescribeNamespace' {namespaceName} -> namespaceName) (\s@DescribeNamespace' {} a -> s {namespaceName = a} :: DescribeNamespace)

instance Core.AWSRequest DescribeNamespace where
  type
    AWSResponse DescribeNamespace =
      DescribeNamespaceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeNamespaceResponse'
            Prelude.<$> (x Core..?> "namespaceArn")
            Prelude.<*> (x Core..?> "trackingNamespaceVersion")
            Prelude.<*> (x Core..?> "namespaceVersion")
            Prelude.<*> (x Core..?> "namespaceName")
            Prelude.<*> (x Core..?> "trackingNamespaceName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeNamespace where
  hashWithSalt salt' DescribeNamespace' {..} =
    salt' `Prelude.hashWithSalt` namespaceName

instance Prelude.NFData DescribeNamespace where
  rnf DescribeNamespace' {..} =
    Prelude.rnf namespaceName

instance Core.ToHeaders DescribeNamespace where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "IotThingsGraphFrontEndService.DescribeNamespace" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeNamespace where
  toJSON DescribeNamespace' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("namespaceName" Core..=)
              Prelude.<$> namespaceName
          ]
      )

instance Core.ToPath DescribeNamespace where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeNamespace where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeNamespaceResponse' smart constructor.
data DescribeNamespaceResponse = DescribeNamespaceResponse'
  { -- | The ARN of the namespace.
    namespaceArn :: Prelude.Maybe Prelude.Text,
    -- | The version of the public namespace that the latest version is tracking.
    trackingNamespaceVersion :: Prelude.Maybe Prelude.Integer,
    -- | The version of the user\'s namespace to describe.
    namespaceVersion :: Prelude.Maybe Prelude.Integer,
    -- | The name of the namespace.
    namespaceName :: Prelude.Maybe Prelude.Text,
    -- | The name of the public namespace that the latest namespace version is
    -- tracking.
    trackingNamespaceName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
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
-- 'namespaceArn', 'describeNamespaceResponse_namespaceArn' - The ARN of the namespace.
--
-- 'trackingNamespaceVersion', 'describeNamespaceResponse_trackingNamespaceVersion' - The version of the public namespace that the latest version is tracking.
--
-- 'namespaceVersion', 'describeNamespaceResponse_namespaceVersion' - The version of the user\'s namespace to describe.
--
-- 'namespaceName', 'describeNamespaceResponse_namespaceName' - The name of the namespace.
--
-- 'trackingNamespaceName', 'describeNamespaceResponse_trackingNamespaceName' - The name of the public namespace that the latest namespace version is
-- tracking.
--
-- 'httpStatus', 'describeNamespaceResponse_httpStatus' - The response's http status code.
newDescribeNamespaceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeNamespaceResponse
newDescribeNamespaceResponse pHttpStatus_ =
  DescribeNamespaceResponse'
    { namespaceArn =
        Prelude.Nothing,
      trackingNamespaceVersion = Prelude.Nothing,
      namespaceVersion = Prelude.Nothing,
      namespaceName = Prelude.Nothing,
      trackingNamespaceName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the namespace.
describeNamespaceResponse_namespaceArn :: Lens.Lens' DescribeNamespaceResponse (Prelude.Maybe Prelude.Text)
describeNamespaceResponse_namespaceArn = Lens.lens (\DescribeNamespaceResponse' {namespaceArn} -> namespaceArn) (\s@DescribeNamespaceResponse' {} a -> s {namespaceArn = a} :: DescribeNamespaceResponse)

-- | The version of the public namespace that the latest version is tracking.
describeNamespaceResponse_trackingNamespaceVersion :: Lens.Lens' DescribeNamespaceResponse (Prelude.Maybe Prelude.Integer)
describeNamespaceResponse_trackingNamespaceVersion = Lens.lens (\DescribeNamespaceResponse' {trackingNamespaceVersion} -> trackingNamespaceVersion) (\s@DescribeNamespaceResponse' {} a -> s {trackingNamespaceVersion = a} :: DescribeNamespaceResponse)

-- | The version of the user\'s namespace to describe.
describeNamespaceResponse_namespaceVersion :: Lens.Lens' DescribeNamespaceResponse (Prelude.Maybe Prelude.Integer)
describeNamespaceResponse_namespaceVersion = Lens.lens (\DescribeNamespaceResponse' {namespaceVersion} -> namespaceVersion) (\s@DescribeNamespaceResponse' {} a -> s {namespaceVersion = a} :: DescribeNamespaceResponse)

-- | The name of the namespace.
describeNamespaceResponse_namespaceName :: Lens.Lens' DescribeNamespaceResponse (Prelude.Maybe Prelude.Text)
describeNamespaceResponse_namespaceName = Lens.lens (\DescribeNamespaceResponse' {namespaceName} -> namespaceName) (\s@DescribeNamespaceResponse' {} a -> s {namespaceName = a} :: DescribeNamespaceResponse)

-- | The name of the public namespace that the latest namespace version is
-- tracking.
describeNamespaceResponse_trackingNamespaceName :: Lens.Lens' DescribeNamespaceResponse (Prelude.Maybe Prelude.Text)
describeNamespaceResponse_trackingNamespaceName = Lens.lens (\DescribeNamespaceResponse' {trackingNamespaceName} -> trackingNamespaceName) (\s@DescribeNamespaceResponse' {} a -> s {trackingNamespaceName = a} :: DescribeNamespaceResponse)

-- | The response's http status code.
describeNamespaceResponse_httpStatus :: Lens.Lens' DescribeNamespaceResponse Prelude.Int
describeNamespaceResponse_httpStatus = Lens.lens (\DescribeNamespaceResponse' {httpStatus} -> httpStatus) (\s@DescribeNamespaceResponse' {} a -> s {httpStatus = a} :: DescribeNamespaceResponse)

instance Prelude.NFData DescribeNamespaceResponse where
  rnf DescribeNamespaceResponse' {..} =
    Prelude.rnf namespaceArn
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf trackingNamespaceName
      `Prelude.seq` Prelude.rnf namespaceName
      `Prelude.seq` Prelude.rnf namespaceVersion
      `Prelude.seq` Prelude.rnf trackingNamespaceVersion
