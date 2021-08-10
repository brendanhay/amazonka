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
-- Module      : Network.AWS.MediaStore.DescribeContainer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the properties of the requested container. This request is
-- commonly used to retrieve the endpoint of a container. An endpoint is a
-- value assigned by the service when a new container is created. A
-- container\'s endpoint does not change after it has been assigned. The
-- @DescribeContainer@ request returns a single @Container@ object based on
-- @ContainerName@. To return all @Container@ objects that are associated
-- with a specified AWS account, use ListContainers.
module Network.AWS.MediaStore.DescribeContainer
  ( -- * Creating a Request
    DescribeContainer (..),
    newDescribeContainer,

    -- * Request Lenses
    describeContainer_containerName,

    -- * Destructuring the Response
    DescribeContainerResponse (..),
    newDescribeContainerResponse,

    -- * Response Lenses
    describeContainerResponse_container,
    describeContainerResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaStore.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeContainer' smart constructor.
data DescribeContainer = DescribeContainer'
  { -- | The name of the container to query.
    containerName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeContainer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containerName', 'describeContainer_containerName' - The name of the container to query.
newDescribeContainer ::
  DescribeContainer
newDescribeContainer =
  DescribeContainer' {containerName = Prelude.Nothing}

-- | The name of the container to query.
describeContainer_containerName :: Lens.Lens' DescribeContainer (Prelude.Maybe Prelude.Text)
describeContainer_containerName = Lens.lens (\DescribeContainer' {containerName} -> containerName) (\s@DescribeContainer' {} a -> s {containerName = a} :: DescribeContainer)

instance Core.AWSRequest DescribeContainer where
  type
    AWSResponse DescribeContainer =
      DescribeContainerResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeContainerResponse'
            Prelude.<$> (x Core..?> "Container")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeContainer

instance Prelude.NFData DescribeContainer

instance Core.ToHeaders DescribeContainer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "MediaStore_20170901.DescribeContainer" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeContainer where
  toJSON DescribeContainer' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ContainerName" Core..=)
              Prelude.<$> containerName
          ]
      )

instance Core.ToPath DescribeContainer where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeContainer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeContainerResponse' smart constructor.
data DescribeContainerResponse = DescribeContainerResponse'
  { -- | The name of the queried container.
    container :: Prelude.Maybe Container,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeContainerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'container', 'describeContainerResponse_container' - The name of the queried container.
--
-- 'httpStatus', 'describeContainerResponse_httpStatus' - The response's http status code.
newDescribeContainerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeContainerResponse
newDescribeContainerResponse pHttpStatus_ =
  DescribeContainerResponse'
    { container =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the queried container.
describeContainerResponse_container :: Lens.Lens' DescribeContainerResponse (Prelude.Maybe Container)
describeContainerResponse_container = Lens.lens (\DescribeContainerResponse' {container} -> container) (\s@DescribeContainerResponse' {} a -> s {container = a} :: DescribeContainerResponse)

-- | The response's http status code.
describeContainerResponse_httpStatus :: Lens.Lens' DescribeContainerResponse Prelude.Int
describeContainerResponse_httpStatus = Lens.lens (\DescribeContainerResponse' {httpStatus} -> httpStatus) (\s@DescribeContainerResponse' {} a -> s {httpStatus = a} :: DescribeContainerResponse)

instance Prelude.NFData DescribeContainerResponse
