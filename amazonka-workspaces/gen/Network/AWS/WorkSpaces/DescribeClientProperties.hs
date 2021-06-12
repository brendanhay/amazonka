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
-- Module      : Network.AWS.WorkSpaces.DescribeClientProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list that describes one or more specified Amazon WorkSpaces
-- clients.
module Network.AWS.WorkSpaces.DescribeClientProperties
  ( -- * Creating a Request
    DescribeClientProperties (..),
    newDescribeClientProperties,

    -- * Request Lenses
    describeClientProperties_resourceIds,

    -- * Destructuring the Response
    DescribeClientPropertiesResponse (..),
    newDescribeClientPropertiesResponse,

    -- * Response Lenses
    describeClientPropertiesResponse_clientPropertiesList,
    describeClientPropertiesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'newDescribeClientProperties' smart constructor.
data DescribeClientProperties = DescribeClientProperties'
  { -- | The resource identifier, in the form of directory IDs.
    resourceIds :: Core.NonEmpty Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeClientProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceIds', 'describeClientProperties_resourceIds' - The resource identifier, in the form of directory IDs.
newDescribeClientProperties ::
  -- | 'resourceIds'
  Core.NonEmpty Core.Text ->
  DescribeClientProperties
newDescribeClientProperties pResourceIds_ =
  DescribeClientProperties'
    { resourceIds =
        Lens._Coerce Lens.# pResourceIds_
    }

-- | The resource identifier, in the form of directory IDs.
describeClientProperties_resourceIds :: Lens.Lens' DescribeClientProperties (Core.NonEmpty Core.Text)
describeClientProperties_resourceIds = Lens.lens (\DescribeClientProperties' {resourceIds} -> resourceIds) (\s@DescribeClientProperties' {} a -> s {resourceIds = a} :: DescribeClientProperties) Core.. Lens._Coerce

instance Core.AWSRequest DescribeClientProperties where
  type
    AWSResponse DescribeClientProperties =
      DescribeClientPropertiesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeClientPropertiesResponse'
            Core.<$> ( x Core..?> "ClientPropertiesList"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeClientProperties

instance Core.NFData DescribeClientProperties

instance Core.ToHeaders DescribeClientProperties where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "WorkspacesService.DescribeClientProperties" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeClientProperties where
  toJSON DescribeClientProperties' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("ResourceIds" Core..= resourceIds)]
      )

instance Core.ToPath DescribeClientProperties where
  toPath = Core.const "/"

instance Core.ToQuery DescribeClientProperties where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeClientPropertiesResponse' smart constructor.
data DescribeClientPropertiesResponse = DescribeClientPropertiesResponse'
  { -- | Information about the specified Amazon WorkSpaces clients.
    clientPropertiesList :: Core.Maybe [ClientPropertiesResult],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeClientPropertiesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientPropertiesList', 'describeClientPropertiesResponse_clientPropertiesList' - Information about the specified Amazon WorkSpaces clients.
--
-- 'httpStatus', 'describeClientPropertiesResponse_httpStatus' - The response's http status code.
newDescribeClientPropertiesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeClientPropertiesResponse
newDescribeClientPropertiesResponse pHttpStatus_ =
  DescribeClientPropertiesResponse'
    { clientPropertiesList =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the specified Amazon WorkSpaces clients.
describeClientPropertiesResponse_clientPropertiesList :: Lens.Lens' DescribeClientPropertiesResponse (Core.Maybe [ClientPropertiesResult])
describeClientPropertiesResponse_clientPropertiesList = Lens.lens (\DescribeClientPropertiesResponse' {clientPropertiesList} -> clientPropertiesList) (\s@DescribeClientPropertiesResponse' {} a -> s {clientPropertiesList = a} :: DescribeClientPropertiesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeClientPropertiesResponse_httpStatus :: Lens.Lens' DescribeClientPropertiesResponse Core.Int
describeClientPropertiesResponse_httpStatus = Lens.lens (\DescribeClientPropertiesResponse' {httpStatus} -> httpStatus) (\s@DescribeClientPropertiesResponse' {} a -> s {httpStatus = a} :: DescribeClientPropertiesResponse)

instance Core.NFData DescribeClientPropertiesResponse
