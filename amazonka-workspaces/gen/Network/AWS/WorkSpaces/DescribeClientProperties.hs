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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'newDescribeClientProperties' smart constructor.
data DescribeClientProperties = DescribeClientProperties'
  { -- | The resource identifier, in the form of directory IDs.
    resourceIds :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.NonEmpty Prelude.Text ->
  DescribeClientProperties
newDescribeClientProperties pResourceIds_ =
  DescribeClientProperties'
    { resourceIds =
        Lens._Coerce Lens.# pResourceIds_
    }

-- | The resource identifier, in the form of directory IDs.
describeClientProperties_resourceIds :: Lens.Lens' DescribeClientProperties (Prelude.NonEmpty Prelude.Text)
describeClientProperties_resourceIds = Lens.lens (\DescribeClientProperties' {resourceIds} -> resourceIds) (\s@DescribeClientProperties' {} a -> s {resourceIds = a} :: DescribeClientProperties) Prelude.. Lens._Coerce

instance Core.AWSRequest DescribeClientProperties where
  type
    AWSResponse DescribeClientProperties =
      DescribeClientPropertiesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeClientPropertiesResponse'
            Prelude.<$> ( x Core..?> "ClientPropertiesList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeClientProperties

instance Prelude.NFData DescribeClientProperties

instance Core.ToHeaders DescribeClientProperties where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "WorkspacesService.DescribeClientProperties" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeClientProperties where
  toJSON DescribeClientProperties' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("ResourceIds" Core..= resourceIds)]
      )

instance Core.ToPath DescribeClientProperties where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeClientProperties where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeClientPropertiesResponse' smart constructor.
data DescribeClientPropertiesResponse = DescribeClientPropertiesResponse'
  { -- | Information about the specified Amazon WorkSpaces clients.
    clientPropertiesList :: Prelude.Maybe [ClientPropertiesResult],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeClientPropertiesResponse
newDescribeClientPropertiesResponse pHttpStatus_ =
  DescribeClientPropertiesResponse'
    { clientPropertiesList =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the specified Amazon WorkSpaces clients.
describeClientPropertiesResponse_clientPropertiesList :: Lens.Lens' DescribeClientPropertiesResponse (Prelude.Maybe [ClientPropertiesResult])
describeClientPropertiesResponse_clientPropertiesList = Lens.lens (\DescribeClientPropertiesResponse' {clientPropertiesList} -> clientPropertiesList) (\s@DescribeClientPropertiesResponse' {} a -> s {clientPropertiesList = a} :: DescribeClientPropertiesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeClientPropertiesResponse_httpStatus :: Lens.Lens' DescribeClientPropertiesResponse Prelude.Int
describeClientPropertiesResponse_httpStatus = Lens.lens (\DescribeClientPropertiesResponse' {httpStatus} -> httpStatus) (\s@DescribeClientPropertiesResponse' {} a -> s {httpStatus = a} :: DescribeClientPropertiesResponse)

instance
  Prelude.NFData
    DescribeClientPropertiesResponse
