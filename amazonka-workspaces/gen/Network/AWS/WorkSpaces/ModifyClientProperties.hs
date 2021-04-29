{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.WorkSpaces.ModifyClientProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the properties of the specified Amazon WorkSpaces clients.
module Network.AWS.WorkSpaces.ModifyClientProperties
  ( -- * Creating a Request
    ModifyClientProperties (..),
    newModifyClientProperties,

    -- * Request Lenses
    modifyClientProperties_resourceId,
    modifyClientProperties_clientProperties,

    -- * Destructuring the Response
    ModifyClientPropertiesResponse (..),
    newModifyClientPropertiesResponse,

    -- * Response Lenses
    modifyClientPropertiesResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'newModifyClientProperties' smart constructor.
data ModifyClientProperties = ModifyClientProperties'
  { -- | The resource identifiers, in the form of directory IDs.
    resourceId :: Prelude.Text,
    -- | Information about the Amazon WorkSpaces client.
    clientProperties :: ClientProperties
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ModifyClientProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'modifyClientProperties_resourceId' - The resource identifiers, in the form of directory IDs.
--
-- 'clientProperties', 'modifyClientProperties_clientProperties' - Information about the Amazon WorkSpaces client.
newModifyClientProperties ::
  -- | 'resourceId'
  Prelude.Text ->
  -- | 'clientProperties'
  ClientProperties ->
  ModifyClientProperties
newModifyClientProperties
  pResourceId_
  pClientProperties_ =
    ModifyClientProperties'
      { resourceId = pResourceId_,
        clientProperties = pClientProperties_
      }

-- | The resource identifiers, in the form of directory IDs.
modifyClientProperties_resourceId :: Lens.Lens' ModifyClientProperties Prelude.Text
modifyClientProperties_resourceId = Lens.lens (\ModifyClientProperties' {resourceId} -> resourceId) (\s@ModifyClientProperties' {} a -> s {resourceId = a} :: ModifyClientProperties)

-- | Information about the Amazon WorkSpaces client.
modifyClientProperties_clientProperties :: Lens.Lens' ModifyClientProperties ClientProperties
modifyClientProperties_clientProperties = Lens.lens (\ModifyClientProperties' {clientProperties} -> clientProperties) (\s@ModifyClientProperties' {} a -> s {clientProperties = a} :: ModifyClientProperties)

instance Prelude.AWSRequest ModifyClientProperties where
  type
    Rs ModifyClientProperties =
      ModifyClientPropertiesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          ModifyClientPropertiesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyClientProperties

instance Prelude.NFData ModifyClientProperties

instance Prelude.ToHeaders ModifyClientProperties where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "WorkspacesService.ModifyClientProperties" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ModifyClientProperties where
  toJSON ModifyClientProperties' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ResourceId" Prelude..= resourceId),
            Prelude.Just
              ("ClientProperties" Prelude..= clientProperties)
          ]
      )

instance Prelude.ToPath ModifyClientProperties where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ModifyClientProperties where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newModifyClientPropertiesResponse' smart constructor.
data ModifyClientPropertiesResponse = ModifyClientPropertiesResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ModifyClientPropertiesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'modifyClientPropertiesResponse_httpStatus' - The response's http status code.
newModifyClientPropertiesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyClientPropertiesResponse
newModifyClientPropertiesResponse pHttpStatus_ =
  ModifyClientPropertiesResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
modifyClientPropertiesResponse_httpStatus :: Lens.Lens' ModifyClientPropertiesResponse Prelude.Int
modifyClientPropertiesResponse_httpStatus = Lens.lens (\ModifyClientPropertiesResponse' {httpStatus} -> httpStatus) (\s@ModifyClientPropertiesResponse' {} a -> s {httpStatus = a} :: ModifyClientPropertiesResponse)

instance
  Prelude.NFData
    ModifyClientPropertiesResponse
