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
-- Module      : Network.AWS.DirectoryService.AddRegion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds two domain controllers in the specified Region for the specified
-- directory.
module Network.AWS.DirectoryService.AddRegion
  ( -- * Creating a Request
    AddRegion (..),
    newAddRegion,

    -- * Request Lenses
    addRegion_directoryId,
    addRegion_regionName,
    addRegion_vPCSettings,

    -- * Destructuring the Response
    AddRegionResponse (..),
    newAddRegionResponse,

    -- * Response Lenses
    addRegionResponse_httpStatus,
  )
where

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAddRegion' smart constructor.
data AddRegion = AddRegion'
  { -- | The identifier of the directory to which you want to add Region
    -- replication.
    directoryId :: Prelude.Text,
    -- | The name of the Region where you want to add domain controllers for
    -- replication. For example, @us-east-1@.
    regionName :: Prelude.Text,
    vPCSettings :: DirectoryVpcSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AddRegion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryId', 'addRegion_directoryId' - The identifier of the directory to which you want to add Region
-- replication.
--
-- 'regionName', 'addRegion_regionName' - The name of the Region where you want to add domain controllers for
-- replication. For example, @us-east-1@.
--
-- 'vPCSettings', 'addRegion_vPCSettings' - Undocumented member.
newAddRegion ::
  -- | 'directoryId'
  Prelude.Text ->
  -- | 'regionName'
  Prelude.Text ->
  -- | 'vPCSettings'
  DirectoryVpcSettings ->
  AddRegion
newAddRegion pDirectoryId_ pRegionName_ pVPCSettings_ =
  AddRegion'
    { directoryId = pDirectoryId_,
      regionName = pRegionName_,
      vPCSettings = pVPCSettings_
    }

-- | The identifier of the directory to which you want to add Region
-- replication.
addRegion_directoryId :: Lens.Lens' AddRegion Prelude.Text
addRegion_directoryId = Lens.lens (\AddRegion' {directoryId} -> directoryId) (\s@AddRegion' {} a -> s {directoryId = a} :: AddRegion)

-- | The name of the Region where you want to add domain controllers for
-- replication. For example, @us-east-1@.
addRegion_regionName :: Lens.Lens' AddRegion Prelude.Text
addRegion_regionName = Lens.lens (\AddRegion' {regionName} -> regionName) (\s@AddRegion' {} a -> s {regionName = a} :: AddRegion)

-- | Undocumented member.
addRegion_vPCSettings :: Lens.Lens' AddRegion DirectoryVpcSettings
addRegion_vPCSettings = Lens.lens (\AddRegion' {vPCSettings} -> vPCSettings) (\s@AddRegion' {} a -> s {vPCSettings = a} :: AddRegion)

instance Prelude.AWSRequest AddRegion where
  type Rs AddRegion = AddRegionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          AddRegionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AddRegion

instance Prelude.NFData AddRegion

instance Prelude.ToHeaders AddRegion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "DirectoryService_20150416.AddRegion" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON AddRegion where
  toJSON AddRegion' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DirectoryId" Prelude..= directoryId),
            Prelude.Just ("RegionName" Prelude..= regionName),
            Prelude.Just ("VPCSettings" Prelude..= vPCSettings)
          ]
      )

instance Prelude.ToPath AddRegion where
  toPath = Prelude.const "/"

instance Prelude.ToQuery AddRegion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAddRegionResponse' smart constructor.
data AddRegionResponse = AddRegionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AddRegionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'addRegionResponse_httpStatus' - The response's http status code.
newAddRegionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AddRegionResponse
newAddRegionResponse pHttpStatus_ =
  AddRegionResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
addRegionResponse_httpStatus :: Lens.Lens' AddRegionResponse Prelude.Int
addRegionResponse_httpStatus = Lens.lens (\AddRegionResponse' {httpStatus} -> httpStatus) (\s@AddRegionResponse' {} a -> s {httpStatus = a} :: AddRegionResponse)

instance Prelude.NFData AddRegionResponse
