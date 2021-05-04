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
-- Module      : Network.AWS.EMR.UpdateStudio
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an Amazon EMR Studio configuration, including attributes such as
-- name, description, and subnets.
module Network.AWS.EMR.UpdateStudio
  ( -- * Creating a Request
    UpdateStudio (..),
    newUpdateStudio,

    -- * Request Lenses
    updateStudio_defaultS3Location,
    updateStudio_subnetIds,
    updateStudio_name,
    updateStudio_description,
    updateStudio_studioId,

    -- * Destructuring the Response
    UpdateStudioResponse (..),
    newUpdateStudioResponse,
  )
where

import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateStudio' smart constructor.
data UpdateStudio = UpdateStudio'
  { -- | A default Amazon S3 location to back up Workspaces and notebook files
    -- for the Amazon EMR Studio. A Studio user can select an alternative
    -- Amazon S3 location when creating a Workspace.
    defaultS3Location :: Prelude.Maybe Prelude.Text,
    -- | A list of subnet IDs to associate with the Amazon EMR Studio. The list
    -- can include new subnet IDs, but must also include all of the subnet IDs
    -- previously associated with the Studio. The list order does not matter. A
    -- Studio can have a maximum of 5 subnets. The subnets must belong to the
    -- same VPC as the Studio.
    subnetIds :: Prelude.Maybe [Prelude.Text],
    -- | A descriptive name for the Amazon EMR Studio.
    name :: Prelude.Maybe Prelude.Text,
    -- | A detailed description to assign to the Amazon EMR Studio.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon EMR Studio to update.
    studioId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateStudio' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultS3Location', 'updateStudio_defaultS3Location' - A default Amazon S3 location to back up Workspaces and notebook files
-- for the Amazon EMR Studio. A Studio user can select an alternative
-- Amazon S3 location when creating a Workspace.
--
-- 'subnetIds', 'updateStudio_subnetIds' - A list of subnet IDs to associate with the Amazon EMR Studio. The list
-- can include new subnet IDs, but must also include all of the subnet IDs
-- previously associated with the Studio. The list order does not matter. A
-- Studio can have a maximum of 5 subnets. The subnets must belong to the
-- same VPC as the Studio.
--
-- 'name', 'updateStudio_name' - A descriptive name for the Amazon EMR Studio.
--
-- 'description', 'updateStudio_description' - A detailed description to assign to the Amazon EMR Studio.
--
-- 'studioId', 'updateStudio_studioId' - The ID of the Amazon EMR Studio to update.
newUpdateStudio ::
  -- | 'studioId'
  Prelude.Text ->
  UpdateStudio
newUpdateStudio pStudioId_ =
  UpdateStudio'
    { defaultS3Location = Prelude.Nothing,
      subnetIds = Prelude.Nothing,
      name = Prelude.Nothing,
      description = Prelude.Nothing,
      studioId = pStudioId_
    }

-- | A default Amazon S3 location to back up Workspaces and notebook files
-- for the Amazon EMR Studio. A Studio user can select an alternative
-- Amazon S3 location when creating a Workspace.
updateStudio_defaultS3Location :: Lens.Lens' UpdateStudio (Prelude.Maybe Prelude.Text)
updateStudio_defaultS3Location = Lens.lens (\UpdateStudio' {defaultS3Location} -> defaultS3Location) (\s@UpdateStudio' {} a -> s {defaultS3Location = a} :: UpdateStudio)

-- | A list of subnet IDs to associate with the Amazon EMR Studio. The list
-- can include new subnet IDs, but must also include all of the subnet IDs
-- previously associated with the Studio. The list order does not matter. A
-- Studio can have a maximum of 5 subnets. The subnets must belong to the
-- same VPC as the Studio.
updateStudio_subnetIds :: Lens.Lens' UpdateStudio (Prelude.Maybe [Prelude.Text])
updateStudio_subnetIds = Lens.lens (\UpdateStudio' {subnetIds} -> subnetIds) (\s@UpdateStudio' {} a -> s {subnetIds = a} :: UpdateStudio) Prelude.. Lens.mapping Prelude._Coerce

-- | A descriptive name for the Amazon EMR Studio.
updateStudio_name :: Lens.Lens' UpdateStudio (Prelude.Maybe Prelude.Text)
updateStudio_name = Lens.lens (\UpdateStudio' {name} -> name) (\s@UpdateStudio' {} a -> s {name = a} :: UpdateStudio)

-- | A detailed description to assign to the Amazon EMR Studio.
updateStudio_description :: Lens.Lens' UpdateStudio (Prelude.Maybe Prelude.Text)
updateStudio_description = Lens.lens (\UpdateStudio' {description} -> description) (\s@UpdateStudio' {} a -> s {description = a} :: UpdateStudio)

-- | The ID of the Amazon EMR Studio to update.
updateStudio_studioId :: Lens.Lens' UpdateStudio Prelude.Text
updateStudio_studioId = Lens.lens (\UpdateStudio' {studioId} -> studioId) (\s@UpdateStudio' {} a -> s {studioId = a} :: UpdateStudio)

instance Prelude.AWSRequest UpdateStudio where
  type Rs UpdateStudio = UpdateStudioResponse
  request = Request.postJSON defaultService
  response = Response.receiveNull UpdateStudioResponse'

instance Prelude.Hashable UpdateStudio

instance Prelude.NFData UpdateStudio

instance Prelude.ToHeaders UpdateStudio where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "ElasticMapReduce.UpdateStudio" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateStudio where
  toJSON UpdateStudio' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("DefaultS3Location" Prelude..=)
              Prelude.<$> defaultS3Location,
            ("SubnetIds" Prelude..=) Prelude.<$> subnetIds,
            ("Name" Prelude..=) Prelude.<$> name,
            ("Description" Prelude..=) Prelude.<$> description,
            Prelude.Just ("StudioId" Prelude..= studioId)
          ]
      )

instance Prelude.ToPath UpdateStudio where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateStudio where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateStudioResponse' smart constructor.
data UpdateStudioResponse = UpdateStudioResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateStudioResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateStudioResponse ::
  UpdateStudioResponse
newUpdateStudioResponse = UpdateStudioResponse'

instance Prelude.NFData UpdateStudioResponse
