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
-- Module      : Amazonka.EMR.UpdateStudio
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an Amazon EMR Studio configuration, including attributes such as
-- name, description, and subnets.
module Amazonka.EMR.UpdateStudio
  ( -- * Creating a Request
    UpdateStudio (..),
    newUpdateStudio,

    -- * Request Lenses
    updateStudio_defaultS3Location,
    updateStudio_description,
    updateStudio_name,
    updateStudio_subnetIds,
    updateStudio_studioId,

    -- * Destructuring the Response
    UpdateStudioResponse (..),
    newUpdateStudioResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMR.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateStudio' smart constructor.
data UpdateStudio = UpdateStudio'
  { -- | The Amazon S3 location to back up Workspaces and notebook files for the
    -- Amazon EMR Studio.
    defaultS3Location :: Prelude.Maybe Prelude.Text,
    -- | A detailed description to assign to the Amazon EMR Studio.
    description :: Prelude.Maybe Prelude.Text,
    -- | A descriptive name for the Amazon EMR Studio.
    name :: Prelude.Maybe Prelude.Text,
    -- | A list of subnet IDs to associate with the Amazon EMR Studio. The list
    -- can include new subnet IDs, but must also include all of the subnet IDs
    -- previously associated with the Studio. The list order does not matter. A
    -- Studio can have a maximum of 5 subnets. The subnets must belong to the
    -- same VPC as the Studio.
    subnetIds :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the Amazon EMR Studio to update.
    studioId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateStudio' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultS3Location', 'updateStudio_defaultS3Location' - The Amazon S3 location to back up Workspaces and notebook files for the
-- Amazon EMR Studio.
--
-- 'description', 'updateStudio_description' - A detailed description to assign to the Amazon EMR Studio.
--
-- 'name', 'updateStudio_name' - A descriptive name for the Amazon EMR Studio.
--
-- 'subnetIds', 'updateStudio_subnetIds' - A list of subnet IDs to associate with the Amazon EMR Studio. The list
-- can include new subnet IDs, but must also include all of the subnet IDs
-- previously associated with the Studio. The list order does not matter. A
-- Studio can have a maximum of 5 subnets. The subnets must belong to the
-- same VPC as the Studio.
--
-- 'studioId', 'updateStudio_studioId' - The ID of the Amazon EMR Studio to update.
newUpdateStudio ::
  -- | 'studioId'
  Prelude.Text ->
  UpdateStudio
newUpdateStudio pStudioId_ =
  UpdateStudio'
    { defaultS3Location = Prelude.Nothing,
      description = Prelude.Nothing,
      name = Prelude.Nothing,
      subnetIds = Prelude.Nothing,
      studioId = pStudioId_
    }

-- | The Amazon S3 location to back up Workspaces and notebook files for the
-- Amazon EMR Studio.
updateStudio_defaultS3Location :: Lens.Lens' UpdateStudio (Prelude.Maybe Prelude.Text)
updateStudio_defaultS3Location = Lens.lens (\UpdateStudio' {defaultS3Location} -> defaultS3Location) (\s@UpdateStudio' {} a -> s {defaultS3Location = a} :: UpdateStudio)

-- | A detailed description to assign to the Amazon EMR Studio.
updateStudio_description :: Lens.Lens' UpdateStudio (Prelude.Maybe Prelude.Text)
updateStudio_description = Lens.lens (\UpdateStudio' {description} -> description) (\s@UpdateStudio' {} a -> s {description = a} :: UpdateStudio)

-- | A descriptive name for the Amazon EMR Studio.
updateStudio_name :: Lens.Lens' UpdateStudio (Prelude.Maybe Prelude.Text)
updateStudio_name = Lens.lens (\UpdateStudio' {name} -> name) (\s@UpdateStudio' {} a -> s {name = a} :: UpdateStudio)

-- | A list of subnet IDs to associate with the Amazon EMR Studio. The list
-- can include new subnet IDs, but must also include all of the subnet IDs
-- previously associated with the Studio. The list order does not matter. A
-- Studio can have a maximum of 5 subnets. The subnets must belong to the
-- same VPC as the Studio.
updateStudio_subnetIds :: Lens.Lens' UpdateStudio (Prelude.Maybe [Prelude.Text])
updateStudio_subnetIds = Lens.lens (\UpdateStudio' {subnetIds} -> subnetIds) (\s@UpdateStudio' {} a -> s {subnetIds = a} :: UpdateStudio) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the Amazon EMR Studio to update.
updateStudio_studioId :: Lens.Lens' UpdateStudio Prelude.Text
updateStudio_studioId = Lens.lens (\UpdateStudio' {studioId} -> studioId) (\s@UpdateStudio' {} a -> s {studioId = a} :: UpdateStudio)

instance Core.AWSRequest UpdateStudio where
  type AWSResponse UpdateStudio = UpdateStudioResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response = Response.receiveNull UpdateStudioResponse'

instance Prelude.Hashable UpdateStudio where
  hashWithSalt _salt UpdateStudio' {..} =
    _salt `Prelude.hashWithSalt` defaultS3Location
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` subnetIds
      `Prelude.hashWithSalt` studioId

instance Prelude.NFData UpdateStudio where
  rnf UpdateStudio' {..} =
    Prelude.rnf defaultS3Location
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf subnetIds
      `Prelude.seq` Prelude.rnf studioId

instance Data.ToHeaders UpdateStudio where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ElasticMapReduce.UpdateStudio" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateStudio where
  toJSON UpdateStudio' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DefaultS3Location" Data..=)
              Prelude.<$> defaultS3Location,
            ("Description" Data..=) Prelude.<$> description,
            ("Name" Data..=) Prelude.<$> name,
            ("SubnetIds" Data..=) Prelude.<$> subnetIds,
            Prelude.Just ("StudioId" Data..= studioId)
          ]
      )

instance Data.ToPath UpdateStudio where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateStudio where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateStudioResponse' smart constructor.
data UpdateStudioResponse = UpdateStudioResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateStudioResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateStudioResponse ::
  UpdateStudioResponse
newUpdateStudioResponse = UpdateStudioResponse'

instance Prelude.NFData UpdateStudioResponse where
  rnf _ = ()
