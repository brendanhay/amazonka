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
-- Module      : Amazonka.SageMaker.DeleteFeatureGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete the @FeatureGroup@ and any data that was written to the
-- @OnlineStore@ of the @FeatureGroup@. Data cannot be accessed from the
-- @OnlineStore@ immediately after @DeleteFeatureGroup@ is called.
--
-- Data written into the @OfflineStore@ will not be deleted. The Amazon Web
-- Services Glue database and tables that are automatically created for
-- your @OfflineStore@ are not deleted.
module Amazonka.SageMaker.DeleteFeatureGroup
  ( -- * Creating a Request
    DeleteFeatureGroup (..),
    newDeleteFeatureGroup,

    -- * Request Lenses
    deleteFeatureGroup_featureGroupName,

    -- * Destructuring the Response
    DeleteFeatureGroupResponse (..),
    newDeleteFeatureGroupResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDeleteFeatureGroup' smart constructor.
data DeleteFeatureGroup = DeleteFeatureGroup'
  { -- | The name of the @FeatureGroup@ you want to delete. The name must be
    -- unique within an Amazon Web Services Region in an Amazon Web Services
    -- account.
    featureGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFeatureGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'featureGroupName', 'deleteFeatureGroup_featureGroupName' - The name of the @FeatureGroup@ you want to delete. The name must be
-- unique within an Amazon Web Services Region in an Amazon Web Services
-- account.
newDeleteFeatureGroup ::
  -- | 'featureGroupName'
  Prelude.Text ->
  DeleteFeatureGroup
newDeleteFeatureGroup pFeatureGroupName_ =
  DeleteFeatureGroup'
    { featureGroupName =
        pFeatureGroupName_
    }

-- | The name of the @FeatureGroup@ you want to delete. The name must be
-- unique within an Amazon Web Services Region in an Amazon Web Services
-- account.
deleteFeatureGroup_featureGroupName :: Lens.Lens' DeleteFeatureGroup Prelude.Text
deleteFeatureGroup_featureGroupName = Lens.lens (\DeleteFeatureGroup' {featureGroupName} -> featureGroupName) (\s@DeleteFeatureGroup' {} a -> s {featureGroupName = a} :: DeleteFeatureGroup)

instance Core.AWSRequest DeleteFeatureGroup where
  type
    AWSResponse DeleteFeatureGroup =
      DeleteFeatureGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DeleteFeatureGroupResponse'

instance Prelude.Hashable DeleteFeatureGroup where
  hashWithSalt _salt DeleteFeatureGroup' {..} =
    _salt `Prelude.hashWithSalt` featureGroupName

instance Prelude.NFData DeleteFeatureGroup where
  rnf DeleteFeatureGroup' {..} =
    Prelude.rnf featureGroupName

instance Core.ToHeaders DeleteFeatureGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.DeleteFeatureGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteFeatureGroup where
  toJSON DeleteFeatureGroup' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("FeatureGroupName" Core..= featureGroupName)
          ]
      )

instance Core.ToPath DeleteFeatureGroup where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteFeatureGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteFeatureGroupResponse' smart constructor.
data DeleteFeatureGroupResponse = DeleteFeatureGroupResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFeatureGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteFeatureGroupResponse ::
  DeleteFeatureGroupResponse
newDeleteFeatureGroupResponse =
  DeleteFeatureGroupResponse'

instance Prelude.NFData DeleteFeatureGroupResponse where
  rnf _ = ()
