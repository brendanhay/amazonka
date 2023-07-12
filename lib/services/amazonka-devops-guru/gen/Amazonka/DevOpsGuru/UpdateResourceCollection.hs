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
-- Module      : Amazonka.DevOpsGuru.UpdateResourceCollection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the collection of resources that DevOps Guru analyzes. The two
-- types of Amazon Web Services resource collections supported are Amazon
-- Web Services CloudFormation stacks and Amazon Web Services resources
-- that contain the same Amazon Web Services tag. DevOps Guru can be
-- configured to analyze the Amazon Web Services resources that are defined
-- in the stacks or that are tagged using the same tag /key/. You can
-- specify up to 500 Amazon Web Services CloudFormation stacks. This method
-- also creates the IAM role required for you to use DevOps Guru.
module Amazonka.DevOpsGuru.UpdateResourceCollection
  ( -- * Creating a Request
    UpdateResourceCollection (..),
    newUpdateResourceCollection,

    -- * Request Lenses
    updateResourceCollection_action,
    updateResourceCollection_resourceCollection,

    -- * Destructuring the Response
    UpdateResourceCollectionResponse (..),
    newUpdateResourceCollectionResponse,

    -- * Response Lenses
    updateResourceCollectionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DevOpsGuru.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateResourceCollection' smart constructor.
data UpdateResourceCollection = UpdateResourceCollection'
  { -- | Specifies if the resource collection in the request is added or deleted
    -- to the resource collection.
    action :: UpdateResourceCollectionAction,
    resourceCollection :: UpdateResourceCollectionFilter
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateResourceCollection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'action', 'updateResourceCollection_action' - Specifies if the resource collection in the request is added or deleted
-- to the resource collection.
--
-- 'resourceCollection', 'updateResourceCollection_resourceCollection' - Undocumented member.
newUpdateResourceCollection ::
  -- | 'action'
  UpdateResourceCollectionAction ->
  -- | 'resourceCollection'
  UpdateResourceCollectionFilter ->
  UpdateResourceCollection
newUpdateResourceCollection
  pAction_
  pResourceCollection_ =
    UpdateResourceCollection'
      { action = pAction_,
        resourceCollection = pResourceCollection_
      }

-- | Specifies if the resource collection in the request is added or deleted
-- to the resource collection.
updateResourceCollection_action :: Lens.Lens' UpdateResourceCollection UpdateResourceCollectionAction
updateResourceCollection_action = Lens.lens (\UpdateResourceCollection' {action} -> action) (\s@UpdateResourceCollection' {} a -> s {action = a} :: UpdateResourceCollection)

-- | Undocumented member.
updateResourceCollection_resourceCollection :: Lens.Lens' UpdateResourceCollection UpdateResourceCollectionFilter
updateResourceCollection_resourceCollection = Lens.lens (\UpdateResourceCollection' {resourceCollection} -> resourceCollection) (\s@UpdateResourceCollection' {} a -> s {resourceCollection = a} :: UpdateResourceCollection)

instance Core.AWSRequest UpdateResourceCollection where
  type
    AWSResponse UpdateResourceCollection =
      UpdateResourceCollectionResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateResourceCollectionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateResourceCollection where
  hashWithSalt _salt UpdateResourceCollection' {..} =
    _salt
      `Prelude.hashWithSalt` action
      `Prelude.hashWithSalt` resourceCollection

instance Prelude.NFData UpdateResourceCollection where
  rnf UpdateResourceCollection' {..} =
    Prelude.rnf action
      `Prelude.seq` Prelude.rnf resourceCollection

instance Data.ToHeaders UpdateResourceCollection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateResourceCollection where
  toJSON UpdateResourceCollection' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Action" Data..= action),
            Prelude.Just
              ("ResourceCollection" Data..= resourceCollection)
          ]
      )

instance Data.ToPath UpdateResourceCollection where
  toPath = Prelude.const "/resource-collections"

instance Data.ToQuery UpdateResourceCollection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateResourceCollectionResponse' smart constructor.
data UpdateResourceCollectionResponse = UpdateResourceCollectionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateResourceCollectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateResourceCollectionResponse_httpStatus' - The response's http status code.
newUpdateResourceCollectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateResourceCollectionResponse
newUpdateResourceCollectionResponse pHttpStatus_ =
  UpdateResourceCollectionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateResourceCollectionResponse_httpStatus :: Lens.Lens' UpdateResourceCollectionResponse Prelude.Int
updateResourceCollectionResponse_httpStatus = Lens.lens (\UpdateResourceCollectionResponse' {httpStatus} -> httpStatus) (\s@UpdateResourceCollectionResponse' {} a -> s {httpStatus = a} :: UpdateResourceCollectionResponse)

instance
  Prelude.NFData
    UpdateResourceCollectionResponse
  where
  rnf UpdateResourceCollectionResponse' {..} =
    Prelude.rnf httpStatus
