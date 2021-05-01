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
-- Module      : Network.AWS.APIGateway.DeleteRestApi
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified API.
module Network.AWS.APIGateway.DeleteRestApi
  ( -- * Creating a Request
    DeleteRestApi (..),
    newDeleteRestApi,

    -- * Request Lenses
    deleteRestApi_restApiId,

    -- * Destructuring the Response
    DeleteRestApiResponse (..),
    newDeleteRestApiResponse,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to delete the specified API from your collection.
--
-- /See:/ 'newDeleteRestApi' smart constructor.
data DeleteRestApi = DeleteRestApi'
  { -- | [Required] The string identifier of the associated RestApi.
    restApiId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteRestApi' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'restApiId', 'deleteRestApi_restApiId' - [Required] The string identifier of the associated RestApi.
newDeleteRestApi ::
  -- | 'restApiId'
  Prelude.Text ->
  DeleteRestApi
newDeleteRestApi pRestApiId_ =
  DeleteRestApi' {restApiId = pRestApiId_}

-- | [Required] The string identifier of the associated RestApi.
deleteRestApi_restApiId :: Lens.Lens' DeleteRestApi Prelude.Text
deleteRestApi_restApiId = Lens.lens (\DeleteRestApi' {restApiId} -> restApiId) (\s@DeleteRestApi' {} a -> s {restApiId = a} :: DeleteRestApi)

instance Prelude.AWSRequest DeleteRestApi where
  type Rs DeleteRestApi = DeleteRestApiResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull DeleteRestApiResponse'

instance Prelude.Hashable DeleteRestApi

instance Prelude.NFData DeleteRestApi

instance Prelude.ToHeaders DeleteRestApi where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Prelude.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Prelude.ToPath DeleteRestApi where
  toPath DeleteRestApi' {..} =
    Prelude.mconcat
      ["/restapis/", Prelude.toBS restApiId]

instance Prelude.ToQuery DeleteRestApi where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteRestApiResponse' smart constructor.
data DeleteRestApiResponse = DeleteRestApiResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteRestApiResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteRestApiResponse ::
  DeleteRestApiResponse
newDeleteRestApiResponse = DeleteRestApiResponse'

instance Prelude.NFData DeleteRestApiResponse
