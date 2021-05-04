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
-- Module      : Network.AWS.APIGateway.DeleteApiKey
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the ApiKey resource.
module Network.AWS.APIGateway.DeleteApiKey
  ( -- * Creating a Request
    DeleteApiKey (..),
    newDeleteApiKey,

    -- * Request Lenses
    deleteApiKey_apiKey,

    -- * Destructuring the Response
    DeleteApiKeyResponse (..),
    newDeleteApiKeyResponse,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request to delete the ApiKey resource.
--
-- /See:/ 'newDeleteApiKey' smart constructor.
data DeleteApiKey = DeleteApiKey'
  { -- | [Required] The identifier of the ApiKey resource to be deleted.
    apiKey :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteApiKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiKey', 'deleteApiKey_apiKey' - [Required] The identifier of the ApiKey resource to be deleted.
newDeleteApiKey ::
  -- | 'apiKey'
  Prelude.Text ->
  DeleteApiKey
newDeleteApiKey pApiKey_ =
  DeleteApiKey' {apiKey = pApiKey_}

-- | [Required] The identifier of the ApiKey resource to be deleted.
deleteApiKey_apiKey :: Lens.Lens' DeleteApiKey Prelude.Text
deleteApiKey_apiKey = Lens.lens (\DeleteApiKey' {apiKey} -> apiKey) (\s@DeleteApiKey' {} a -> s {apiKey = a} :: DeleteApiKey)

instance Prelude.AWSRequest DeleteApiKey where
  type Rs DeleteApiKey = DeleteApiKeyResponse
  request = Request.delete defaultService
  response = Response.receiveNull DeleteApiKeyResponse'

instance Prelude.Hashable DeleteApiKey

instance Prelude.NFData DeleteApiKey

instance Prelude.ToHeaders DeleteApiKey where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Prelude.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Prelude.ToPath DeleteApiKey where
  toPath DeleteApiKey' {..} =
    Prelude.mconcat ["/apikeys/", Prelude.toBS apiKey]

instance Prelude.ToQuery DeleteApiKey where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteApiKeyResponse' smart constructor.
data DeleteApiKeyResponse = DeleteApiKeyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteApiKeyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteApiKeyResponse ::
  DeleteApiKeyResponse
newDeleteApiKeyResponse = DeleteApiKeyResponse'

instance Prelude.NFData DeleteApiKeyResponse
