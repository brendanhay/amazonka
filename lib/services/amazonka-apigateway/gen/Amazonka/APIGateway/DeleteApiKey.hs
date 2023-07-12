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
-- Module      : Amazonka.APIGateway.DeleteApiKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the ApiKey resource.
module Amazonka.APIGateway.DeleteApiKey
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

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | A request to delete the ApiKey resource.
--
-- /See:/ 'newDeleteApiKey' smart constructor.
data DeleteApiKey = DeleteApiKey'
  { -- | The identifier of the ApiKey resource to be deleted.
    apiKey :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteApiKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiKey', 'deleteApiKey_apiKey' - The identifier of the ApiKey resource to be deleted.
newDeleteApiKey ::
  -- | 'apiKey'
  Prelude.Text ->
  DeleteApiKey
newDeleteApiKey pApiKey_ =
  DeleteApiKey' {apiKey = pApiKey_}

-- | The identifier of the ApiKey resource to be deleted.
deleteApiKey_apiKey :: Lens.Lens' DeleteApiKey Prelude.Text
deleteApiKey_apiKey = Lens.lens (\DeleteApiKey' {apiKey} -> apiKey) (\s@DeleteApiKey' {} a -> s {apiKey = a} :: DeleteApiKey)

instance Core.AWSRequest DeleteApiKey where
  type AWSResponse DeleteApiKey = DeleteApiKeyResponse
  request overrides =
    Request.delete (overrides defaultService)
  response = Response.receiveNull DeleteApiKeyResponse'

instance Prelude.Hashable DeleteApiKey where
  hashWithSalt _salt DeleteApiKey' {..} =
    _salt `Prelude.hashWithSalt` apiKey

instance Prelude.NFData DeleteApiKey where
  rnf DeleteApiKey' {..} = Prelude.rnf apiKey

instance Data.ToHeaders DeleteApiKey where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Data.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Data.ToPath DeleteApiKey where
  toPath DeleteApiKey' {..} =
    Prelude.mconcat ["/apikeys/", Data.toBS apiKey]

instance Data.ToQuery DeleteApiKey where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteApiKeyResponse' smart constructor.
data DeleteApiKeyResponse = DeleteApiKeyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteApiKeyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteApiKeyResponse ::
  DeleteApiKeyResponse
newDeleteApiKeyResponse = DeleteApiKeyResponse'

instance Prelude.NFData DeleteApiKeyResponse where
  rnf _ = ()
