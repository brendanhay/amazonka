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
-- Module      : Amazonka.APIGateway.DeleteBasePathMapping
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the BasePathMapping resource.
module Amazonka.APIGateway.DeleteBasePathMapping
  ( -- * Creating a Request
    DeleteBasePathMapping (..),
    newDeleteBasePathMapping,

    -- * Request Lenses
    deleteBasePathMapping_domainName,
    deleteBasePathMapping_basePath,

    -- * Destructuring the Response
    DeleteBasePathMappingResponse (..),
    newDeleteBasePathMappingResponse,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | A request to delete the BasePathMapping resource.
--
-- /See:/ 'newDeleteBasePathMapping' smart constructor.
data DeleteBasePathMapping = DeleteBasePathMapping'
  { -- | The domain name of the BasePathMapping resource to delete.
    domainName :: Prelude.Text,
    -- | The base path name of the BasePathMapping resource to delete.
    --
    -- To specify an empty base path, set this parameter to @\'(none)\'@.
    basePath :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteBasePathMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'deleteBasePathMapping_domainName' - The domain name of the BasePathMapping resource to delete.
--
-- 'basePath', 'deleteBasePathMapping_basePath' - The base path name of the BasePathMapping resource to delete.
--
-- To specify an empty base path, set this parameter to @\'(none)\'@.
newDeleteBasePathMapping ::
  -- | 'domainName'
  Prelude.Text ->
  -- | 'basePath'
  Prelude.Text ->
  DeleteBasePathMapping
newDeleteBasePathMapping pDomainName_ pBasePath_ =
  DeleteBasePathMapping'
    { domainName = pDomainName_,
      basePath = pBasePath_
    }

-- | The domain name of the BasePathMapping resource to delete.
deleteBasePathMapping_domainName :: Lens.Lens' DeleteBasePathMapping Prelude.Text
deleteBasePathMapping_domainName = Lens.lens (\DeleteBasePathMapping' {domainName} -> domainName) (\s@DeleteBasePathMapping' {} a -> s {domainName = a} :: DeleteBasePathMapping)

-- | The base path name of the BasePathMapping resource to delete.
--
-- To specify an empty base path, set this parameter to @\'(none)\'@.
deleteBasePathMapping_basePath :: Lens.Lens' DeleteBasePathMapping Prelude.Text
deleteBasePathMapping_basePath = Lens.lens (\DeleteBasePathMapping' {basePath} -> basePath) (\s@DeleteBasePathMapping' {} a -> s {basePath = a} :: DeleteBasePathMapping)

instance Core.AWSRequest DeleteBasePathMapping where
  type
    AWSResponse DeleteBasePathMapping =
      DeleteBasePathMappingResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteBasePathMappingResponse'

instance Prelude.Hashable DeleteBasePathMapping where
  hashWithSalt _salt DeleteBasePathMapping' {..} =
    _salt `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` basePath

instance Prelude.NFData DeleteBasePathMapping where
  rnf DeleteBasePathMapping' {..} =
    Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf basePath

instance Data.ToHeaders DeleteBasePathMapping where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Data.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Data.ToPath DeleteBasePathMapping where
  toPath DeleteBasePathMapping' {..} =
    Prelude.mconcat
      [ "/domainnames/",
        Data.toBS domainName,
        "/basepathmappings/",
        Data.toBS basePath
      ]

instance Data.ToQuery DeleteBasePathMapping where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteBasePathMappingResponse' smart constructor.
data DeleteBasePathMappingResponse = DeleteBasePathMappingResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteBasePathMappingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteBasePathMappingResponse ::
  DeleteBasePathMappingResponse
newDeleteBasePathMappingResponse =
  DeleteBasePathMappingResponse'

instance Prelude.NFData DeleteBasePathMappingResponse where
  rnf _ = ()
