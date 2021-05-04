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
-- Module      : Network.AWS.APIGateway.DeleteBasePathMapping
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the BasePathMapping resource.
module Network.AWS.APIGateway.DeleteBasePathMapping
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

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request to delete the BasePathMapping resource.
--
-- /See:/ 'newDeleteBasePathMapping' smart constructor.
data DeleteBasePathMapping = DeleteBasePathMapping'
  { -- | [Required] The domain name of the BasePathMapping resource to delete.
    domainName :: Prelude.Text,
    -- | [Required] The base path name of the BasePathMapping resource to delete.
    --
    -- To specify an empty base path, set this parameter to @\'(none)\'@.
    basePath :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteBasePathMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'deleteBasePathMapping_domainName' - [Required] The domain name of the BasePathMapping resource to delete.
--
-- 'basePath', 'deleteBasePathMapping_basePath' - [Required] The base path name of the BasePathMapping resource to delete.
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

-- | [Required] The domain name of the BasePathMapping resource to delete.
deleteBasePathMapping_domainName :: Lens.Lens' DeleteBasePathMapping Prelude.Text
deleteBasePathMapping_domainName = Lens.lens (\DeleteBasePathMapping' {domainName} -> domainName) (\s@DeleteBasePathMapping' {} a -> s {domainName = a} :: DeleteBasePathMapping)

-- | [Required] The base path name of the BasePathMapping resource to delete.
--
-- To specify an empty base path, set this parameter to @\'(none)\'@.
deleteBasePathMapping_basePath :: Lens.Lens' DeleteBasePathMapping Prelude.Text
deleteBasePathMapping_basePath = Lens.lens (\DeleteBasePathMapping' {basePath} -> basePath) (\s@DeleteBasePathMapping' {} a -> s {basePath = a} :: DeleteBasePathMapping)

instance Prelude.AWSRequest DeleteBasePathMapping where
  type
    Rs DeleteBasePathMapping =
      DeleteBasePathMappingResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull DeleteBasePathMappingResponse'

instance Prelude.Hashable DeleteBasePathMapping

instance Prelude.NFData DeleteBasePathMapping

instance Prelude.ToHeaders DeleteBasePathMapping where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Prelude.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Prelude.ToPath DeleteBasePathMapping where
  toPath DeleteBasePathMapping' {..} =
    Prelude.mconcat
      [ "/domainnames/",
        Prelude.toBS domainName,
        "/basepathmappings/",
        Prelude.toBS basePath
      ]

instance Prelude.ToQuery DeleteBasePathMapping where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteBasePathMappingResponse' smart constructor.
data DeleteBasePathMappingResponse = DeleteBasePathMappingResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteBasePathMappingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteBasePathMappingResponse ::
  DeleteBasePathMappingResponse
newDeleteBasePathMappingResponse =
  DeleteBasePathMappingResponse'

instance Prelude.NFData DeleteBasePathMappingResponse
