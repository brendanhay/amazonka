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
-- Module      : Network.AWS.APIGateway.UpdateBasePathMapping
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes information about the BasePathMapping resource.
module Network.AWS.APIGateway.UpdateBasePathMapping
  ( -- * Creating a Request
    UpdateBasePathMapping (..),
    newUpdateBasePathMapping,

    -- * Request Lenses
    updateBasePathMapping_patchOperations,
    updateBasePathMapping_domainName,
    updateBasePathMapping_basePath,

    -- * Destructuring the Response
    BasePathMapping (..),
    newBasePathMapping,

    -- * Response Lenses
    basePathMapping_basePath,
    basePathMapping_stage,
    basePathMapping_restApiId,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request to change information about the BasePathMapping resource.
--
-- /See:/ 'newUpdateBasePathMapping' smart constructor.
data UpdateBasePathMapping = UpdateBasePathMapping'
  { -- | A list of update operations to be applied to the specified resource and
    -- in the order specified in this list.
    patchOperations :: Core.Maybe [PatchOperation],
    -- | [Required] The domain name of the BasePathMapping resource to change.
    domainName :: Core.Text,
    -- | [Required] The base path of the BasePathMapping resource to change.
    --
    -- To specify an empty base path, set this parameter to @\'(none)\'@.
    basePath :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateBasePathMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'patchOperations', 'updateBasePathMapping_patchOperations' - A list of update operations to be applied to the specified resource and
-- in the order specified in this list.
--
-- 'domainName', 'updateBasePathMapping_domainName' - [Required] The domain name of the BasePathMapping resource to change.
--
-- 'basePath', 'updateBasePathMapping_basePath' - [Required] The base path of the BasePathMapping resource to change.
--
-- To specify an empty base path, set this parameter to @\'(none)\'@.
newUpdateBasePathMapping ::
  -- | 'domainName'
  Core.Text ->
  -- | 'basePath'
  Core.Text ->
  UpdateBasePathMapping
newUpdateBasePathMapping pDomainName_ pBasePath_ =
  UpdateBasePathMapping'
    { patchOperations =
        Core.Nothing,
      domainName = pDomainName_,
      basePath = pBasePath_
    }

-- | A list of update operations to be applied to the specified resource and
-- in the order specified in this list.
updateBasePathMapping_patchOperations :: Lens.Lens' UpdateBasePathMapping (Core.Maybe [PatchOperation])
updateBasePathMapping_patchOperations = Lens.lens (\UpdateBasePathMapping' {patchOperations} -> patchOperations) (\s@UpdateBasePathMapping' {} a -> s {patchOperations = a} :: UpdateBasePathMapping) Core.. Lens.mapping Lens._Coerce

-- | [Required] The domain name of the BasePathMapping resource to change.
updateBasePathMapping_domainName :: Lens.Lens' UpdateBasePathMapping Core.Text
updateBasePathMapping_domainName = Lens.lens (\UpdateBasePathMapping' {domainName} -> domainName) (\s@UpdateBasePathMapping' {} a -> s {domainName = a} :: UpdateBasePathMapping)

-- | [Required] The base path of the BasePathMapping resource to change.
--
-- To specify an empty base path, set this parameter to @\'(none)\'@.
updateBasePathMapping_basePath :: Lens.Lens' UpdateBasePathMapping Core.Text
updateBasePathMapping_basePath = Lens.lens (\UpdateBasePathMapping' {basePath} -> basePath) (\s@UpdateBasePathMapping' {} a -> s {basePath = a} :: UpdateBasePathMapping)

instance Core.AWSRequest UpdateBasePathMapping where
  type
    AWSResponse UpdateBasePathMapping =
      BasePathMapping
  request = Request.patchJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Core.Hashable UpdateBasePathMapping

instance Core.NFData UpdateBasePathMapping

instance Core.ToHeaders UpdateBasePathMapping where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateBasePathMapping where
  toJSON UpdateBasePathMapping' {..} =
    Core.object
      ( Core.catMaybes
          [ ("patchOperations" Core..=)
              Core.<$> patchOperations
          ]
      )

instance Core.ToPath UpdateBasePathMapping where
  toPath UpdateBasePathMapping' {..} =
    Core.mconcat
      [ "/domainnames/",
        Core.toBS domainName,
        "/basepathmappings/",
        Core.toBS basePath
      ]

instance Core.ToQuery UpdateBasePathMapping where
  toQuery = Core.const Core.mempty
