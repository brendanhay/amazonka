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
-- Module      : Network.AWS.Glue.UpdateDevEndpoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a specified development endpoint.
module Network.AWS.Glue.UpdateDevEndpoint
  ( -- * Creating a Request
    UpdateDevEndpoint (..),
    newUpdateDevEndpoint,

    -- * Request Lenses
    updateDevEndpoint_publicKey,
    updateDevEndpoint_updateEtlLibraries,
    updateDevEndpoint_addPublicKeys,
    updateDevEndpoint_deletePublicKeys,
    updateDevEndpoint_addArguments,
    updateDevEndpoint_deleteArguments,
    updateDevEndpoint_customLibraries,
    updateDevEndpoint_endpointName,

    -- * Destructuring the Response
    UpdateDevEndpointResponse (..),
    newUpdateDevEndpointResponse,

    -- * Response Lenses
    updateDevEndpointResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateDevEndpoint' smart constructor.
data UpdateDevEndpoint = UpdateDevEndpoint'
  { -- | The public key for the @DevEndpoint@ to use.
    publicKey :: Core.Maybe Core.Text,
    -- | @True@ if the list of custom libraries to be loaded in the development
    -- endpoint needs to be updated, or @False@ if otherwise.
    updateEtlLibraries :: Core.Maybe Core.Bool,
    -- | The list of public keys for the @DevEndpoint@ to use.
    addPublicKeys :: Core.Maybe [Core.Text],
    -- | The list of public keys to be deleted from the @DevEndpoint@.
    deletePublicKeys :: Core.Maybe [Core.Text],
    -- | The map of arguments to add the map of arguments used to configure the
    -- @DevEndpoint@.
    --
    -- Valid arguments are:
    --
    -- -   @\"--enable-glue-datacatalog\": \"\"@
    --
    -- -   @\"GLUE_PYTHON_VERSION\": \"3\"@
    --
    -- -   @\"GLUE_PYTHON_VERSION\": \"2\"@
    --
    -- You can specify a version of Python support for development endpoints by
    -- using the @Arguments@ parameter in the @CreateDevEndpoint@ or
    -- @UpdateDevEndpoint@ APIs. If no arguments are provided, the version
    -- defaults to Python 2.
    addArguments :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The list of argument keys to be deleted from the map of arguments used
    -- to configure the @DevEndpoint@.
    deleteArguments :: Core.Maybe [Core.Text],
    -- | Custom Python or Java libraries to be loaded in the @DevEndpoint@.
    customLibraries :: Core.Maybe DevEndpointCustomLibraries,
    -- | The name of the @DevEndpoint@ to be updated.
    endpointName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateDevEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'publicKey', 'updateDevEndpoint_publicKey' - The public key for the @DevEndpoint@ to use.
--
-- 'updateEtlLibraries', 'updateDevEndpoint_updateEtlLibraries' - @True@ if the list of custom libraries to be loaded in the development
-- endpoint needs to be updated, or @False@ if otherwise.
--
-- 'addPublicKeys', 'updateDevEndpoint_addPublicKeys' - The list of public keys for the @DevEndpoint@ to use.
--
-- 'deletePublicKeys', 'updateDevEndpoint_deletePublicKeys' - The list of public keys to be deleted from the @DevEndpoint@.
--
-- 'addArguments', 'updateDevEndpoint_addArguments' - The map of arguments to add the map of arguments used to configure the
-- @DevEndpoint@.
--
-- Valid arguments are:
--
-- -   @\"--enable-glue-datacatalog\": \"\"@
--
-- -   @\"GLUE_PYTHON_VERSION\": \"3\"@
--
-- -   @\"GLUE_PYTHON_VERSION\": \"2\"@
--
-- You can specify a version of Python support for development endpoints by
-- using the @Arguments@ parameter in the @CreateDevEndpoint@ or
-- @UpdateDevEndpoint@ APIs. If no arguments are provided, the version
-- defaults to Python 2.
--
-- 'deleteArguments', 'updateDevEndpoint_deleteArguments' - The list of argument keys to be deleted from the map of arguments used
-- to configure the @DevEndpoint@.
--
-- 'customLibraries', 'updateDevEndpoint_customLibraries' - Custom Python or Java libraries to be loaded in the @DevEndpoint@.
--
-- 'endpointName', 'updateDevEndpoint_endpointName' - The name of the @DevEndpoint@ to be updated.
newUpdateDevEndpoint ::
  -- | 'endpointName'
  Core.Text ->
  UpdateDevEndpoint
newUpdateDevEndpoint pEndpointName_ =
  UpdateDevEndpoint'
    { publicKey = Core.Nothing,
      updateEtlLibraries = Core.Nothing,
      addPublicKeys = Core.Nothing,
      deletePublicKeys = Core.Nothing,
      addArguments = Core.Nothing,
      deleteArguments = Core.Nothing,
      customLibraries = Core.Nothing,
      endpointName = pEndpointName_
    }

-- | The public key for the @DevEndpoint@ to use.
updateDevEndpoint_publicKey :: Lens.Lens' UpdateDevEndpoint (Core.Maybe Core.Text)
updateDevEndpoint_publicKey = Lens.lens (\UpdateDevEndpoint' {publicKey} -> publicKey) (\s@UpdateDevEndpoint' {} a -> s {publicKey = a} :: UpdateDevEndpoint)

-- | @True@ if the list of custom libraries to be loaded in the development
-- endpoint needs to be updated, or @False@ if otherwise.
updateDevEndpoint_updateEtlLibraries :: Lens.Lens' UpdateDevEndpoint (Core.Maybe Core.Bool)
updateDevEndpoint_updateEtlLibraries = Lens.lens (\UpdateDevEndpoint' {updateEtlLibraries} -> updateEtlLibraries) (\s@UpdateDevEndpoint' {} a -> s {updateEtlLibraries = a} :: UpdateDevEndpoint)

-- | The list of public keys for the @DevEndpoint@ to use.
updateDevEndpoint_addPublicKeys :: Lens.Lens' UpdateDevEndpoint (Core.Maybe [Core.Text])
updateDevEndpoint_addPublicKeys = Lens.lens (\UpdateDevEndpoint' {addPublicKeys} -> addPublicKeys) (\s@UpdateDevEndpoint' {} a -> s {addPublicKeys = a} :: UpdateDevEndpoint) Core.. Lens.mapping Lens._Coerce

-- | The list of public keys to be deleted from the @DevEndpoint@.
updateDevEndpoint_deletePublicKeys :: Lens.Lens' UpdateDevEndpoint (Core.Maybe [Core.Text])
updateDevEndpoint_deletePublicKeys = Lens.lens (\UpdateDevEndpoint' {deletePublicKeys} -> deletePublicKeys) (\s@UpdateDevEndpoint' {} a -> s {deletePublicKeys = a} :: UpdateDevEndpoint) Core.. Lens.mapping Lens._Coerce

-- | The map of arguments to add the map of arguments used to configure the
-- @DevEndpoint@.
--
-- Valid arguments are:
--
-- -   @\"--enable-glue-datacatalog\": \"\"@
--
-- -   @\"GLUE_PYTHON_VERSION\": \"3\"@
--
-- -   @\"GLUE_PYTHON_VERSION\": \"2\"@
--
-- You can specify a version of Python support for development endpoints by
-- using the @Arguments@ parameter in the @CreateDevEndpoint@ or
-- @UpdateDevEndpoint@ APIs. If no arguments are provided, the version
-- defaults to Python 2.
updateDevEndpoint_addArguments :: Lens.Lens' UpdateDevEndpoint (Core.Maybe (Core.HashMap Core.Text Core.Text))
updateDevEndpoint_addArguments = Lens.lens (\UpdateDevEndpoint' {addArguments} -> addArguments) (\s@UpdateDevEndpoint' {} a -> s {addArguments = a} :: UpdateDevEndpoint) Core.. Lens.mapping Lens._Coerce

-- | The list of argument keys to be deleted from the map of arguments used
-- to configure the @DevEndpoint@.
updateDevEndpoint_deleteArguments :: Lens.Lens' UpdateDevEndpoint (Core.Maybe [Core.Text])
updateDevEndpoint_deleteArguments = Lens.lens (\UpdateDevEndpoint' {deleteArguments} -> deleteArguments) (\s@UpdateDevEndpoint' {} a -> s {deleteArguments = a} :: UpdateDevEndpoint) Core.. Lens.mapping Lens._Coerce

-- | Custom Python or Java libraries to be loaded in the @DevEndpoint@.
updateDevEndpoint_customLibraries :: Lens.Lens' UpdateDevEndpoint (Core.Maybe DevEndpointCustomLibraries)
updateDevEndpoint_customLibraries = Lens.lens (\UpdateDevEndpoint' {customLibraries} -> customLibraries) (\s@UpdateDevEndpoint' {} a -> s {customLibraries = a} :: UpdateDevEndpoint)

-- | The name of the @DevEndpoint@ to be updated.
updateDevEndpoint_endpointName :: Lens.Lens' UpdateDevEndpoint Core.Text
updateDevEndpoint_endpointName = Lens.lens (\UpdateDevEndpoint' {endpointName} -> endpointName) (\s@UpdateDevEndpoint' {} a -> s {endpointName = a} :: UpdateDevEndpoint)

instance Core.AWSRequest UpdateDevEndpoint where
  type
    AWSResponse UpdateDevEndpoint =
      UpdateDevEndpointResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateDevEndpointResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateDevEndpoint

instance Core.NFData UpdateDevEndpoint

instance Core.ToHeaders UpdateDevEndpoint where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.UpdateDevEndpoint" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateDevEndpoint where
  toJSON UpdateDevEndpoint' {..} =
    Core.object
      ( Core.catMaybes
          [ ("PublicKey" Core..=) Core.<$> publicKey,
            ("UpdateEtlLibraries" Core..=)
              Core.<$> updateEtlLibraries,
            ("AddPublicKeys" Core..=) Core.<$> addPublicKeys,
            ("DeletePublicKeys" Core..=)
              Core.<$> deletePublicKeys,
            ("AddArguments" Core..=) Core.<$> addArguments,
            ("DeleteArguments" Core..=) Core.<$> deleteArguments,
            ("CustomLibraries" Core..=) Core.<$> customLibraries,
            Core.Just ("EndpointName" Core..= endpointName)
          ]
      )

instance Core.ToPath UpdateDevEndpoint where
  toPath = Core.const "/"

instance Core.ToQuery UpdateDevEndpoint where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateDevEndpointResponse' smart constructor.
data UpdateDevEndpointResponse = UpdateDevEndpointResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateDevEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateDevEndpointResponse_httpStatus' - The response's http status code.
newUpdateDevEndpointResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateDevEndpointResponse
newUpdateDevEndpointResponse pHttpStatus_ =
  UpdateDevEndpointResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateDevEndpointResponse_httpStatus :: Lens.Lens' UpdateDevEndpointResponse Core.Int
updateDevEndpointResponse_httpStatus = Lens.lens (\UpdateDevEndpointResponse' {httpStatus} -> httpStatus) (\s@UpdateDevEndpointResponse' {} a -> s {httpStatus = a} :: UpdateDevEndpointResponse)

instance Core.NFData UpdateDevEndpointResponse
