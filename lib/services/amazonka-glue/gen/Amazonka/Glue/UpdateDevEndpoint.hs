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
-- Module      : Amazonka.Glue.UpdateDevEndpoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a specified development endpoint.
module Amazonka.Glue.UpdateDevEndpoint
  ( -- * Creating a Request
    UpdateDevEndpoint (..),
    newUpdateDevEndpoint,

    -- * Request Lenses
    updateDevEndpoint_addPublicKeys,
    updateDevEndpoint_customLibraries,
    updateDevEndpoint_publicKey,
    updateDevEndpoint_deleteArguments,
    updateDevEndpoint_deletePublicKeys,
    updateDevEndpoint_updateEtlLibraries,
    updateDevEndpoint_addArguments,
    updateDevEndpoint_endpointName,

    -- * Destructuring the Response
    UpdateDevEndpointResponse (..),
    newUpdateDevEndpointResponse,

    -- * Response Lenses
    updateDevEndpointResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.Glue.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateDevEndpoint' smart constructor.
data UpdateDevEndpoint = UpdateDevEndpoint'
  { -- | The list of public keys for the @DevEndpoint@ to use.
    addPublicKeys :: Prelude.Maybe [Prelude.Text],
    -- | Custom Python or Java libraries to be loaded in the @DevEndpoint@.
    customLibraries :: Prelude.Maybe DevEndpointCustomLibraries,
    -- | The public key for the @DevEndpoint@ to use.
    publicKey :: Prelude.Maybe Prelude.Text,
    -- | The list of argument keys to be deleted from the map of arguments used
    -- to configure the @DevEndpoint@.
    deleteArguments :: Prelude.Maybe [Prelude.Text],
    -- | The list of public keys to be deleted from the @DevEndpoint@.
    deletePublicKeys :: Prelude.Maybe [Prelude.Text],
    -- | @True@ if the list of custom libraries to be loaded in the development
    -- endpoint needs to be updated, or @False@ if otherwise.
    updateEtlLibraries :: Prelude.Maybe Prelude.Bool,
    -- | The map of arguments to add the map of arguments used to configure the
    -- @DevEndpoint@.
    --
    -- Valid arguments are:
    --
    -- -   @\"--enable-glue-datacatalog\": \"\"@
    --
    -- You can specify a version of Python support for development endpoints by
    -- using the @Arguments@ parameter in the @CreateDevEndpoint@ or
    -- @UpdateDevEndpoint@ APIs. If no arguments are provided, the version
    -- defaults to Python 2.
    addArguments :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the @DevEndpoint@ to be updated.
    endpointName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDevEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addPublicKeys', 'updateDevEndpoint_addPublicKeys' - The list of public keys for the @DevEndpoint@ to use.
--
-- 'customLibraries', 'updateDevEndpoint_customLibraries' - Custom Python or Java libraries to be loaded in the @DevEndpoint@.
--
-- 'publicKey', 'updateDevEndpoint_publicKey' - The public key for the @DevEndpoint@ to use.
--
-- 'deleteArguments', 'updateDevEndpoint_deleteArguments' - The list of argument keys to be deleted from the map of arguments used
-- to configure the @DevEndpoint@.
--
-- 'deletePublicKeys', 'updateDevEndpoint_deletePublicKeys' - The list of public keys to be deleted from the @DevEndpoint@.
--
-- 'updateEtlLibraries', 'updateDevEndpoint_updateEtlLibraries' - @True@ if the list of custom libraries to be loaded in the development
-- endpoint needs to be updated, or @False@ if otherwise.
--
-- 'addArguments', 'updateDevEndpoint_addArguments' - The map of arguments to add the map of arguments used to configure the
-- @DevEndpoint@.
--
-- Valid arguments are:
--
-- -   @\"--enable-glue-datacatalog\": \"\"@
--
-- You can specify a version of Python support for development endpoints by
-- using the @Arguments@ parameter in the @CreateDevEndpoint@ or
-- @UpdateDevEndpoint@ APIs. If no arguments are provided, the version
-- defaults to Python 2.
--
-- 'endpointName', 'updateDevEndpoint_endpointName' - The name of the @DevEndpoint@ to be updated.
newUpdateDevEndpoint ::
  -- | 'endpointName'
  Prelude.Text ->
  UpdateDevEndpoint
newUpdateDevEndpoint pEndpointName_ =
  UpdateDevEndpoint'
    { addPublicKeys = Prelude.Nothing,
      customLibraries = Prelude.Nothing,
      publicKey = Prelude.Nothing,
      deleteArguments = Prelude.Nothing,
      deletePublicKeys = Prelude.Nothing,
      updateEtlLibraries = Prelude.Nothing,
      addArguments = Prelude.Nothing,
      endpointName = pEndpointName_
    }

-- | The list of public keys for the @DevEndpoint@ to use.
updateDevEndpoint_addPublicKeys :: Lens.Lens' UpdateDevEndpoint (Prelude.Maybe [Prelude.Text])
updateDevEndpoint_addPublicKeys = Lens.lens (\UpdateDevEndpoint' {addPublicKeys} -> addPublicKeys) (\s@UpdateDevEndpoint' {} a -> s {addPublicKeys = a} :: UpdateDevEndpoint) Prelude.. Lens.mapping Lens.coerced

-- | Custom Python or Java libraries to be loaded in the @DevEndpoint@.
updateDevEndpoint_customLibraries :: Lens.Lens' UpdateDevEndpoint (Prelude.Maybe DevEndpointCustomLibraries)
updateDevEndpoint_customLibraries = Lens.lens (\UpdateDevEndpoint' {customLibraries} -> customLibraries) (\s@UpdateDevEndpoint' {} a -> s {customLibraries = a} :: UpdateDevEndpoint)

-- | The public key for the @DevEndpoint@ to use.
updateDevEndpoint_publicKey :: Lens.Lens' UpdateDevEndpoint (Prelude.Maybe Prelude.Text)
updateDevEndpoint_publicKey = Lens.lens (\UpdateDevEndpoint' {publicKey} -> publicKey) (\s@UpdateDevEndpoint' {} a -> s {publicKey = a} :: UpdateDevEndpoint)

-- | The list of argument keys to be deleted from the map of arguments used
-- to configure the @DevEndpoint@.
updateDevEndpoint_deleteArguments :: Lens.Lens' UpdateDevEndpoint (Prelude.Maybe [Prelude.Text])
updateDevEndpoint_deleteArguments = Lens.lens (\UpdateDevEndpoint' {deleteArguments} -> deleteArguments) (\s@UpdateDevEndpoint' {} a -> s {deleteArguments = a} :: UpdateDevEndpoint) Prelude.. Lens.mapping Lens.coerced

-- | The list of public keys to be deleted from the @DevEndpoint@.
updateDevEndpoint_deletePublicKeys :: Lens.Lens' UpdateDevEndpoint (Prelude.Maybe [Prelude.Text])
updateDevEndpoint_deletePublicKeys = Lens.lens (\UpdateDevEndpoint' {deletePublicKeys} -> deletePublicKeys) (\s@UpdateDevEndpoint' {} a -> s {deletePublicKeys = a} :: UpdateDevEndpoint) Prelude.. Lens.mapping Lens.coerced

-- | @True@ if the list of custom libraries to be loaded in the development
-- endpoint needs to be updated, or @False@ if otherwise.
updateDevEndpoint_updateEtlLibraries :: Lens.Lens' UpdateDevEndpoint (Prelude.Maybe Prelude.Bool)
updateDevEndpoint_updateEtlLibraries = Lens.lens (\UpdateDevEndpoint' {updateEtlLibraries} -> updateEtlLibraries) (\s@UpdateDevEndpoint' {} a -> s {updateEtlLibraries = a} :: UpdateDevEndpoint)

-- | The map of arguments to add the map of arguments used to configure the
-- @DevEndpoint@.
--
-- Valid arguments are:
--
-- -   @\"--enable-glue-datacatalog\": \"\"@
--
-- You can specify a version of Python support for development endpoints by
-- using the @Arguments@ parameter in the @CreateDevEndpoint@ or
-- @UpdateDevEndpoint@ APIs. If no arguments are provided, the version
-- defaults to Python 2.
updateDevEndpoint_addArguments :: Lens.Lens' UpdateDevEndpoint (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateDevEndpoint_addArguments = Lens.lens (\UpdateDevEndpoint' {addArguments} -> addArguments) (\s@UpdateDevEndpoint' {} a -> s {addArguments = a} :: UpdateDevEndpoint) Prelude.. Lens.mapping Lens.coerced

-- | The name of the @DevEndpoint@ to be updated.
updateDevEndpoint_endpointName :: Lens.Lens' UpdateDevEndpoint Prelude.Text
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
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateDevEndpoint

instance Prelude.NFData UpdateDevEndpoint

instance Core.ToHeaders UpdateDevEndpoint where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.UpdateDevEndpoint" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateDevEndpoint where
  toJSON UpdateDevEndpoint' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AddPublicKeys" Core..=) Prelude.<$> addPublicKeys,
            ("CustomLibraries" Core..=)
              Prelude.<$> customLibraries,
            ("PublicKey" Core..=) Prelude.<$> publicKey,
            ("DeleteArguments" Core..=)
              Prelude.<$> deleteArguments,
            ("DeletePublicKeys" Core..=)
              Prelude.<$> deletePublicKeys,
            ("UpdateEtlLibraries" Core..=)
              Prelude.<$> updateEtlLibraries,
            ("AddArguments" Core..=) Prelude.<$> addArguments,
            Prelude.Just ("EndpointName" Core..= endpointName)
          ]
      )

instance Core.ToPath UpdateDevEndpoint where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateDevEndpoint where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDevEndpointResponse' smart constructor.
data UpdateDevEndpointResponse = UpdateDevEndpointResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  UpdateDevEndpointResponse
newUpdateDevEndpointResponse pHttpStatus_ =
  UpdateDevEndpointResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateDevEndpointResponse_httpStatus :: Lens.Lens' UpdateDevEndpointResponse Prelude.Int
updateDevEndpointResponse_httpStatus = Lens.lens (\UpdateDevEndpointResponse' {httpStatus} -> httpStatus) (\s@UpdateDevEndpointResponse' {} a -> s {httpStatus = a} :: UpdateDevEndpointResponse)

instance Prelude.NFData UpdateDevEndpointResponse
