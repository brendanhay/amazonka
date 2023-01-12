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
-- Module      : Amazonka.Lambda.DeleteLayerVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a version of an
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html Lambda layer>.
-- Deleted versions can no longer be viewed or added to functions. To avoid
-- breaking functions, a copy of the version remains in Lambda until no
-- functions refer to it.
module Amazonka.Lambda.DeleteLayerVersion
  ( -- * Creating a Request
    DeleteLayerVersion (..),
    newDeleteLayerVersion,

    -- * Request Lenses
    deleteLayerVersion_layerName,
    deleteLayerVersion_versionNumber,

    -- * Destructuring the Response
    DeleteLayerVersionResponse (..),
    newDeleteLayerVersionResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lambda.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteLayerVersion' smart constructor.
data DeleteLayerVersion = DeleteLayerVersion'
  { -- | The name or Amazon Resource Name (ARN) of the layer.
    layerName :: Prelude.Text,
    -- | The version number.
    versionNumber :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteLayerVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'layerName', 'deleteLayerVersion_layerName' - The name or Amazon Resource Name (ARN) of the layer.
--
-- 'versionNumber', 'deleteLayerVersion_versionNumber' - The version number.
newDeleteLayerVersion ::
  -- | 'layerName'
  Prelude.Text ->
  -- | 'versionNumber'
  Prelude.Integer ->
  DeleteLayerVersion
newDeleteLayerVersion pLayerName_ pVersionNumber_ =
  DeleteLayerVersion'
    { layerName = pLayerName_,
      versionNumber = pVersionNumber_
    }

-- | The name or Amazon Resource Name (ARN) of the layer.
deleteLayerVersion_layerName :: Lens.Lens' DeleteLayerVersion Prelude.Text
deleteLayerVersion_layerName = Lens.lens (\DeleteLayerVersion' {layerName} -> layerName) (\s@DeleteLayerVersion' {} a -> s {layerName = a} :: DeleteLayerVersion)

-- | The version number.
deleteLayerVersion_versionNumber :: Lens.Lens' DeleteLayerVersion Prelude.Integer
deleteLayerVersion_versionNumber = Lens.lens (\DeleteLayerVersion' {versionNumber} -> versionNumber) (\s@DeleteLayerVersion' {} a -> s {versionNumber = a} :: DeleteLayerVersion)

instance Core.AWSRequest DeleteLayerVersion where
  type
    AWSResponse DeleteLayerVersion =
      DeleteLayerVersionResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteLayerVersionResponse'

instance Prelude.Hashable DeleteLayerVersion where
  hashWithSalt _salt DeleteLayerVersion' {..} =
    _salt `Prelude.hashWithSalt` layerName
      `Prelude.hashWithSalt` versionNumber

instance Prelude.NFData DeleteLayerVersion where
  rnf DeleteLayerVersion' {..} =
    Prelude.rnf layerName
      `Prelude.seq` Prelude.rnf versionNumber

instance Data.ToHeaders DeleteLayerVersion where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteLayerVersion where
  toPath DeleteLayerVersion' {..} =
    Prelude.mconcat
      [ "/2018-10-31/layers/",
        Data.toBS layerName,
        "/versions/",
        Data.toBS versionNumber
      ]

instance Data.ToQuery DeleteLayerVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteLayerVersionResponse' smart constructor.
data DeleteLayerVersionResponse = DeleteLayerVersionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteLayerVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteLayerVersionResponse ::
  DeleteLayerVersionResponse
newDeleteLayerVersionResponse =
  DeleteLayerVersionResponse'

instance Prelude.NFData DeleteLayerVersionResponse where
  rnf _ = ()
