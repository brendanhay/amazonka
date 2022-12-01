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
-- Module      : Amazonka.AppConfig.DeleteExtension
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an AppConfig extension. You must delete all associations to an
-- extension before you delete the extension.
module Amazonka.AppConfig.DeleteExtension
  ( -- * Creating a Request
    DeleteExtension (..),
    newDeleteExtension,

    -- * Request Lenses
    deleteExtension_versionNumber,
    deleteExtension_extensionIdentifier,

    -- * Destructuring the Response
    DeleteExtensionResponse (..),
    newDeleteExtensionResponse,
  )
where

import Amazonka.AppConfig.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteExtension' smart constructor.
data DeleteExtension = DeleteExtension'
  { -- | A specific version of an extension to delete. If omitted, the highest
    -- version is deleted.
    versionNumber :: Prelude.Maybe Prelude.Int,
    -- | The name, ID, or Amazon Resource Name (ARN) of the extension you want to
    -- delete.
    extensionIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteExtension' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'versionNumber', 'deleteExtension_versionNumber' - A specific version of an extension to delete. If omitted, the highest
-- version is deleted.
--
-- 'extensionIdentifier', 'deleteExtension_extensionIdentifier' - The name, ID, or Amazon Resource Name (ARN) of the extension you want to
-- delete.
newDeleteExtension ::
  -- | 'extensionIdentifier'
  Prelude.Text ->
  DeleteExtension
newDeleteExtension pExtensionIdentifier_ =
  DeleteExtension'
    { versionNumber = Prelude.Nothing,
      extensionIdentifier = pExtensionIdentifier_
    }

-- | A specific version of an extension to delete. If omitted, the highest
-- version is deleted.
deleteExtension_versionNumber :: Lens.Lens' DeleteExtension (Prelude.Maybe Prelude.Int)
deleteExtension_versionNumber = Lens.lens (\DeleteExtension' {versionNumber} -> versionNumber) (\s@DeleteExtension' {} a -> s {versionNumber = a} :: DeleteExtension)

-- | The name, ID, or Amazon Resource Name (ARN) of the extension you want to
-- delete.
deleteExtension_extensionIdentifier :: Lens.Lens' DeleteExtension Prelude.Text
deleteExtension_extensionIdentifier = Lens.lens (\DeleteExtension' {extensionIdentifier} -> extensionIdentifier) (\s@DeleteExtension' {} a -> s {extensionIdentifier = a} :: DeleteExtension)

instance Core.AWSRequest DeleteExtension where
  type
    AWSResponse DeleteExtension =
      DeleteExtensionResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteExtensionResponse'

instance Prelude.Hashable DeleteExtension where
  hashWithSalt _salt DeleteExtension' {..} =
    _salt `Prelude.hashWithSalt` versionNumber
      `Prelude.hashWithSalt` extensionIdentifier

instance Prelude.NFData DeleteExtension where
  rnf DeleteExtension' {..} =
    Prelude.rnf versionNumber
      `Prelude.seq` Prelude.rnf extensionIdentifier

instance Core.ToHeaders DeleteExtension where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteExtension where
  toPath DeleteExtension' {..} =
    Prelude.mconcat
      ["/extensions/", Core.toBS extensionIdentifier]

instance Core.ToQuery DeleteExtension where
  toQuery DeleteExtension' {..} =
    Prelude.mconcat ["version" Core.=: versionNumber]

-- | /See:/ 'newDeleteExtensionResponse' smart constructor.
data DeleteExtensionResponse = DeleteExtensionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteExtensionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteExtensionResponse ::
  DeleteExtensionResponse
newDeleteExtensionResponse = DeleteExtensionResponse'

instance Prelude.NFData DeleteExtensionResponse where
  rnf _ = ()
