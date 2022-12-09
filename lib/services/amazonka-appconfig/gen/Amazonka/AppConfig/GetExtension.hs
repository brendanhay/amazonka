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
-- Module      : Amazonka.AppConfig.GetExtension
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about an AppConfig extension.
module Amazonka.AppConfig.GetExtension
  ( -- * Creating a Request
    GetExtension (..),
    newGetExtension,

    -- * Request Lenses
    getExtension_versionNumber,
    getExtension_extensionIdentifier,

    -- * Destructuring the Response
    Extension (..),
    newExtension,

    -- * Response Lenses
    extension_actions,
    extension_arn,
    extension_description,
    extension_id,
    extension_name,
    extension_parameters,
    extension_versionNumber,
  )
where

import Amazonka.AppConfig.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetExtension' smart constructor.
data GetExtension = GetExtension'
  { -- | The extension version number. If no version number was defined,
    -- AppConfig uses the highest version.
    versionNumber :: Prelude.Maybe Prelude.Int,
    -- | The name, the ID, or the Amazon Resource Name (ARN) of the extension.
    extensionIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetExtension' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'versionNumber', 'getExtension_versionNumber' - The extension version number. If no version number was defined,
-- AppConfig uses the highest version.
--
-- 'extensionIdentifier', 'getExtension_extensionIdentifier' - The name, the ID, or the Amazon Resource Name (ARN) of the extension.
newGetExtension ::
  -- | 'extensionIdentifier'
  Prelude.Text ->
  GetExtension
newGetExtension pExtensionIdentifier_ =
  GetExtension'
    { versionNumber = Prelude.Nothing,
      extensionIdentifier = pExtensionIdentifier_
    }

-- | The extension version number. If no version number was defined,
-- AppConfig uses the highest version.
getExtension_versionNumber :: Lens.Lens' GetExtension (Prelude.Maybe Prelude.Int)
getExtension_versionNumber = Lens.lens (\GetExtension' {versionNumber} -> versionNumber) (\s@GetExtension' {} a -> s {versionNumber = a} :: GetExtension)

-- | The name, the ID, or the Amazon Resource Name (ARN) of the extension.
getExtension_extensionIdentifier :: Lens.Lens' GetExtension Prelude.Text
getExtension_extensionIdentifier = Lens.lens (\GetExtension' {extensionIdentifier} -> extensionIdentifier) (\s@GetExtension' {} a -> s {extensionIdentifier = a} :: GetExtension)

instance Core.AWSRequest GetExtension where
  type AWSResponse GetExtension = Extension
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable GetExtension where
  hashWithSalt _salt GetExtension' {..} =
    _salt `Prelude.hashWithSalt` versionNumber
      `Prelude.hashWithSalt` extensionIdentifier

instance Prelude.NFData GetExtension where
  rnf GetExtension' {..} =
    Prelude.rnf versionNumber
      `Prelude.seq` Prelude.rnf extensionIdentifier

instance Data.ToHeaders GetExtension where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetExtension where
  toPath GetExtension' {..} =
    Prelude.mconcat
      ["/extensions/", Data.toBS extensionIdentifier]

instance Data.ToQuery GetExtension where
  toQuery GetExtension' {..} =
    Prelude.mconcat
      ["version_number" Data.=: versionNumber]
