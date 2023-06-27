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
-- Module      : Amazonka.ElasticBeanstalk.UpdateApplicationVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified application version to have the specified
-- properties.
--
-- If a property (for example, @description@) is not provided, the value
-- remains unchanged. To clear properties, specify an empty string.
module Amazonka.ElasticBeanstalk.UpdateApplicationVersion
  ( -- * Creating a Request
    UpdateApplicationVersion (..),
    newUpdateApplicationVersion,

    -- * Request Lenses
    updateApplicationVersion_description,
    updateApplicationVersion_applicationName,
    updateApplicationVersion_versionLabel,

    -- * Destructuring the Response
    ApplicationVersionDescriptionMessage (..),
    newApplicationVersionDescriptionMessage,

    -- * Response Lenses
    applicationVersionDescriptionMessage_applicationVersion,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticBeanstalk.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newUpdateApplicationVersion' smart constructor.
data UpdateApplicationVersion = UpdateApplicationVersion'
  { -- | A new description for this version.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the application associated with this version.
    --
    -- If no application is found with this name, @UpdateApplication@ returns
    -- an @InvalidParameterValue@ error.
    applicationName :: Prelude.Text,
    -- | The name of the version to update.
    --
    -- If no application version is found with this label, @UpdateApplication@
    -- returns an @InvalidParameterValue@ error.
    versionLabel :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateApplicationVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateApplicationVersion_description' - A new description for this version.
--
-- 'applicationName', 'updateApplicationVersion_applicationName' - The name of the application associated with this version.
--
-- If no application is found with this name, @UpdateApplication@ returns
-- an @InvalidParameterValue@ error.
--
-- 'versionLabel', 'updateApplicationVersion_versionLabel' - The name of the version to update.
--
-- If no application version is found with this label, @UpdateApplication@
-- returns an @InvalidParameterValue@ error.
newUpdateApplicationVersion ::
  -- | 'applicationName'
  Prelude.Text ->
  -- | 'versionLabel'
  Prelude.Text ->
  UpdateApplicationVersion
newUpdateApplicationVersion
  pApplicationName_
  pVersionLabel_ =
    UpdateApplicationVersion'
      { description =
          Prelude.Nothing,
        applicationName = pApplicationName_,
        versionLabel = pVersionLabel_
      }

-- | A new description for this version.
updateApplicationVersion_description :: Lens.Lens' UpdateApplicationVersion (Prelude.Maybe Prelude.Text)
updateApplicationVersion_description = Lens.lens (\UpdateApplicationVersion' {description} -> description) (\s@UpdateApplicationVersion' {} a -> s {description = a} :: UpdateApplicationVersion)

-- | The name of the application associated with this version.
--
-- If no application is found with this name, @UpdateApplication@ returns
-- an @InvalidParameterValue@ error.
updateApplicationVersion_applicationName :: Lens.Lens' UpdateApplicationVersion Prelude.Text
updateApplicationVersion_applicationName = Lens.lens (\UpdateApplicationVersion' {applicationName} -> applicationName) (\s@UpdateApplicationVersion' {} a -> s {applicationName = a} :: UpdateApplicationVersion)

-- | The name of the version to update.
--
-- If no application version is found with this label, @UpdateApplication@
-- returns an @InvalidParameterValue@ error.
updateApplicationVersion_versionLabel :: Lens.Lens' UpdateApplicationVersion Prelude.Text
updateApplicationVersion_versionLabel = Lens.lens (\UpdateApplicationVersion' {versionLabel} -> versionLabel) (\s@UpdateApplicationVersion' {} a -> s {versionLabel = a} :: UpdateApplicationVersion)

instance Core.AWSRequest UpdateApplicationVersion where
  type
    AWSResponse UpdateApplicationVersion =
      ApplicationVersionDescriptionMessage
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "UpdateApplicationVersionResult"
      (\s h x -> Data.parseXML x)

instance Prelude.Hashable UpdateApplicationVersion where
  hashWithSalt _salt UpdateApplicationVersion' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` applicationName
      `Prelude.hashWithSalt` versionLabel

instance Prelude.NFData UpdateApplicationVersion where
  rnf UpdateApplicationVersion' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf applicationName
      `Prelude.seq` Prelude.rnf versionLabel

instance Data.ToHeaders UpdateApplicationVersion where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath UpdateApplicationVersion where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateApplicationVersion where
  toQuery UpdateApplicationVersion' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("UpdateApplicationVersion" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-12-01" :: Prelude.ByteString),
        "Description" Data.=: description,
        "ApplicationName" Data.=: applicationName,
        "VersionLabel" Data.=: versionLabel
      ]
