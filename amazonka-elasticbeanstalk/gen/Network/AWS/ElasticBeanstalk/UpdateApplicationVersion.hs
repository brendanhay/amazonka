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
-- Module      : Network.AWS.ElasticBeanstalk.UpdateApplicationVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.ElasticBeanstalk.UpdateApplicationVersion
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

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "UpdateApplicationVersionResult"
      (\s h x -> Core.parseXML x)

instance Prelude.Hashable UpdateApplicationVersion

instance Prelude.NFData UpdateApplicationVersion

instance Core.ToHeaders UpdateApplicationVersion where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath UpdateApplicationVersion where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateApplicationVersion where
  toQuery UpdateApplicationVersion' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("UpdateApplicationVersion" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-12-01" :: Prelude.ByteString),
        "Description" Core.=: description,
        "ApplicationName" Core.=: applicationName,
        "VersionLabel" Core.=: versionLabel
      ]
