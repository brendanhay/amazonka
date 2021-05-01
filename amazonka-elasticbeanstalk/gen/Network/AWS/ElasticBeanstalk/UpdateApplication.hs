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
-- Module      : Network.AWS.ElasticBeanstalk.UpdateApplication
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified application to have the specified properties.
--
-- If a property (for example, @description@) is not provided, the value
-- remains unchanged. To clear these properties, specify an empty string.
module Network.AWS.ElasticBeanstalk.UpdateApplication
  ( -- * Creating a Request
    UpdateApplication (..),
    newUpdateApplication,

    -- * Request Lenses
    updateApplication_description,
    updateApplication_applicationName,

    -- * Destructuring the Response
    ApplicationDescriptionMessage (..),
    newApplicationDescriptionMessage,

    -- * Response Lenses
    applicationDescriptionMessage_application,
  )
where

import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to update an application.
--
-- /See:/ 'newUpdateApplication' smart constructor.
data UpdateApplication = UpdateApplication'
  { -- | A new description for the application.
    --
    -- Default: If not specified, AWS Elastic Beanstalk does not update the
    -- description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the application to update. If no such application is found,
    -- @UpdateApplication@ returns an @InvalidParameterValue@ error.
    applicationName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateApplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateApplication_description' - A new description for the application.
--
-- Default: If not specified, AWS Elastic Beanstalk does not update the
-- description.
--
-- 'applicationName', 'updateApplication_applicationName' - The name of the application to update. If no such application is found,
-- @UpdateApplication@ returns an @InvalidParameterValue@ error.
newUpdateApplication ::
  -- | 'applicationName'
  Prelude.Text ->
  UpdateApplication
newUpdateApplication pApplicationName_ =
  UpdateApplication'
    { description = Prelude.Nothing,
      applicationName = pApplicationName_
    }

-- | A new description for the application.
--
-- Default: If not specified, AWS Elastic Beanstalk does not update the
-- description.
updateApplication_description :: Lens.Lens' UpdateApplication (Prelude.Maybe Prelude.Text)
updateApplication_description = Lens.lens (\UpdateApplication' {description} -> description) (\s@UpdateApplication' {} a -> s {description = a} :: UpdateApplication)

-- | The name of the application to update. If no such application is found,
-- @UpdateApplication@ returns an @InvalidParameterValue@ error.
updateApplication_applicationName :: Lens.Lens' UpdateApplication Prelude.Text
updateApplication_applicationName = Lens.lens (\UpdateApplication' {applicationName} -> applicationName) (\s@UpdateApplication' {} a -> s {applicationName = a} :: UpdateApplication)

instance Prelude.AWSRequest UpdateApplication where
  type
    Rs UpdateApplication =
      ApplicationDescriptionMessage
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "UpdateApplicationResult"
      (\s h x -> Prelude.parseXML x)

instance Prelude.Hashable UpdateApplication

instance Prelude.NFData UpdateApplication

instance Prelude.ToHeaders UpdateApplication where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath UpdateApplication where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateApplication where
  toQuery UpdateApplication' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("UpdateApplication" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-12-01" :: Prelude.ByteString),
        "Description" Prelude.=: description,
        "ApplicationName" Prelude.=: applicationName
      ]
