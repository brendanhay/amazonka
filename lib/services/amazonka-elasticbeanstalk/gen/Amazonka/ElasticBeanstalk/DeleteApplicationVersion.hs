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
-- Module      : Amazonka.ElasticBeanstalk.DeleteApplicationVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified version from the specified application.
--
-- You cannot delete an application version that is associated with a
-- running environment.
module Amazonka.ElasticBeanstalk.DeleteApplicationVersion
  ( -- * Creating a Request
    DeleteApplicationVersion (..),
    newDeleteApplicationVersion,

    -- * Request Lenses
    deleteApplicationVersion_deleteSourceBundle,
    deleteApplicationVersion_applicationName,
    deleteApplicationVersion_versionLabel,

    -- * Destructuring the Response
    DeleteApplicationVersionResponse (..),
    newDeleteApplicationVersionResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticBeanstalk.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Request to delete an application version.
--
-- /See:/ 'newDeleteApplicationVersion' smart constructor.
data DeleteApplicationVersion = DeleteApplicationVersion'
  { -- | Set to @true@ to delete the source bundle from your storage bucket.
    -- Otherwise, the application version is deleted only from Elastic
    -- Beanstalk and the source bundle remains in Amazon S3.
    deleteSourceBundle :: Prelude.Maybe Prelude.Bool,
    -- | The name of the application to which the version belongs.
    applicationName :: Prelude.Text,
    -- | The label of the version to delete.
    versionLabel :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteApplicationVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deleteSourceBundle', 'deleteApplicationVersion_deleteSourceBundle' - Set to @true@ to delete the source bundle from your storage bucket.
-- Otherwise, the application version is deleted only from Elastic
-- Beanstalk and the source bundle remains in Amazon S3.
--
-- 'applicationName', 'deleteApplicationVersion_applicationName' - The name of the application to which the version belongs.
--
-- 'versionLabel', 'deleteApplicationVersion_versionLabel' - The label of the version to delete.
newDeleteApplicationVersion ::
  -- | 'applicationName'
  Prelude.Text ->
  -- | 'versionLabel'
  Prelude.Text ->
  DeleteApplicationVersion
newDeleteApplicationVersion
  pApplicationName_
  pVersionLabel_ =
    DeleteApplicationVersion'
      { deleteSourceBundle =
          Prelude.Nothing,
        applicationName = pApplicationName_,
        versionLabel = pVersionLabel_
      }

-- | Set to @true@ to delete the source bundle from your storage bucket.
-- Otherwise, the application version is deleted only from Elastic
-- Beanstalk and the source bundle remains in Amazon S3.
deleteApplicationVersion_deleteSourceBundle :: Lens.Lens' DeleteApplicationVersion (Prelude.Maybe Prelude.Bool)
deleteApplicationVersion_deleteSourceBundle = Lens.lens (\DeleteApplicationVersion' {deleteSourceBundle} -> deleteSourceBundle) (\s@DeleteApplicationVersion' {} a -> s {deleteSourceBundle = a} :: DeleteApplicationVersion)

-- | The name of the application to which the version belongs.
deleteApplicationVersion_applicationName :: Lens.Lens' DeleteApplicationVersion Prelude.Text
deleteApplicationVersion_applicationName = Lens.lens (\DeleteApplicationVersion' {applicationName} -> applicationName) (\s@DeleteApplicationVersion' {} a -> s {applicationName = a} :: DeleteApplicationVersion)

-- | The label of the version to delete.
deleteApplicationVersion_versionLabel :: Lens.Lens' DeleteApplicationVersion Prelude.Text
deleteApplicationVersion_versionLabel = Lens.lens (\DeleteApplicationVersion' {versionLabel} -> versionLabel) (\s@DeleteApplicationVersion' {} a -> s {versionLabel = a} :: DeleteApplicationVersion)

instance Core.AWSRequest DeleteApplicationVersion where
  type
    AWSResponse DeleteApplicationVersion =
      DeleteApplicationVersionResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull
      DeleteApplicationVersionResponse'

instance Prelude.Hashable DeleteApplicationVersion where
  hashWithSalt _salt DeleteApplicationVersion' {..} =
    _salt
      `Prelude.hashWithSalt` deleteSourceBundle
      `Prelude.hashWithSalt` applicationName
      `Prelude.hashWithSalt` versionLabel

instance Prelude.NFData DeleteApplicationVersion where
  rnf DeleteApplicationVersion' {..} =
    Prelude.rnf deleteSourceBundle `Prelude.seq`
      Prelude.rnf applicationName `Prelude.seq`
        Prelude.rnf versionLabel

instance Data.ToHeaders DeleteApplicationVersion where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteApplicationVersion where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteApplicationVersion where
  toQuery DeleteApplicationVersion' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeleteApplicationVersion" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-12-01" :: Prelude.ByteString),
        "DeleteSourceBundle" Data.=: deleteSourceBundle,
        "ApplicationName" Data.=: applicationName,
        "VersionLabel" Data.=: versionLabel
      ]

-- | /See:/ 'newDeleteApplicationVersionResponse' smart constructor.
data DeleteApplicationVersionResponse = DeleteApplicationVersionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteApplicationVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteApplicationVersionResponse ::
  DeleteApplicationVersionResponse
newDeleteApplicationVersionResponse =
  DeleteApplicationVersionResponse'

instance
  Prelude.NFData
    DeleteApplicationVersionResponse
  where
  rnf _ = ()
