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
-- Module      : Network.AWS.CodeDeploy.RegisterApplicationRevision
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers with AWS CodeDeploy a revision for the specified application.
module Network.AWS.CodeDeploy.RegisterApplicationRevision
  ( -- * Creating a Request
    RegisterApplicationRevision (..),
    newRegisterApplicationRevision,

    -- * Request Lenses
    registerApplicationRevision_description,
    registerApplicationRevision_applicationName,
    registerApplicationRevision_revision,

    -- * Destructuring the Response
    RegisterApplicationRevisionResponse (..),
    newRegisterApplicationRevisionResponse,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a RegisterApplicationRevision operation.
--
-- /See:/ 'newRegisterApplicationRevision' smart constructor.
data RegisterApplicationRevision = RegisterApplicationRevision'
  { -- | A comment about the revision.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of an AWS CodeDeploy application associated with the IAM user
    -- or AWS account.
    applicationName :: Prelude.Text,
    -- | Information about the application revision to register, including type
    -- and location.
    revision :: RevisionLocation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RegisterApplicationRevision' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'registerApplicationRevision_description' - A comment about the revision.
--
-- 'applicationName', 'registerApplicationRevision_applicationName' - The name of an AWS CodeDeploy application associated with the IAM user
-- or AWS account.
--
-- 'revision', 'registerApplicationRevision_revision' - Information about the application revision to register, including type
-- and location.
newRegisterApplicationRevision ::
  -- | 'applicationName'
  Prelude.Text ->
  -- | 'revision'
  RevisionLocation ->
  RegisterApplicationRevision
newRegisterApplicationRevision
  pApplicationName_
  pRevision_ =
    RegisterApplicationRevision'
      { description =
          Prelude.Nothing,
        applicationName = pApplicationName_,
        revision = pRevision_
      }

-- | A comment about the revision.
registerApplicationRevision_description :: Lens.Lens' RegisterApplicationRevision (Prelude.Maybe Prelude.Text)
registerApplicationRevision_description = Lens.lens (\RegisterApplicationRevision' {description} -> description) (\s@RegisterApplicationRevision' {} a -> s {description = a} :: RegisterApplicationRevision)

-- | The name of an AWS CodeDeploy application associated with the IAM user
-- or AWS account.
registerApplicationRevision_applicationName :: Lens.Lens' RegisterApplicationRevision Prelude.Text
registerApplicationRevision_applicationName = Lens.lens (\RegisterApplicationRevision' {applicationName} -> applicationName) (\s@RegisterApplicationRevision' {} a -> s {applicationName = a} :: RegisterApplicationRevision)

-- | Information about the application revision to register, including type
-- and location.
registerApplicationRevision_revision :: Lens.Lens' RegisterApplicationRevision RevisionLocation
registerApplicationRevision_revision = Lens.lens (\RegisterApplicationRevision' {revision} -> revision) (\s@RegisterApplicationRevision' {} a -> s {revision = a} :: RegisterApplicationRevision)

instance
  Prelude.AWSRequest
    RegisterApplicationRevision
  where
  type
    Rs RegisterApplicationRevision =
      RegisterApplicationRevisionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      RegisterApplicationRevisionResponse'

instance Prelude.Hashable RegisterApplicationRevision

instance Prelude.NFData RegisterApplicationRevision

instance
  Prelude.ToHeaders
    RegisterApplicationRevision
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CodeDeploy_20141006.RegisterApplicationRevision" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON RegisterApplicationRevision where
  toJSON RegisterApplicationRevision' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("description" Prelude..=) Prelude.<$> description,
            Prelude.Just
              ("applicationName" Prelude..= applicationName),
            Prelude.Just ("revision" Prelude..= revision)
          ]
      )

instance Prelude.ToPath RegisterApplicationRevision where
  toPath = Prelude.const "/"

instance Prelude.ToQuery RegisterApplicationRevision where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRegisterApplicationRevisionResponse' smart constructor.
data RegisterApplicationRevisionResponse = RegisterApplicationRevisionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RegisterApplicationRevisionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newRegisterApplicationRevisionResponse ::
  RegisterApplicationRevisionResponse
newRegisterApplicationRevisionResponse =
  RegisterApplicationRevisionResponse'

instance
  Prelude.NFData
    RegisterApplicationRevisionResponse
