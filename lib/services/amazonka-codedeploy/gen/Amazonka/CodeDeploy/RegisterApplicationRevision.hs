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
-- Module      : Amazonka.CodeDeploy.RegisterApplicationRevision
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers with CodeDeploy a revision for the specified application.
module Amazonka.CodeDeploy.RegisterApplicationRevision
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

import Amazonka.CodeDeploy.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a RegisterApplicationRevision operation.
--
-- /See:/ 'newRegisterApplicationRevision' smart constructor.
data RegisterApplicationRevision = RegisterApplicationRevision'
  { -- | A comment about the revision.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of an CodeDeploy application associated with the IAM user or
    -- Amazon Web Services account.
    applicationName :: Prelude.Text,
    -- | Information about the application revision to register, including type
    -- and location.
    revision :: RevisionLocation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'applicationName', 'registerApplicationRevision_applicationName' - The name of an CodeDeploy application associated with the IAM user or
-- Amazon Web Services account.
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

-- | The name of an CodeDeploy application associated with the IAM user or
-- Amazon Web Services account.
registerApplicationRevision_applicationName :: Lens.Lens' RegisterApplicationRevision Prelude.Text
registerApplicationRevision_applicationName = Lens.lens (\RegisterApplicationRevision' {applicationName} -> applicationName) (\s@RegisterApplicationRevision' {} a -> s {applicationName = a} :: RegisterApplicationRevision)

-- | Information about the application revision to register, including type
-- and location.
registerApplicationRevision_revision :: Lens.Lens' RegisterApplicationRevision RevisionLocation
registerApplicationRevision_revision = Lens.lens (\RegisterApplicationRevision' {revision} -> revision) (\s@RegisterApplicationRevision' {} a -> s {revision = a} :: RegisterApplicationRevision)

instance Core.AWSRequest RegisterApplicationRevision where
  type
    AWSResponse RegisterApplicationRevision =
      RegisterApplicationRevisionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      RegisterApplicationRevisionResponse'

instance Prelude.Hashable RegisterApplicationRevision where
  hashWithSalt _salt RegisterApplicationRevision' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` applicationName
      `Prelude.hashWithSalt` revision

instance Prelude.NFData RegisterApplicationRevision where
  rnf RegisterApplicationRevision' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf applicationName
      `Prelude.seq` Prelude.rnf revision

instance Core.ToHeaders RegisterApplicationRevision where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeDeploy_20141006.RegisterApplicationRevision" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON RegisterApplicationRevision where
  toJSON RegisterApplicationRevision' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("description" Core..=) Prelude.<$> description,
            Prelude.Just
              ("applicationName" Core..= applicationName),
            Prelude.Just ("revision" Core..= revision)
          ]
      )

instance Core.ToPath RegisterApplicationRevision where
  toPath = Prelude.const "/"

instance Core.ToQuery RegisterApplicationRevision where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRegisterApplicationRevisionResponse' smart constructor.
data RegisterApplicationRevisionResponse = RegisterApplicationRevisionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf _ = ()
