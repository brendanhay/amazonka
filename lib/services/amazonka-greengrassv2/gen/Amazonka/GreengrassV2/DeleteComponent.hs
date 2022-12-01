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
-- Module      : Amazonka.GreengrassV2.DeleteComponent
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a version of a component from IoT Greengrass.
--
-- This operation deletes the component\'s recipe and artifacts. As a
-- result, deployments that refer to this component version will fail. If
-- you have deployments that use this component version, you can remove the
-- component from the deployment or update the deployment to use a valid
-- version.
module Amazonka.GreengrassV2.DeleteComponent
  ( -- * Creating a Request
    DeleteComponent (..),
    newDeleteComponent,

    -- * Request Lenses
    deleteComponent_arn,

    -- * Destructuring the Response
    DeleteComponentResponse (..),
    newDeleteComponentResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GreengrassV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteComponent' smart constructor.
data DeleteComponent = DeleteComponent'
  { -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
    -- of the component version.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteComponent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'deleteComponent_arn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the component version.
newDeleteComponent ::
  -- | 'arn'
  Prelude.Text ->
  DeleteComponent
newDeleteComponent pArn_ =
  DeleteComponent' {arn = pArn_}

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the component version.
deleteComponent_arn :: Lens.Lens' DeleteComponent Prelude.Text
deleteComponent_arn = Lens.lens (\DeleteComponent' {arn} -> arn) (\s@DeleteComponent' {} a -> s {arn = a} :: DeleteComponent)

instance Core.AWSRequest DeleteComponent where
  type
    AWSResponse DeleteComponent =
      DeleteComponentResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteComponentResponse'

instance Prelude.Hashable DeleteComponent where
  hashWithSalt _salt DeleteComponent' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData DeleteComponent where
  rnf DeleteComponent' {..} = Prelude.rnf arn

instance Core.ToHeaders DeleteComponent where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DeleteComponent where
  toPath DeleteComponent' {..} =
    Prelude.mconcat
      ["/greengrass/v2/components/", Core.toBS arn]

instance Core.ToQuery DeleteComponent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteComponentResponse' smart constructor.
data DeleteComponentResponse = DeleteComponentResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteComponentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteComponentResponse ::
  DeleteComponentResponse
newDeleteComponentResponse = DeleteComponentResponse'

instance Prelude.NFData DeleteComponentResponse where
  rnf _ = ()
