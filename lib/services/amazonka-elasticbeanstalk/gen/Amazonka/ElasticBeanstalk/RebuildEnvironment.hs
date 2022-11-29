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
-- Module      : Amazonka.ElasticBeanstalk.RebuildEnvironment
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes and recreates all of the AWS resources (for example: the Auto
-- Scaling group, load balancer, etc.) for a specified environment and
-- forces a restart.
module Amazonka.ElasticBeanstalk.RebuildEnvironment
  ( -- * Creating a Request
    RebuildEnvironment (..),
    newRebuildEnvironment,

    -- * Request Lenses
    rebuildEnvironment_environmentName,
    rebuildEnvironment_environmentId,

    -- * Destructuring the Response
    RebuildEnvironmentResponse (..),
    newRebuildEnvironmentResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ElasticBeanstalk.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newRebuildEnvironment' smart constructor.
data RebuildEnvironment = RebuildEnvironment'
  { -- | The name of the environment to rebuild.
    --
    -- Condition: You must specify either this or an EnvironmentId, or both. If
    -- you do not specify either, AWS Elastic Beanstalk returns
    -- @MissingRequiredParameter@ error.
    environmentName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the environment to rebuild.
    --
    -- Condition: You must specify either this or an EnvironmentName, or both.
    -- If you do not specify either, AWS Elastic Beanstalk returns
    -- @MissingRequiredParameter@ error.
    environmentId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RebuildEnvironment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'environmentName', 'rebuildEnvironment_environmentName' - The name of the environment to rebuild.
--
-- Condition: You must specify either this or an EnvironmentId, or both. If
-- you do not specify either, AWS Elastic Beanstalk returns
-- @MissingRequiredParameter@ error.
--
-- 'environmentId', 'rebuildEnvironment_environmentId' - The ID of the environment to rebuild.
--
-- Condition: You must specify either this or an EnvironmentName, or both.
-- If you do not specify either, AWS Elastic Beanstalk returns
-- @MissingRequiredParameter@ error.
newRebuildEnvironment ::
  RebuildEnvironment
newRebuildEnvironment =
  RebuildEnvironment'
    { environmentName =
        Prelude.Nothing,
      environmentId = Prelude.Nothing
    }

-- | The name of the environment to rebuild.
--
-- Condition: You must specify either this or an EnvironmentId, or both. If
-- you do not specify either, AWS Elastic Beanstalk returns
-- @MissingRequiredParameter@ error.
rebuildEnvironment_environmentName :: Lens.Lens' RebuildEnvironment (Prelude.Maybe Prelude.Text)
rebuildEnvironment_environmentName = Lens.lens (\RebuildEnvironment' {environmentName} -> environmentName) (\s@RebuildEnvironment' {} a -> s {environmentName = a} :: RebuildEnvironment)

-- | The ID of the environment to rebuild.
--
-- Condition: You must specify either this or an EnvironmentName, or both.
-- If you do not specify either, AWS Elastic Beanstalk returns
-- @MissingRequiredParameter@ error.
rebuildEnvironment_environmentId :: Lens.Lens' RebuildEnvironment (Prelude.Maybe Prelude.Text)
rebuildEnvironment_environmentId = Lens.lens (\RebuildEnvironment' {environmentId} -> environmentId) (\s@RebuildEnvironment' {} a -> s {environmentId = a} :: RebuildEnvironment)

instance Core.AWSRequest RebuildEnvironment where
  type
    AWSResponse RebuildEnvironment =
      RebuildEnvironmentResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull RebuildEnvironmentResponse'

instance Prelude.Hashable RebuildEnvironment where
  hashWithSalt _salt RebuildEnvironment' {..} =
    _salt `Prelude.hashWithSalt` environmentName
      `Prelude.hashWithSalt` environmentId

instance Prelude.NFData RebuildEnvironment where
  rnf RebuildEnvironment' {..} =
    Prelude.rnf environmentName
      `Prelude.seq` Prelude.rnf environmentId

instance Core.ToHeaders RebuildEnvironment where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath RebuildEnvironment where
  toPath = Prelude.const "/"

instance Core.ToQuery RebuildEnvironment where
  toQuery RebuildEnvironment' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("RebuildEnvironment" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-12-01" :: Prelude.ByteString),
        "EnvironmentName" Core.=: environmentName,
        "EnvironmentId" Core.=: environmentId
      ]

-- | /See:/ 'newRebuildEnvironmentResponse' smart constructor.
data RebuildEnvironmentResponse = RebuildEnvironmentResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RebuildEnvironmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newRebuildEnvironmentResponse ::
  RebuildEnvironmentResponse
newRebuildEnvironmentResponse =
  RebuildEnvironmentResponse'

instance Prelude.NFData RebuildEnvironmentResponse where
  rnf _ = ()
