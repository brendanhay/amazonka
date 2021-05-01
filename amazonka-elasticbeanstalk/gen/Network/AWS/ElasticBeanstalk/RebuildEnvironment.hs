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
-- Module      : Network.AWS.ElasticBeanstalk.RebuildEnvironment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes and recreates all of the AWS resources (for example: the Auto
-- Scaling group, load balancer, etc.) for a specified environment and
-- forces a restart.
module Network.AWS.ElasticBeanstalk.RebuildEnvironment
  ( -- * Creating a Request
    RebuildEnvironment (..),
    newRebuildEnvironment,

    -- * Request Lenses
    rebuildEnvironment_environmentId,
    rebuildEnvironment_environmentName,

    -- * Destructuring the Response
    RebuildEnvironmentResponse (..),
    newRebuildEnvironmentResponse,
  )
where

import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newRebuildEnvironment' smart constructor.
data RebuildEnvironment = RebuildEnvironment'
  { -- | The ID of the environment to rebuild.
    --
    -- Condition: You must specify either this or an EnvironmentName, or both.
    -- If you do not specify either, AWS Elastic Beanstalk returns
    -- @MissingRequiredParameter@ error.
    environmentId :: Prelude.Maybe Prelude.Text,
    -- | The name of the environment to rebuild.
    --
    -- Condition: You must specify either this or an EnvironmentId, or both. If
    -- you do not specify either, AWS Elastic Beanstalk returns
    -- @MissingRequiredParameter@ error.
    environmentName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RebuildEnvironment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'environmentId', 'rebuildEnvironment_environmentId' - The ID of the environment to rebuild.
--
-- Condition: You must specify either this or an EnvironmentName, or both.
-- If you do not specify either, AWS Elastic Beanstalk returns
-- @MissingRequiredParameter@ error.
--
-- 'environmentName', 'rebuildEnvironment_environmentName' - The name of the environment to rebuild.
--
-- Condition: You must specify either this or an EnvironmentId, or both. If
-- you do not specify either, AWS Elastic Beanstalk returns
-- @MissingRequiredParameter@ error.
newRebuildEnvironment ::
  RebuildEnvironment
newRebuildEnvironment =
  RebuildEnvironment'
    { environmentId =
        Prelude.Nothing,
      environmentName = Prelude.Nothing
    }

-- | The ID of the environment to rebuild.
--
-- Condition: You must specify either this or an EnvironmentName, or both.
-- If you do not specify either, AWS Elastic Beanstalk returns
-- @MissingRequiredParameter@ error.
rebuildEnvironment_environmentId :: Lens.Lens' RebuildEnvironment (Prelude.Maybe Prelude.Text)
rebuildEnvironment_environmentId = Lens.lens (\RebuildEnvironment' {environmentId} -> environmentId) (\s@RebuildEnvironment' {} a -> s {environmentId = a} :: RebuildEnvironment)

-- | The name of the environment to rebuild.
--
-- Condition: You must specify either this or an EnvironmentId, or both. If
-- you do not specify either, AWS Elastic Beanstalk returns
-- @MissingRequiredParameter@ error.
rebuildEnvironment_environmentName :: Lens.Lens' RebuildEnvironment (Prelude.Maybe Prelude.Text)
rebuildEnvironment_environmentName = Lens.lens (\RebuildEnvironment' {environmentName} -> environmentName) (\s@RebuildEnvironment' {} a -> s {environmentName = a} :: RebuildEnvironment)

instance Prelude.AWSRequest RebuildEnvironment where
  type
    Rs RebuildEnvironment =
      RebuildEnvironmentResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull RebuildEnvironmentResponse'

instance Prelude.Hashable RebuildEnvironment

instance Prelude.NFData RebuildEnvironment

instance Prelude.ToHeaders RebuildEnvironment where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath RebuildEnvironment where
  toPath = Prelude.const "/"

instance Prelude.ToQuery RebuildEnvironment where
  toQuery RebuildEnvironment' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("RebuildEnvironment" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-12-01" :: Prelude.ByteString),
        "EnvironmentId" Prelude.=: environmentId,
        "EnvironmentName" Prelude.=: environmentName
      ]

-- | /See:/ 'newRebuildEnvironmentResponse' smart constructor.
data RebuildEnvironmentResponse = RebuildEnvironmentResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RebuildEnvironmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newRebuildEnvironmentResponse ::
  RebuildEnvironmentResponse
newRebuildEnvironmentResponse =
  RebuildEnvironmentResponse'

instance Prelude.NFData RebuildEnvironmentResponse
