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
-- Module      : Network.AWS.ElasticBeanstalk.AbortEnvironmentUpdate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels in-progress environment configuration update or application
-- version deployment.
module Network.AWS.ElasticBeanstalk.AbortEnvironmentUpdate
  ( -- * Creating a Request
    AbortEnvironmentUpdate (..),
    newAbortEnvironmentUpdate,

    -- * Request Lenses
    abortEnvironmentUpdate_environmentId,
    abortEnvironmentUpdate_environmentName,

    -- * Destructuring the Response
    AbortEnvironmentUpdateResponse (..),
    newAbortEnvironmentUpdateResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newAbortEnvironmentUpdate' smart constructor.
data AbortEnvironmentUpdate = AbortEnvironmentUpdate'
  { -- | This specifies the ID of the environment with the in-progress update
    -- that you want to cancel.
    environmentId :: Core.Maybe Core.Text,
    -- | This specifies the name of the environment with the in-progress update
    -- that you want to cancel.
    environmentName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AbortEnvironmentUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'environmentId', 'abortEnvironmentUpdate_environmentId' - This specifies the ID of the environment with the in-progress update
-- that you want to cancel.
--
-- 'environmentName', 'abortEnvironmentUpdate_environmentName' - This specifies the name of the environment with the in-progress update
-- that you want to cancel.
newAbortEnvironmentUpdate ::
  AbortEnvironmentUpdate
newAbortEnvironmentUpdate =
  AbortEnvironmentUpdate'
    { environmentId =
        Core.Nothing,
      environmentName = Core.Nothing
    }

-- | This specifies the ID of the environment with the in-progress update
-- that you want to cancel.
abortEnvironmentUpdate_environmentId :: Lens.Lens' AbortEnvironmentUpdate (Core.Maybe Core.Text)
abortEnvironmentUpdate_environmentId = Lens.lens (\AbortEnvironmentUpdate' {environmentId} -> environmentId) (\s@AbortEnvironmentUpdate' {} a -> s {environmentId = a} :: AbortEnvironmentUpdate)

-- | This specifies the name of the environment with the in-progress update
-- that you want to cancel.
abortEnvironmentUpdate_environmentName :: Lens.Lens' AbortEnvironmentUpdate (Core.Maybe Core.Text)
abortEnvironmentUpdate_environmentName = Lens.lens (\AbortEnvironmentUpdate' {environmentName} -> environmentName) (\s@AbortEnvironmentUpdate' {} a -> s {environmentName = a} :: AbortEnvironmentUpdate)

instance Core.AWSRequest AbortEnvironmentUpdate where
  type
    AWSResponse AbortEnvironmentUpdate =
      AbortEnvironmentUpdateResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      AbortEnvironmentUpdateResponse'

instance Core.Hashable AbortEnvironmentUpdate

instance Core.NFData AbortEnvironmentUpdate

instance Core.ToHeaders AbortEnvironmentUpdate where
  toHeaders = Core.const Core.mempty

instance Core.ToPath AbortEnvironmentUpdate where
  toPath = Core.const "/"

instance Core.ToQuery AbortEnvironmentUpdate where
  toQuery AbortEnvironmentUpdate' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("AbortEnvironmentUpdate" :: Core.ByteString),
        "Version" Core.=: ("2010-12-01" :: Core.ByteString),
        "EnvironmentId" Core.=: environmentId,
        "EnvironmentName" Core.=: environmentName
      ]

-- | /See:/ 'newAbortEnvironmentUpdateResponse' smart constructor.
data AbortEnvironmentUpdateResponse = AbortEnvironmentUpdateResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AbortEnvironmentUpdateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAbortEnvironmentUpdateResponse ::
  AbortEnvironmentUpdateResponse
newAbortEnvironmentUpdateResponse =
  AbortEnvironmentUpdateResponse'

instance Core.NFData AbortEnvironmentUpdateResponse
