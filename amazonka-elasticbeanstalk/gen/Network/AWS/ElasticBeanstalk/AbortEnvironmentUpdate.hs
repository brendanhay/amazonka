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

import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newAbortEnvironmentUpdate' smart constructor.
data AbortEnvironmentUpdate = AbortEnvironmentUpdate'
  { -- | This specifies the ID of the environment with the in-progress update
    -- that you want to cancel.
    environmentId :: Prelude.Maybe Prelude.Text,
    -- | This specifies the name of the environment with the in-progress update
    -- that you want to cancel.
    environmentName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      environmentName = Prelude.Nothing
    }

-- | This specifies the ID of the environment with the in-progress update
-- that you want to cancel.
abortEnvironmentUpdate_environmentId :: Lens.Lens' AbortEnvironmentUpdate (Prelude.Maybe Prelude.Text)
abortEnvironmentUpdate_environmentId = Lens.lens (\AbortEnvironmentUpdate' {environmentId} -> environmentId) (\s@AbortEnvironmentUpdate' {} a -> s {environmentId = a} :: AbortEnvironmentUpdate)

-- | This specifies the name of the environment with the in-progress update
-- that you want to cancel.
abortEnvironmentUpdate_environmentName :: Lens.Lens' AbortEnvironmentUpdate (Prelude.Maybe Prelude.Text)
abortEnvironmentUpdate_environmentName = Lens.lens (\AbortEnvironmentUpdate' {environmentName} -> environmentName) (\s@AbortEnvironmentUpdate' {} a -> s {environmentName = a} :: AbortEnvironmentUpdate)

instance Prelude.AWSRequest AbortEnvironmentUpdate where
  type
    Rs AbortEnvironmentUpdate =
      AbortEnvironmentUpdateResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      AbortEnvironmentUpdateResponse'

instance Prelude.Hashable AbortEnvironmentUpdate

instance Prelude.NFData AbortEnvironmentUpdate

instance Prelude.ToHeaders AbortEnvironmentUpdate where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath AbortEnvironmentUpdate where
  toPath = Prelude.const "/"

instance Prelude.ToQuery AbortEnvironmentUpdate where
  toQuery AbortEnvironmentUpdate' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("AbortEnvironmentUpdate" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-12-01" :: Prelude.ByteString),
        "EnvironmentId" Prelude.=: environmentId,
        "EnvironmentName" Prelude.=: environmentName
      ]

-- | /See:/ 'newAbortEnvironmentUpdateResponse' smart constructor.
data AbortEnvironmentUpdateResponse = AbortEnvironmentUpdateResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AbortEnvironmentUpdateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAbortEnvironmentUpdateResponse ::
  AbortEnvironmentUpdateResponse
newAbortEnvironmentUpdateResponse =
  AbortEnvironmentUpdateResponse'

instance
  Prelude.NFData
    AbortEnvironmentUpdateResponse
