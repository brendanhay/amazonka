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
-- Module      : Amazonka.ElasticBeanstalk.AbortEnvironmentUpdate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels in-progress environment configuration update or application
-- version deployment.
module Amazonka.ElasticBeanstalk.AbortEnvironmentUpdate
  ( -- * Creating a Request
    AbortEnvironmentUpdate (..),
    newAbortEnvironmentUpdate,

    -- * Request Lenses
    abortEnvironmentUpdate_environmentName,
    abortEnvironmentUpdate_environmentId,

    -- * Destructuring the Response
    AbortEnvironmentUpdateResponse (..),
    newAbortEnvironmentUpdateResponse,
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
-- /See:/ 'newAbortEnvironmentUpdate' smart constructor.
data AbortEnvironmentUpdate = AbortEnvironmentUpdate'
  { -- | This specifies the name of the environment with the in-progress update
    -- that you want to cancel.
    environmentName :: Prelude.Maybe Prelude.Text,
    -- | This specifies the ID of the environment with the in-progress update
    -- that you want to cancel.
    environmentId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AbortEnvironmentUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'environmentName', 'abortEnvironmentUpdate_environmentName' - This specifies the name of the environment with the in-progress update
-- that you want to cancel.
--
-- 'environmentId', 'abortEnvironmentUpdate_environmentId' - This specifies the ID of the environment with the in-progress update
-- that you want to cancel.
newAbortEnvironmentUpdate ::
  AbortEnvironmentUpdate
newAbortEnvironmentUpdate =
  AbortEnvironmentUpdate'
    { environmentName =
        Prelude.Nothing,
      environmentId = Prelude.Nothing
    }

-- | This specifies the name of the environment with the in-progress update
-- that you want to cancel.
abortEnvironmentUpdate_environmentName :: Lens.Lens' AbortEnvironmentUpdate (Prelude.Maybe Prelude.Text)
abortEnvironmentUpdate_environmentName = Lens.lens (\AbortEnvironmentUpdate' {environmentName} -> environmentName) (\s@AbortEnvironmentUpdate' {} a -> s {environmentName = a} :: AbortEnvironmentUpdate)

-- | This specifies the ID of the environment with the in-progress update
-- that you want to cancel.
abortEnvironmentUpdate_environmentId :: Lens.Lens' AbortEnvironmentUpdate (Prelude.Maybe Prelude.Text)
abortEnvironmentUpdate_environmentId = Lens.lens (\AbortEnvironmentUpdate' {environmentId} -> environmentId) (\s@AbortEnvironmentUpdate' {} a -> s {environmentId = a} :: AbortEnvironmentUpdate)

instance Core.AWSRequest AbortEnvironmentUpdate where
  type
    AWSResponse AbortEnvironmentUpdate =
      AbortEnvironmentUpdateResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull
      AbortEnvironmentUpdateResponse'

instance Prelude.Hashable AbortEnvironmentUpdate where
  hashWithSalt _salt AbortEnvironmentUpdate' {..} =
    _salt `Prelude.hashWithSalt` environmentName
      `Prelude.hashWithSalt` environmentId

instance Prelude.NFData AbortEnvironmentUpdate where
  rnf AbortEnvironmentUpdate' {..} =
    Prelude.rnf environmentName
      `Prelude.seq` Prelude.rnf environmentId

instance Core.ToHeaders AbortEnvironmentUpdate where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath AbortEnvironmentUpdate where
  toPath = Prelude.const "/"

instance Core.ToQuery AbortEnvironmentUpdate where
  toQuery AbortEnvironmentUpdate' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("AbortEnvironmentUpdate" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-12-01" :: Prelude.ByteString),
        "EnvironmentName" Core.=: environmentName,
        "EnvironmentId" Core.=: environmentId
      ]

-- | /See:/ 'newAbortEnvironmentUpdateResponse' smart constructor.
data AbortEnvironmentUpdateResponse = AbortEnvironmentUpdateResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf _ = ()
