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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
    abortEnvironmentUpdate_environmentId,
    abortEnvironmentUpdate_environmentName,

    -- * Destructuring the Response
    AbortEnvironmentUpdateResponse (..),
    newAbortEnvironmentUpdateResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticBeanstalk.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    _salt
      `Prelude.hashWithSalt` environmentId
      `Prelude.hashWithSalt` environmentName

instance Prelude.NFData AbortEnvironmentUpdate where
  rnf AbortEnvironmentUpdate' {..} =
    Prelude.rnf environmentId
      `Prelude.seq` Prelude.rnf environmentName

instance Data.ToHeaders AbortEnvironmentUpdate where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath AbortEnvironmentUpdate where
  toPath = Prelude.const "/"

instance Data.ToQuery AbortEnvironmentUpdate where
  toQuery AbortEnvironmentUpdate' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("AbortEnvironmentUpdate" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-12-01" :: Prelude.ByteString),
        "EnvironmentId" Data.=: environmentId,
        "EnvironmentName" Data.=: environmentName
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
