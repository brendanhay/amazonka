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
-- Module      : Amazonka.CodeCommit.GetRepositoryTriggers
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about triggers configured for a repository.
module Amazonka.CodeCommit.GetRepositoryTriggers
  ( -- * Creating a Request
    GetRepositoryTriggers (..),
    newGetRepositoryTriggers,

    -- * Request Lenses
    getRepositoryTriggers_repositoryName,

    -- * Destructuring the Response
    GetRepositoryTriggersResponse (..),
    newGetRepositoryTriggersResponse,

    -- * Response Lenses
    getRepositoryTriggersResponse_configurationId,
    getRepositoryTriggersResponse_triggers,
    getRepositoryTriggersResponse_httpStatus,
  )
where

import Amazonka.CodeCommit.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a get repository triggers operation.
--
-- /See:/ 'newGetRepositoryTriggers' smart constructor.
data GetRepositoryTriggers = GetRepositoryTriggers'
  { -- | The name of the repository for which the trigger is configured.
    repositoryName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRepositoryTriggers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'repositoryName', 'getRepositoryTriggers_repositoryName' - The name of the repository for which the trigger is configured.
newGetRepositoryTriggers ::
  -- | 'repositoryName'
  Prelude.Text ->
  GetRepositoryTriggers
newGetRepositoryTriggers pRepositoryName_ =
  GetRepositoryTriggers'
    { repositoryName =
        pRepositoryName_
    }

-- | The name of the repository for which the trigger is configured.
getRepositoryTriggers_repositoryName :: Lens.Lens' GetRepositoryTriggers Prelude.Text
getRepositoryTriggers_repositoryName = Lens.lens (\GetRepositoryTriggers' {repositoryName} -> repositoryName) (\s@GetRepositoryTriggers' {} a -> s {repositoryName = a} :: GetRepositoryTriggers)

instance Core.AWSRequest GetRepositoryTriggers where
  type
    AWSResponse GetRepositoryTriggers =
      GetRepositoryTriggersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRepositoryTriggersResponse'
            Prelude.<$> (x Data..?> "configurationId")
            Prelude.<*> (x Data..?> "triggers" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetRepositoryTriggers where
  hashWithSalt _salt GetRepositoryTriggers' {..} =
    _salt `Prelude.hashWithSalt` repositoryName

instance Prelude.NFData GetRepositoryTriggers where
  rnf GetRepositoryTriggers' {..} =
    Prelude.rnf repositoryName

instance Data.ToHeaders GetRepositoryTriggers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeCommit_20150413.GetRepositoryTriggers" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetRepositoryTriggers where
  toJSON GetRepositoryTriggers' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("repositoryName" Data..= repositoryName)
          ]
      )

instance Data.ToPath GetRepositoryTriggers where
  toPath = Prelude.const "/"

instance Data.ToQuery GetRepositoryTriggers where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a get repository triggers operation.
--
-- /See:/ 'newGetRepositoryTriggersResponse' smart constructor.
data GetRepositoryTriggersResponse = GetRepositoryTriggersResponse'
  { -- | The system-generated unique ID for the trigger.
    configurationId :: Prelude.Maybe Prelude.Text,
    -- | The JSON block of configuration information for each trigger.
    triggers :: Prelude.Maybe [RepositoryTrigger],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRepositoryTriggersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationId', 'getRepositoryTriggersResponse_configurationId' - The system-generated unique ID for the trigger.
--
-- 'triggers', 'getRepositoryTriggersResponse_triggers' - The JSON block of configuration information for each trigger.
--
-- 'httpStatus', 'getRepositoryTriggersResponse_httpStatus' - The response's http status code.
newGetRepositoryTriggersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetRepositoryTriggersResponse
newGetRepositoryTriggersResponse pHttpStatus_ =
  GetRepositoryTriggersResponse'
    { configurationId =
        Prelude.Nothing,
      triggers = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The system-generated unique ID for the trigger.
getRepositoryTriggersResponse_configurationId :: Lens.Lens' GetRepositoryTriggersResponse (Prelude.Maybe Prelude.Text)
getRepositoryTriggersResponse_configurationId = Lens.lens (\GetRepositoryTriggersResponse' {configurationId} -> configurationId) (\s@GetRepositoryTriggersResponse' {} a -> s {configurationId = a} :: GetRepositoryTriggersResponse)

-- | The JSON block of configuration information for each trigger.
getRepositoryTriggersResponse_triggers :: Lens.Lens' GetRepositoryTriggersResponse (Prelude.Maybe [RepositoryTrigger])
getRepositoryTriggersResponse_triggers = Lens.lens (\GetRepositoryTriggersResponse' {triggers} -> triggers) (\s@GetRepositoryTriggersResponse' {} a -> s {triggers = a} :: GetRepositoryTriggersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getRepositoryTriggersResponse_httpStatus :: Lens.Lens' GetRepositoryTriggersResponse Prelude.Int
getRepositoryTriggersResponse_httpStatus = Lens.lens (\GetRepositoryTriggersResponse' {httpStatus} -> httpStatus) (\s@GetRepositoryTriggersResponse' {} a -> s {httpStatus = a} :: GetRepositoryTriggersResponse)

instance Prelude.NFData GetRepositoryTriggersResponse where
  rnf GetRepositoryTriggersResponse' {..} =
    Prelude.rnf configurationId `Prelude.seq`
      Prelude.rnf triggers `Prelude.seq`
        Prelude.rnf httpStatus
