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
-- Module      : Network.AWS.CodeCommit.GetRepositoryTriggers
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about triggers configured for a repository.
module Network.AWS.CodeCommit.GetRepositoryTriggers
  ( -- * Creating a Request
    GetRepositoryTriggers (..),
    newGetRepositoryTriggers,

    -- * Request Lenses
    getRepositoryTriggers_repositoryName,

    -- * Destructuring the Response
    GetRepositoryTriggersResponse (..),
    newGetRepositoryTriggersResponse,

    -- * Response Lenses
    getRepositoryTriggersResponse_triggers,
    getRepositoryTriggersResponse_configurationId,
    getRepositoryTriggersResponse_httpStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a get repository triggers operation.
--
-- /See:/ 'newGetRepositoryTriggers' smart constructor.
data GetRepositoryTriggers = GetRepositoryTriggers'
  { -- | The name of the repository for which the trigger is configured.
    repositoryName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  GetRepositoryTriggers
newGetRepositoryTriggers pRepositoryName_ =
  GetRepositoryTriggers'
    { repositoryName =
        pRepositoryName_
    }

-- | The name of the repository for which the trigger is configured.
getRepositoryTriggers_repositoryName :: Lens.Lens' GetRepositoryTriggers Core.Text
getRepositoryTriggers_repositoryName = Lens.lens (\GetRepositoryTriggers' {repositoryName} -> repositoryName) (\s@GetRepositoryTriggers' {} a -> s {repositoryName = a} :: GetRepositoryTriggers)

instance Core.AWSRequest GetRepositoryTriggers where
  type
    AWSResponse GetRepositoryTriggers =
      GetRepositoryTriggersResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRepositoryTriggersResponse'
            Core.<$> (x Core..?> "triggers" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "configurationId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetRepositoryTriggers

instance Core.NFData GetRepositoryTriggers

instance Core.ToHeaders GetRepositoryTriggers where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeCommit_20150413.GetRepositoryTriggers" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetRepositoryTriggers where
  toJSON GetRepositoryTriggers' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("repositoryName" Core..= repositoryName)
          ]
      )

instance Core.ToPath GetRepositoryTriggers where
  toPath = Core.const "/"

instance Core.ToQuery GetRepositoryTriggers where
  toQuery = Core.const Core.mempty

-- | Represents the output of a get repository triggers operation.
--
-- /See:/ 'newGetRepositoryTriggersResponse' smart constructor.
data GetRepositoryTriggersResponse = GetRepositoryTriggersResponse'
  { -- | The JSON block of configuration information for each trigger.
    triggers :: Core.Maybe [RepositoryTrigger],
    -- | The system-generated unique ID for the trigger.
    configurationId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetRepositoryTriggersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'triggers', 'getRepositoryTriggersResponse_triggers' - The JSON block of configuration information for each trigger.
--
-- 'configurationId', 'getRepositoryTriggersResponse_configurationId' - The system-generated unique ID for the trigger.
--
-- 'httpStatus', 'getRepositoryTriggersResponse_httpStatus' - The response's http status code.
newGetRepositoryTriggersResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetRepositoryTriggersResponse
newGetRepositoryTriggersResponse pHttpStatus_ =
  GetRepositoryTriggersResponse'
    { triggers =
        Core.Nothing,
      configurationId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The JSON block of configuration information for each trigger.
getRepositoryTriggersResponse_triggers :: Lens.Lens' GetRepositoryTriggersResponse (Core.Maybe [RepositoryTrigger])
getRepositoryTriggersResponse_triggers = Lens.lens (\GetRepositoryTriggersResponse' {triggers} -> triggers) (\s@GetRepositoryTriggersResponse' {} a -> s {triggers = a} :: GetRepositoryTriggersResponse) Core.. Lens.mapping Lens._Coerce

-- | The system-generated unique ID for the trigger.
getRepositoryTriggersResponse_configurationId :: Lens.Lens' GetRepositoryTriggersResponse (Core.Maybe Core.Text)
getRepositoryTriggersResponse_configurationId = Lens.lens (\GetRepositoryTriggersResponse' {configurationId} -> configurationId) (\s@GetRepositoryTriggersResponse' {} a -> s {configurationId = a} :: GetRepositoryTriggersResponse)

-- | The response's http status code.
getRepositoryTriggersResponse_httpStatus :: Lens.Lens' GetRepositoryTriggersResponse Core.Int
getRepositoryTriggersResponse_httpStatus = Lens.lens (\GetRepositoryTriggersResponse' {httpStatus} -> httpStatus) (\s@GetRepositoryTriggersResponse' {} a -> s {httpStatus = a} :: GetRepositoryTriggersResponse)

instance Core.NFData GetRepositoryTriggersResponse
