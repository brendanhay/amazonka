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
-- Module      : Network.AWS.CodeCommit.PutRepositoryTriggers
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Replaces all triggers for a repository. Used to create or delete
-- triggers.
module Network.AWS.CodeCommit.PutRepositoryTriggers
  ( -- * Creating a Request
    PutRepositoryTriggers (..),
    newPutRepositoryTriggers,

    -- * Request Lenses
    putRepositoryTriggers_repositoryName,
    putRepositoryTriggers_triggers,

    -- * Destructuring the Response
    PutRepositoryTriggersResponse (..),
    newPutRepositoryTriggersResponse,

    -- * Response Lenses
    putRepositoryTriggersResponse_configurationId,
    putRepositoryTriggersResponse_httpStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a put repository triggers operation.
--
-- /See:/ 'newPutRepositoryTriggers' smart constructor.
data PutRepositoryTriggers = PutRepositoryTriggers'
  { -- | The name of the repository where you want to create or update the
    -- trigger.
    repositoryName :: Core.Text,
    -- | The JSON block of configuration information for each trigger.
    triggers :: [RepositoryTrigger]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutRepositoryTriggers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'repositoryName', 'putRepositoryTriggers_repositoryName' - The name of the repository where you want to create or update the
-- trigger.
--
-- 'triggers', 'putRepositoryTriggers_triggers' - The JSON block of configuration information for each trigger.
newPutRepositoryTriggers ::
  -- | 'repositoryName'
  Core.Text ->
  PutRepositoryTriggers
newPutRepositoryTriggers pRepositoryName_ =
  PutRepositoryTriggers'
    { repositoryName =
        pRepositoryName_,
      triggers = Core.mempty
    }

-- | The name of the repository where you want to create or update the
-- trigger.
putRepositoryTriggers_repositoryName :: Lens.Lens' PutRepositoryTriggers Core.Text
putRepositoryTriggers_repositoryName = Lens.lens (\PutRepositoryTriggers' {repositoryName} -> repositoryName) (\s@PutRepositoryTriggers' {} a -> s {repositoryName = a} :: PutRepositoryTriggers)

-- | The JSON block of configuration information for each trigger.
putRepositoryTriggers_triggers :: Lens.Lens' PutRepositoryTriggers [RepositoryTrigger]
putRepositoryTriggers_triggers = Lens.lens (\PutRepositoryTriggers' {triggers} -> triggers) (\s@PutRepositoryTriggers' {} a -> s {triggers = a} :: PutRepositoryTriggers) Core.. Lens._Coerce

instance Core.AWSRequest PutRepositoryTriggers where
  type
    AWSResponse PutRepositoryTriggers =
      PutRepositoryTriggersResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PutRepositoryTriggersResponse'
            Core.<$> (x Core..?> "configurationId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable PutRepositoryTriggers

instance Core.NFData PutRepositoryTriggers

instance Core.ToHeaders PutRepositoryTriggers where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeCommit_20150413.PutRepositoryTriggers" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON PutRepositoryTriggers where
  toJSON PutRepositoryTriggers' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("repositoryName" Core..= repositoryName),
            Core.Just ("triggers" Core..= triggers)
          ]
      )

instance Core.ToPath PutRepositoryTriggers where
  toPath = Core.const "/"

instance Core.ToQuery PutRepositoryTriggers where
  toQuery = Core.const Core.mempty

-- | Represents the output of a put repository triggers operation.
--
-- /See:/ 'newPutRepositoryTriggersResponse' smart constructor.
data PutRepositoryTriggersResponse = PutRepositoryTriggersResponse'
  { -- | The system-generated unique ID for the create or update operation.
    configurationId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutRepositoryTriggersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationId', 'putRepositoryTriggersResponse_configurationId' - The system-generated unique ID for the create or update operation.
--
-- 'httpStatus', 'putRepositoryTriggersResponse_httpStatus' - The response's http status code.
newPutRepositoryTriggersResponse ::
  -- | 'httpStatus'
  Core.Int ->
  PutRepositoryTriggersResponse
newPutRepositoryTriggersResponse pHttpStatus_ =
  PutRepositoryTriggersResponse'
    { configurationId =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The system-generated unique ID for the create or update operation.
putRepositoryTriggersResponse_configurationId :: Lens.Lens' PutRepositoryTriggersResponse (Core.Maybe Core.Text)
putRepositoryTriggersResponse_configurationId = Lens.lens (\PutRepositoryTriggersResponse' {configurationId} -> configurationId) (\s@PutRepositoryTriggersResponse' {} a -> s {configurationId = a} :: PutRepositoryTriggersResponse)

-- | The response's http status code.
putRepositoryTriggersResponse_httpStatus :: Lens.Lens' PutRepositoryTriggersResponse Core.Int
putRepositoryTriggersResponse_httpStatus = Lens.lens (\PutRepositoryTriggersResponse' {httpStatus} -> httpStatus) (\s@PutRepositoryTriggersResponse' {} a -> s {httpStatus = a} :: PutRepositoryTriggersResponse)

instance Core.NFData PutRepositoryTriggersResponse
