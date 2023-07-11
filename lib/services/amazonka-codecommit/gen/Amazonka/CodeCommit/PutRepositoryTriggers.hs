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
-- Module      : Amazonka.CodeCommit.PutRepositoryTriggers
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Replaces all triggers for a repository. Used to create or delete
-- triggers.
module Amazonka.CodeCommit.PutRepositoryTriggers
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

import Amazonka.CodeCommit.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a put repository triggers operation.
--
-- /See:/ 'newPutRepositoryTriggers' smart constructor.
data PutRepositoryTriggers = PutRepositoryTriggers'
  { -- | The name of the repository where you want to create or update the
    -- trigger.
    repositoryName :: Prelude.Text,
    -- | The JSON block of configuration information for each trigger.
    triggers :: [RepositoryTrigger]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  PutRepositoryTriggers
newPutRepositoryTriggers pRepositoryName_ =
  PutRepositoryTriggers'
    { repositoryName =
        pRepositoryName_,
      triggers = Prelude.mempty
    }

-- | The name of the repository where you want to create or update the
-- trigger.
putRepositoryTriggers_repositoryName :: Lens.Lens' PutRepositoryTriggers Prelude.Text
putRepositoryTriggers_repositoryName = Lens.lens (\PutRepositoryTriggers' {repositoryName} -> repositoryName) (\s@PutRepositoryTriggers' {} a -> s {repositoryName = a} :: PutRepositoryTriggers)

-- | The JSON block of configuration information for each trigger.
putRepositoryTriggers_triggers :: Lens.Lens' PutRepositoryTriggers [RepositoryTrigger]
putRepositoryTriggers_triggers = Lens.lens (\PutRepositoryTriggers' {triggers} -> triggers) (\s@PutRepositoryTriggers' {} a -> s {triggers = a} :: PutRepositoryTriggers) Prelude.. Lens.coerced

instance Core.AWSRequest PutRepositoryTriggers where
  type
    AWSResponse PutRepositoryTriggers =
      PutRepositoryTriggersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutRepositoryTriggersResponse'
            Prelude.<$> (x Data..?> "configurationId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutRepositoryTriggers where
  hashWithSalt _salt PutRepositoryTriggers' {..} =
    _salt
      `Prelude.hashWithSalt` repositoryName
      `Prelude.hashWithSalt` triggers

instance Prelude.NFData PutRepositoryTriggers where
  rnf PutRepositoryTriggers' {..} =
    Prelude.rnf repositoryName
      `Prelude.seq` Prelude.rnf triggers

instance Data.ToHeaders PutRepositoryTriggers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeCommit_20150413.PutRepositoryTriggers" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutRepositoryTriggers where
  toJSON PutRepositoryTriggers' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("repositoryName" Data..= repositoryName),
            Prelude.Just ("triggers" Data..= triggers)
          ]
      )

instance Data.ToPath PutRepositoryTriggers where
  toPath = Prelude.const "/"

instance Data.ToQuery PutRepositoryTriggers where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a put repository triggers operation.
--
-- /See:/ 'newPutRepositoryTriggersResponse' smart constructor.
data PutRepositoryTriggersResponse = PutRepositoryTriggersResponse'
  { -- | The system-generated unique ID for the create or update operation.
    configurationId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  PutRepositoryTriggersResponse
newPutRepositoryTriggersResponse pHttpStatus_ =
  PutRepositoryTriggersResponse'
    { configurationId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The system-generated unique ID for the create or update operation.
putRepositoryTriggersResponse_configurationId :: Lens.Lens' PutRepositoryTriggersResponse (Prelude.Maybe Prelude.Text)
putRepositoryTriggersResponse_configurationId = Lens.lens (\PutRepositoryTriggersResponse' {configurationId} -> configurationId) (\s@PutRepositoryTriggersResponse' {} a -> s {configurationId = a} :: PutRepositoryTriggersResponse)

-- | The response's http status code.
putRepositoryTriggersResponse_httpStatus :: Lens.Lens' PutRepositoryTriggersResponse Prelude.Int
putRepositoryTriggersResponse_httpStatus = Lens.lens (\PutRepositoryTriggersResponse' {httpStatus} -> httpStatus) (\s@PutRepositoryTriggersResponse' {} a -> s {httpStatus = a} :: PutRepositoryTriggersResponse)

instance Prelude.NFData PutRepositoryTriggersResponse where
  rnf PutRepositoryTriggersResponse' {..} =
    Prelude.rnf configurationId
      `Prelude.seq` Prelude.rnf httpStatus
