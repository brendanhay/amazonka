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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
putRepositoryTriggers_triggers = Lens.lens (\PutRepositoryTriggers' {triggers} -> triggers) (\s@PutRepositoryTriggers' {} a -> s {triggers = a} :: PutRepositoryTriggers) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest PutRepositoryTriggers where
  type
    Rs PutRepositoryTriggers =
      PutRepositoryTriggersResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PutRepositoryTriggersResponse'
            Prelude.<$> (x Prelude..?> "configurationId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutRepositoryTriggers

instance Prelude.NFData PutRepositoryTriggers

instance Prelude.ToHeaders PutRepositoryTriggers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CodeCommit_20150413.PutRepositoryTriggers" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON PutRepositoryTriggers where
  toJSON PutRepositoryTriggers' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("repositoryName" Prelude..= repositoryName),
            Prelude.Just ("triggers" Prelude..= triggers)
          ]
      )

instance Prelude.ToPath PutRepositoryTriggers where
  toPath = Prelude.const "/"

instance Prelude.ToQuery PutRepositoryTriggers where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData PutRepositoryTriggersResponse
