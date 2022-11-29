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
-- Module      : Amazonka.Glue.ListTriggers
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the names of all trigger resources in this Amazon Web Services
-- account, or the resources with the specified tag. This operation allows
-- you to see which resources are available in your account, and their
-- names.
--
-- This operation takes the optional @Tags@ field, which you can use as a
-- filter on the response so that tagged resources can be retrieved as a
-- group. If you choose to use tags filtering, only resources with the tag
-- are retrieved.
module Amazonka.Glue.ListTriggers
  ( -- * Creating a Request
    ListTriggers (..),
    newListTriggers,

    -- * Request Lenses
    listTriggers_tags,
    listTriggers_nextToken,
    listTriggers_maxResults,
    listTriggers_dependentJobName,

    -- * Destructuring the Response
    ListTriggersResponse (..),
    newListTriggersResponse,

    -- * Response Lenses
    listTriggersResponse_nextToken,
    listTriggersResponse_triggerNames,
    listTriggersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListTriggers' smart constructor.
data ListTriggers = ListTriggers'
  { -- | Specifies to return only these tagged resources.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A continuation token, if this is a continuation request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum size of a list to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The name of the job for which to retrieve triggers. The trigger that can
    -- start this job is returned. If there is no such trigger, all triggers
    -- are returned.
    dependentJobName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTriggers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'listTriggers_tags' - Specifies to return only these tagged resources.
--
-- 'nextToken', 'listTriggers_nextToken' - A continuation token, if this is a continuation request.
--
-- 'maxResults', 'listTriggers_maxResults' - The maximum size of a list to return.
--
-- 'dependentJobName', 'listTriggers_dependentJobName' - The name of the job for which to retrieve triggers. The trigger that can
-- start this job is returned. If there is no such trigger, all triggers
-- are returned.
newListTriggers ::
  ListTriggers
newListTriggers =
  ListTriggers'
    { tags = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      dependentJobName = Prelude.Nothing
    }

-- | Specifies to return only these tagged resources.
listTriggers_tags :: Lens.Lens' ListTriggers (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
listTriggers_tags = Lens.lens (\ListTriggers' {tags} -> tags) (\s@ListTriggers' {} a -> s {tags = a} :: ListTriggers) Prelude.. Lens.mapping Lens.coerced

-- | A continuation token, if this is a continuation request.
listTriggers_nextToken :: Lens.Lens' ListTriggers (Prelude.Maybe Prelude.Text)
listTriggers_nextToken = Lens.lens (\ListTriggers' {nextToken} -> nextToken) (\s@ListTriggers' {} a -> s {nextToken = a} :: ListTriggers)

-- | The maximum size of a list to return.
listTriggers_maxResults :: Lens.Lens' ListTriggers (Prelude.Maybe Prelude.Natural)
listTriggers_maxResults = Lens.lens (\ListTriggers' {maxResults} -> maxResults) (\s@ListTriggers' {} a -> s {maxResults = a} :: ListTriggers)

-- | The name of the job for which to retrieve triggers. The trigger that can
-- start this job is returned. If there is no such trigger, all triggers
-- are returned.
listTriggers_dependentJobName :: Lens.Lens' ListTriggers (Prelude.Maybe Prelude.Text)
listTriggers_dependentJobName = Lens.lens (\ListTriggers' {dependentJobName} -> dependentJobName) (\s@ListTriggers' {} a -> s {dependentJobName = a} :: ListTriggers)

instance Core.AWSRequest ListTriggers where
  type AWSResponse ListTriggers = ListTriggersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTriggersResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "TriggerNames" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTriggers where
  hashWithSalt _salt ListTriggers' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` dependentJobName

instance Prelude.NFData ListTriggers where
  rnf ListTriggers' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf dependentJobName

instance Core.ToHeaders ListTriggers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.ListTriggers" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListTriggers where
  toJSON ListTriggers' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Tags" Core..=) Prelude.<$> tags,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("DependentJobName" Core..=)
              Prelude.<$> dependentJobName
          ]
      )

instance Core.ToPath ListTriggers where
  toPath = Prelude.const "/"

instance Core.ToQuery ListTriggers where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListTriggersResponse' smart constructor.
data ListTriggersResponse = ListTriggersResponse'
  { -- | A continuation token, if the returned list does not contain the last
    -- metric available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The names of all triggers in the account, or the triggers with the
    -- specified tags.
    triggerNames :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTriggersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTriggersResponse_nextToken' - A continuation token, if the returned list does not contain the last
-- metric available.
--
-- 'triggerNames', 'listTriggersResponse_triggerNames' - The names of all triggers in the account, or the triggers with the
-- specified tags.
--
-- 'httpStatus', 'listTriggersResponse_httpStatus' - The response's http status code.
newListTriggersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTriggersResponse
newListTriggersResponse pHttpStatus_ =
  ListTriggersResponse'
    { nextToken = Prelude.Nothing,
      triggerNames = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A continuation token, if the returned list does not contain the last
-- metric available.
listTriggersResponse_nextToken :: Lens.Lens' ListTriggersResponse (Prelude.Maybe Prelude.Text)
listTriggersResponse_nextToken = Lens.lens (\ListTriggersResponse' {nextToken} -> nextToken) (\s@ListTriggersResponse' {} a -> s {nextToken = a} :: ListTriggersResponse)

-- | The names of all triggers in the account, or the triggers with the
-- specified tags.
listTriggersResponse_triggerNames :: Lens.Lens' ListTriggersResponse (Prelude.Maybe [Prelude.Text])
listTriggersResponse_triggerNames = Lens.lens (\ListTriggersResponse' {triggerNames} -> triggerNames) (\s@ListTriggersResponse' {} a -> s {triggerNames = a} :: ListTriggersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listTriggersResponse_httpStatus :: Lens.Lens' ListTriggersResponse Prelude.Int
listTriggersResponse_httpStatus = Lens.lens (\ListTriggersResponse' {httpStatus} -> httpStatus) (\s@ListTriggersResponse' {} a -> s {httpStatus = a} :: ListTriggersResponse)

instance Prelude.NFData ListTriggersResponse where
  rnf ListTriggersResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf triggerNames
      `Prelude.seq` Prelude.rnf httpStatus
