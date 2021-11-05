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
-- Module      : Network.AWS.Backup.ListBackupPlanTemplates
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns metadata of your saved backup plan templates, including the
-- template ID, name, and the creation and deletion dates.
module Network.AWS.Backup.ListBackupPlanTemplates
  ( -- * Creating a Request
    ListBackupPlanTemplates (..),
    newListBackupPlanTemplates,

    -- * Request Lenses
    listBackupPlanTemplates_nextToken,
    listBackupPlanTemplates_maxResults,

    -- * Destructuring the Response
    ListBackupPlanTemplatesResponse (..),
    newListBackupPlanTemplatesResponse,

    -- * Response Lenses
    listBackupPlanTemplatesResponse_backupPlanTemplatesList,
    listBackupPlanTemplatesResponse_nextToken,
    listBackupPlanTemplatesResponse_httpStatus,
  )
where

import Network.AWS.Backup.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListBackupPlanTemplates' smart constructor.
data ListBackupPlanTemplates = ListBackupPlanTemplates'
  { -- | The next item following a partial list of returned items. For example,
    -- if a request is made to return @maxResults@ number of items, @NextToken@
    -- allows you to return more items in your list starting at the location
    -- pointed to by the next token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to be returned.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBackupPlanTemplates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listBackupPlanTemplates_nextToken' - The next item following a partial list of returned items. For example,
-- if a request is made to return @maxResults@ number of items, @NextToken@
-- allows you to return more items in your list starting at the location
-- pointed to by the next token.
--
-- 'maxResults', 'listBackupPlanTemplates_maxResults' - The maximum number of items to be returned.
newListBackupPlanTemplates ::
  ListBackupPlanTemplates
newListBackupPlanTemplates =
  ListBackupPlanTemplates'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The next item following a partial list of returned items. For example,
-- if a request is made to return @maxResults@ number of items, @NextToken@
-- allows you to return more items in your list starting at the location
-- pointed to by the next token.
listBackupPlanTemplates_nextToken :: Lens.Lens' ListBackupPlanTemplates (Prelude.Maybe Prelude.Text)
listBackupPlanTemplates_nextToken = Lens.lens (\ListBackupPlanTemplates' {nextToken} -> nextToken) (\s@ListBackupPlanTemplates' {} a -> s {nextToken = a} :: ListBackupPlanTemplates)

-- | The maximum number of items to be returned.
listBackupPlanTemplates_maxResults :: Lens.Lens' ListBackupPlanTemplates (Prelude.Maybe Prelude.Natural)
listBackupPlanTemplates_maxResults = Lens.lens (\ListBackupPlanTemplates' {maxResults} -> maxResults) (\s@ListBackupPlanTemplates' {} a -> s {maxResults = a} :: ListBackupPlanTemplates)

instance Core.AWSRequest ListBackupPlanTemplates where
  type
    AWSResponse ListBackupPlanTemplates =
      ListBackupPlanTemplatesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListBackupPlanTemplatesResponse'
            Prelude.<$> ( x Core..?> "BackupPlanTemplatesList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListBackupPlanTemplates

instance Prelude.NFData ListBackupPlanTemplates

instance Core.ToHeaders ListBackupPlanTemplates where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListBackupPlanTemplates where
  toPath = Prelude.const "/backup/template/plans"

instance Core.ToQuery ListBackupPlanTemplates where
  toQuery ListBackupPlanTemplates' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListBackupPlanTemplatesResponse' smart constructor.
data ListBackupPlanTemplatesResponse = ListBackupPlanTemplatesResponse'
  { -- | An array of template list items containing metadata about your saved
    -- templates.
    backupPlanTemplatesList :: Prelude.Maybe [BackupPlanTemplatesListMember],
    -- | The next item following a partial list of returned items. For example,
    -- if a request is made to return @maxResults@ number of items, @NextToken@
    -- allows you to return more items in your list starting at the location
    -- pointed to by the next token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBackupPlanTemplatesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backupPlanTemplatesList', 'listBackupPlanTemplatesResponse_backupPlanTemplatesList' - An array of template list items containing metadata about your saved
-- templates.
--
-- 'nextToken', 'listBackupPlanTemplatesResponse_nextToken' - The next item following a partial list of returned items. For example,
-- if a request is made to return @maxResults@ number of items, @NextToken@
-- allows you to return more items in your list starting at the location
-- pointed to by the next token.
--
-- 'httpStatus', 'listBackupPlanTemplatesResponse_httpStatus' - The response's http status code.
newListBackupPlanTemplatesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListBackupPlanTemplatesResponse
newListBackupPlanTemplatesResponse pHttpStatus_ =
  ListBackupPlanTemplatesResponse'
    { backupPlanTemplatesList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of template list items containing metadata about your saved
-- templates.
listBackupPlanTemplatesResponse_backupPlanTemplatesList :: Lens.Lens' ListBackupPlanTemplatesResponse (Prelude.Maybe [BackupPlanTemplatesListMember])
listBackupPlanTemplatesResponse_backupPlanTemplatesList = Lens.lens (\ListBackupPlanTemplatesResponse' {backupPlanTemplatesList} -> backupPlanTemplatesList) (\s@ListBackupPlanTemplatesResponse' {} a -> s {backupPlanTemplatesList = a} :: ListBackupPlanTemplatesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The next item following a partial list of returned items. For example,
-- if a request is made to return @maxResults@ number of items, @NextToken@
-- allows you to return more items in your list starting at the location
-- pointed to by the next token.
listBackupPlanTemplatesResponse_nextToken :: Lens.Lens' ListBackupPlanTemplatesResponse (Prelude.Maybe Prelude.Text)
listBackupPlanTemplatesResponse_nextToken = Lens.lens (\ListBackupPlanTemplatesResponse' {nextToken} -> nextToken) (\s@ListBackupPlanTemplatesResponse' {} a -> s {nextToken = a} :: ListBackupPlanTemplatesResponse)

-- | The response's http status code.
listBackupPlanTemplatesResponse_httpStatus :: Lens.Lens' ListBackupPlanTemplatesResponse Prelude.Int
listBackupPlanTemplatesResponse_httpStatus = Lens.lens (\ListBackupPlanTemplatesResponse' {httpStatus} -> httpStatus) (\s@ListBackupPlanTemplatesResponse' {} a -> s {httpStatus = a} :: ListBackupPlanTemplatesResponse)

instance
  Prelude.NFData
    ListBackupPlanTemplatesResponse
