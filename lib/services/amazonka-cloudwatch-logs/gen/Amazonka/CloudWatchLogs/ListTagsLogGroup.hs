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
-- Module      : Amazonka.CloudWatchLogs.ListTagsLogGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the tags for the specified log group.
module Amazonka.CloudWatchLogs.ListTagsLogGroup
  ( -- * Creating a Request
    ListTagsLogGroup (..),
    newListTagsLogGroup,

    -- * Request Lenses
    listTagsLogGroup_logGroupName,

    -- * Destructuring the Response
    ListTagsLogGroupResponse (..),
    newListTagsLogGroupResponse,

    -- * Response Lenses
    listTagsLogGroupResponse_tags,
    listTagsLogGroupResponse_httpStatus,
  )
where

import Amazonka.CloudWatchLogs.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListTagsLogGroup' smart constructor.
data ListTagsLogGroup = ListTagsLogGroup'
  { -- | The name of the log group.
    logGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTagsLogGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logGroupName', 'listTagsLogGroup_logGroupName' - The name of the log group.
newListTagsLogGroup ::
  -- | 'logGroupName'
  Prelude.Text ->
  ListTagsLogGroup
newListTagsLogGroup pLogGroupName_ =
  ListTagsLogGroup' {logGroupName = pLogGroupName_}

-- | The name of the log group.
listTagsLogGroup_logGroupName :: Lens.Lens' ListTagsLogGroup Prelude.Text
listTagsLogGroup_logGroupName = Lens.lens (\ListTagsLogGroup' {logGroupName} -> logGroupName) (\s@ListTagsLogGroup' {} a -> s {logGroupName = a} :: ListTagsLogGroup)

instance Core.AWSRequest ListTagsLogGroup where
  type
    AWSResponse ListTagsLogGroup =
      ListTagsLogGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTagsLogGroupResponse'
            Prelude.<$> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTagsLogGroup where
  hashWithSalt _salt ListTagsLogGroup' {..} =
    _salt `Prelude.hashWithSalt` logGroupName

instance Prelude.NFData ListTagsLogGroup where
  rnf ListTagsLogGroup' {..} = Prelude.rnf logGroupName

instance Core.ToHeaders ListTagsLogGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Logs_20140328.ListTagsLogGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListTagsLogGroup where
  toJSON ListTagsLogGroup' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("logGroupName" Core..= logGroupName)]
      )

instance Core.ToPath ListTagsLogGroup where
  toPath = Prelude.const "/"

instance Core.ToQuery ListTagsLogGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListTagsLogGroupResponse' smart constructor.
data ListTagsLogGroupResponse = ListTagsLogGroupResponse'
  { -- | The tags for the log group.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTagsLogGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'listTagsLogGroupResponse_tags' - The tags for the log group.
--
-- 'httpStatus', 'listTagsLogGroupResponse_httpStatus' - The response's http status code.
newListTagsLogGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTagsLogGroupResponse
newListTagsLogGroupResponse pHttpStatus_ =
  ListTagsLogGroupResponse'
    { tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The tags for the log group.
listTagsLogGroupResponse_tags :: Lens.Lens' ListTagsLogGroupResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
listTagsLogGroupResponse_tags = Lens.lens (\ListTagsLogGroupResponse' {tags} -> tags) (\s@ListTagsLogGroupResponse' {} a -> s {tags = a} :: ListTagsLogGroupResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listTagsLogGroupResponse_httpStatus :: Lens.Lens' ListTagsLogGroupResponse Prelude.Int
listTagsLogGroupResponse_httpStatus = Lens.lens (\ListTagsLogGroupResponse' {httpStatus} -> httpStatus) (\s@ListTagsLogGroupResponse' {} a -> s {httpStatus = a} :: ListTagsLogGroupResponse)

instance Prelude.NFData ListTagsLogGroupResponse where
  rnf ListTagsLogGroupResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
