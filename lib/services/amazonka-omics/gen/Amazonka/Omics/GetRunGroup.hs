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
-- Module      : Amazonka.Omics.GetRunGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a workflow run group.
module Amazonka.Omics.GetRunGroup
  ( -- * Creating a Request
    GetRunGroup (..),
    newGetRunGroup,

    -- * Request Lenses
    getRunGroup_id,

    -- * Destructuring the Response
    GetRunGroupResponse (..),
    newGetRunGroupResponse,

    -- * Response Lenses
    getRunGroupResponse_arn,
    getRunGroupResponse_creationTime,
    getRunGroupResponse_id,
    getRunGroupResponse_maxCpus,
    getRunGroupResponse_maxDuration,
    getRunGroupResponse_maxRuns,
    getRunGroupResponse_name,
    getRunGroupResponse_tags,
    getRunGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetRunGroup' smart constructor.
data GetRunGroup = GetRunGroup'
  { -- | The group\'s ID.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRunGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getRunGroup_id' - The group\'s ID.
newGetRunGroup ::
  -- | 'id'
  Prelude.Text ->
  GetRunGroup
newGetRunGroup pId_ = GetRunGroup' {id = pId_}

-- | The group\'s ID.
getRunGroup_id :: Lens.Lens' GetRunGroup Prelude.Text
getRunGroup_id = Lens.lens (\GetRunGroup' {id} -> id) (\s@GetRunGroup' {} a -> s {id = a} :: GetRunGroup)

instance Core.AWSRequest GetRunGroup where
  type AWSResponse GetRunGroup = GetRunGroupResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRunGroupResponse'
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "creationTime")
            Prelude.<*> (x Data..?> "id")
            Prelude.<*> (x Data..?> "maxCpus")
            Prelude.<*> (x Data..?> "maxDuration")
            Prelude.<*> (x Data..?> "maxRuns")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetRunGroup where
  hashWithSalt _salt GetRunGroup' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData GetRunGroup where
  rnf GetRunGroup' {..} = Prelude.rnf id

instance Data.ToHeaders GetRunGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetRunGroup where
  toPath GetRunGroup' {..} =
    Prelude.mconcat ["/runGroup/", Data.toBS id]

instance Data.ToQuery GetRunGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetRunGroupResponse' smart constructor.
data GetRunGroupResponse = GetRunGroupResponse'
  { -- | The group\'s ARN.
    arn :: Prelude.Maybe Prelude.Text,
    -- | When the group was created.
    creationTime :: Prelude.Maybe Data.ISO8601,
    -- | The group\'s ID.
    id :: Prelude.Maybe Prelude.Text,
    -- | The group\'s maximum number of CPUs to use.
    maxCpus :: Prelude.Maybe Prelude.Natural,
    -- | The group\'s maximum run duration.
    maxDuration :: Prelude.Maybe Prelude.Natural,
    -- | The maximum number of concurrent runs for the group.
    maxRuns :: Prelude.Maybe Prelude.Natural,
    -- | The group\'s name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The group\'s tags.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRunGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'getRunGroupResponse_arn' - The group\'s ARN.
--
-- 'creationTime', 'getRunGroupResponse_creationTime' - When the group was created.
--
-- 'id', 'getRunGroupResponse_id' - The group\'s ID.
--
-- 'maxCpus', 'getRunGroupResponse_maxCpus' - The group\'s maximum number of CPUs to use.
--
-- 'maxDuration', 'getRunGroupResponse_maxDuration' - The group\'s maximum run duration.
--
-- 'maxRuns', 'getRunGroupResponse_maxRuns' - The maximum number of concurrent runs for the group.
--
-- 'name', 'getRunGroupResponse_name' - The group\'s name.
--
-- 'tags', 'getRunGroupResponse_tags' - The group\'s tags.
--
-- 'httpStatus', 'getRunGroupResponse_httpStatus' - The response's http status code.
newGetRunGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetRunGroupResponse
newGetRunGroupResponse pHttpStatus_ =
  GetRunGroupResponse'
    { arn = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      id = Prelude.Nothing,
      maxCpus = Prelude.Nothing,
      maxDuration = Prelude.Nothing,
      maxRuns = Prelude.Nothing,
      name = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The group\'s ARN.
getRunGroupResponse_arn :: Lens.Lens' GetRunGroupResponse (Prelude.Maybe Prelude.Text)
getRunGroupResponse_arn = Lens.lens (\GetRunGroupResponse' {arn} -> arn) (\s@GetRunGroupResponse' {} a -> s {arn = a} :: GetRunGroupResponse)

-- | When the group was created.
getRunGroupResponse_creationTime :: Lens.Lens' GetRunGroupResponse (Prelude.Maybe Prelude.UTCTime)
getRunGroupResponse_creationTime = Lens.lens (\GetRunGroupResponse' {creationTime} -> creationTime) (\s@GetRunGroupResponse' {} a -> s {creationTime = a} :: GetRunGroupResponse) Prelude.. Lens.mapping Data._Time

-- | The group\'s ID.
getRunGroupResponse_id :: Lens.Lens' GetRunGroupResponse (Prelude.Maybe Prelude.Text)
getRunGroupResponse_id = Lens.lens (\GetRunGroupResponse' {id} -> id) (\s@GetRunGroupResponse' {} a -> s {id = a} :: GetRunGroupResponse)

-- | The group\'s maximum number of CPUs to use.
getRunGroupResponse_maxCpus :: Lens.Lens' GetRunGroupResponse (Prelude.Maybe Prelude.Natural)
getRunGroupResponse_maxCpus = Lens.lens (\GetRunGroupResponse' {maxCpus} -> maxCpus) (\s@GetRunGroupResponse' {} a -> s {maxCpus = a} :: GetRunGroupResponse)

-- | The group\'s maximum run duration.
getRunGroupResponse_maxDuration :: Lens.Lens' GetRunGroupResponse (Prelude.Maybe Prelude.Natural)
getRunGroupResponse_maxDuration = Lens.lens (\GetRunGroupResponse' {maxDuration} -> maxDuration) (\s@GetRunGroupResponse' {} a -> s {maxDuration = a} :: GetRunGroupResponse)

-- | The maximum number of concurrent runs for the group.
getRunGroupResponse_maxRuns :: Lens.Lens' GetRunGroupResponse (Prelude.Maybe Prelude.Natural)
getRunGroupResponse_maxRuns = Lens.lens (\GetRunGroupResponse' {maxRuns} -> maxRuns) (\s@GetRunGroupResponse' {} a -> s {maxRuns = a} :: GetRunGroupResponse)

-- | The group\'s name.
getRunGroupResponse_name :: Lens.Lens' GetRunGroupResponse (Prelude.Maybe Prelude.Text)
getRunGroupResponse_name = Lens.lens (\GetRunGroupResponse' {name} -> name) (\s@GetRunGroupResponse' {} a -> s {name = a} :: GetRunGroupResponse)

-- | The group\'s tags.
getRunGroupResponse_tags :: Lens.Lens' GetRunGroupResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getRunGroupResponse_tags = Lens.lens (\GetRunGroupResponse' {tags} -> tags) (\s@GetRunGroupResponse' {} a -> s {tags = a} :: GetRunGroupResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getRunGroupResponse_httpStatus :: Lens.Lens' GetRunGroupResponse Prelude.Int
getRunGroupResponse_httpStatus = Lens.lens (\GetRunGroupResponse' {httpStatus} -> httpStatus) (\s@GetRunGroupResponse' {} a -> s {httpStatus = a} :: GetRunGroupResponse)

instance Prelude.NFData GetRunGroupResponse where
  rnf GetRunGroupResponse' {..} =
    Prelude.rnf arn `Prelude.seq`
      Prelude.rnf creationTime `Prelude.seq`
        Prelude.rnf id `Prelude.seq`
          Prelude.rnf maxCpus `Prelude.seq`
            Prelude.rnf maxDuration `Prelude.seq`
              Prelude.rnf maxRuns `Prelude.seq`
                Prelude.rnf name `Prelude.seq`
                  Prelude.rnf tags `Prelude.seq`
                    Prelude.rnf httpStatus
