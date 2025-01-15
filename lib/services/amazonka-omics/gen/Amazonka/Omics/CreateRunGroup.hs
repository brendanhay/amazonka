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
-- Module      : Amazonka.Omics.CreateRunGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a run group.
module Amazonka.Omics.CreateRunGroup
  ( -- * Creating a Request
    CreateRunGroup (..),
    newCreateRunGroup,

    -- * Request Lenses
    createRunGroup_maxCpus,
    createRunGroup_maxDuration,
    createRunGroup_maxRuns,
    createRunGroup_name,
    createRunGroup_tags,
    createRunGroup_requestId,

    -- * Destructuring the Response
    CreateRunGroupResponse (..),
    newCreateRunGroupResponse,

    -- * Response Lenses
    createRunGroupResponse_arn,
    createRunGroupResponse_id,
    createRunGroupResponse_tags,
    createRunGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateRunGroup' smart constructor.
data CreateRunGroup = CreateRunGroup'
  { -- | The maximum number of CPUs to use in the group.
    maxCpus :: Prelude.Maybe Prelude.Natural,
    -- | A max duration for the group.
    maxDuration :: Prelude.Maybe Prelude.Natural,
    -- | The maximum number of concurrent runs for the group.
    maxRuns :: Prelude.Maybe Prelude.Natural,
    -- | A name for the group.
    name :: Prelude.Maybe Prelude.Text,
    -- | Tags for the group.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A request ID for the group.
    requestId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRunGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxCpus', 'createRunGroup_maxCpus' - The maximum number of CPUs to use in the group.
--
-- 'maxDuration', 'createRunGroup_maxDuration' - A max duration for the group.
--
-- 'maxRuns', 'createRunGroup_maxRuns' - The maximum number of concurrent runs for the group.
--
-- 'name', 'createRunGroup_name' - A name for the group.
--
-- 'tags', 'createRunGroup_tags' - Tags for the group.
--
-- 'requestId', 'createRunGroup_requestId' - A request ID for the group.
newCreateRunGroup ::
  -- | 'requestId'
  Prelude.Text ->
  CreateRunGroup
newCreateRunGroup pRequestId_ =
  CreateRunGroup'
    { maxCpus = Prelude.Nothing,
      maxDuration = Prelude.Nothing,
      maxRuns = Prelude.Nothing,
      name = Prelude.Nothing,
      tags = Prelude.Nothing,
      requestId = pRequestId_
    }

-- | The maximum number of CPUs to use in the group.
createRunGroup_maxCpus :: Lens.Lens' CreateRunGroup (Prelude.Maybe Prelude.Natural)
createRunGroup_maxCpus = Lens.lens (\CreateRunGroup' {maxCpus} -> maxCpus) (\s@CreateRunGroup' {} a -> s {maxCpus = a} :: CreateRunGroup)

-- | A max duration for the group.
createRunGroup_maxDuration :: Lens.Lens' CreateRunGroup (Prelude.Maybe Prelude.Natural)
createRunGroup_maxDuration = Lens.lens (\CreateRunGroup' {maxDuration} -> maxDuration) (\s@CreateRunGroup' {} a -> s {maxDuration = a} :: CreateRunGroup)

-- | The maximum number of concurrent runs for the group.
createRunGroup_maxRuns :: Lens.Lens' CreateRunGroup (Prelude.Maybe Prelude.Natural)
createRunGroup_maxRuns = Lens.lens (\CreateRunGroup' {maxRuns} -> maxRuns) (\s@CreateRunGroup' {} a -> s {maxRuns = a} :: CreateRunGroup)

-- | A name for the group.
createRunGroup_name :: Lens.Lens' CreateRunGroup (Prelude.Maybe Prelude.Text)
createRunGroup_name = Lens.lens (\CreateRunGroup' {name} -> name) (\s@CreateRunGroup' {} a -> s {name = a} :: CreateRunGroup)

-- | Tags for the group.
createRunGroup_tags :: Lens.Lens' CreateRunGroup (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createRunGroup_tags = Lens.lens (\CreateRunGroup' {tags} -> tags) (\s@CreateRunGroup' {} a -> s {tags = a} :: CreateRunGroup) Prelude.. Lens.mapping Lens.coerced

-- | A request ID for the group.
createRunGroup_requestId :: Lens.Lens' CreateRunGroup Prelude.Text
createRunGroup_requestId = Lens.lens (\CreateRunGroup' {requestId} -> requestId) (\s@CreateRunGroup' {} a -> s {requestId = a} :: CreateRunGroup)

instance Core.AWSRequest CreateRunGroup where
  type
    AWSResponse CreateRunGroup =
      CreateRunGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateRunGroupResponse'
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "id")
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateRunGroup where
  hashWithSalt _salt CreateRunGroup' {..} =
    _salt
      `Prelude.hashWithSalt` maxCpus
      `Prelude.hashWithSalt` maxDuration
      `Prelude.hashWithSalt` maxRuns
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` requestId

instance Prelude.NFData CreateRunGroup where
  rnf CreateRunGroup' {..} =
    Prelude.rnf maxCpus `Prelude.seq`
      Prelude.rnf maxDuration `Prelude.seq`
        Prelude.rnf maxRuns `Prelude.seq`
          Prelude.rnf name `Prelude.seq`
            Prelude.rnf tags `Prelude.seq`
              Prelude.rnf requestId

instance Data.ToHeaders CreateRunGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateRunGroup where
  toJSON CreateRunGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxCpus" Data..=) Prelude.<$> maxCpus,
            ("maxDuration" Data..=) Prelude.<$> maxDuration,
            ("maxRuns" Data..=) Prelude.<$> maxRuns,
            ("name" Data..=) Prelude.<$> name,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("requestId" Data..= requestId)
          ]
      )

instance Data.ToPath CreateRunGroup where
  toPath = Prelude.const "/runGroup"

instance Data.ToQuery CreateRunGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateRunGroupResponse' smart constructor.
data CreateRunGroupResponse = CreateRunGroupResponse'
  { -- | The group\'s ARN.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The group\'s ID.
    id :: Prelude.Maybe Prelude.Text,
    -- | Tags for the run group.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRunGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'createRunGroupResponse_arn' - The group\'s ARN.
--
-- 'id', 'createRunGroupResponse_id' - The group\'s ID.
--
-- 'tags', 'createRunGroupResponse_tags' - Tags for the run group.
--
-- 'httpStatus', 'createRunGroupResponse_httpStatus' - The response's http status code.
newCreateRunGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateRunGroupResponse
newCreateRunGroupResponse pHttpStatus_ =
  CreateRunGroupResponse'
    { arn = Prelude.Nothing,
      id = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The group\'s ARN.
createRunGroupResponse_arn :: Lens.Lens' CreateRunGroupResponse (Prelude.Maybe Prelude.Text)
createRunGroupResponse_arn = Lens.lens (\CreateRunGroupResponse' {arn} -> arn) (\s@CreateRunGroupResponse' {} a -> s {arn = a} :: CreateRunGroupResponse)

-- | The group\'s ID.
createRunGroupResponse_id :: Lens.Lens' CreateRunGroupResponse (Prelude.Maybe Prelude.Text)
createRunGroupResponse_id = Lens.lens (\CreateRunGroupResponse' {id} -> id) (\s@CreateRunGroupResponse' {} a -> s {id = a} :: CreateRunGroupResponse)

-- | Tags for the run group.
createRunGroupResponse_tags :: Lens.Lens' CreateRunGroupResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createRunGroupResponse_tags = Lens.lens (\CreateRunGroupResponse' {tags} -> tags) (\s@CreateRunGroupResponse' {} a -> s {tags = a} :: CreateRunGroupResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createRunGroupResponse_httpStatus :: Lens.Lens' CreateRunGroupResponse Prelude.Int
createRunGroupResponse_httpStatus = Lens.lens (\CreateRunGroupResponse' {httpStatus} -> httpStatus) (\s@CreateRunGroupResponse' {} a -> s {httpStatus = a} :: CreateRunGroupResponse)

instance Prelude.NFData CreateRunGroupResponse where
  rnf CreateRunGroupResponse' {..} =
    Prelude.rnf arn `Prelude.seq`
      Prelude.rnf id `Prelude.seq`
        Prelude.rnf tags `Prelude.seq`
          Prelude.rnf httpStatus
