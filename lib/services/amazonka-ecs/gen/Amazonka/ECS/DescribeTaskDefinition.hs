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
-- Module      : Amazonka.ECS.DescribeTaskDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a task definition. You can specify a @family@ and @revision@
-- to find information about a specific task definition, or you can simply
-- specify the family to find the latest @ACTIVE@ revision in that family.
--
-- You can only describe @INACTIVE@ task definitions while an active task
-- or service references them.
module Amazonka.ECS.DescribeTaskDefinition
  ( -- * Creating a Request
    DescribeTaskDefinition (..),
    newDescribeTaskDefinition,

    -- * Request Lenses
    describeTaskDefinition_include,
    describeTaskDefinition_taskDefinition,

    -- * Destructuring the Response
    DescribeTaskDefinitionResponse (..),
    newDescribeTaskDefinitionResponse,

    -- * Response Lenses
    describeTaskDefinitionResponse_tags,
    describeTaskDefinitionResponse_taskDefinition,
    describeTaskDefinitionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeTaskDefinition' smart constructor.
data DescribeTaskDefinition = DescribeTaskDefinition'
  { -- | Determines whether to see the resource tags for the task definition. If
    -- @TAGS@ is specified, the tags are included in the response. If this
    -- field is omitted, tags aren\'t included in the response.
    include :: Prelude.Maybe [TaskDefinitionField],
    -- | The @family@ for the latest @ACTIVE@ revision, @family@ and @revision@
    -- (@family:revision@) for a specific revision in the family, or full
    -- Amazon Resource Name (ARN) of the task definition to describe.
    taskDefinition :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTaskDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'include', 'describeTaskDefinition_include' - Determines whether to see the resource tags for the task definition. If
-- @TAGS@ is specified, the tags are included in the response. If this
-- field is omitted, tags aren\'t included in the response.
--
-- 'taskDefinition', 'describeTaskDefinition_taskDefinition' - The @family@ for the latest @ACTIVE@ revision, @family@ and @revision@
-- (@family:revision@) for a specific revision in the family, or full
-- Amazon Resource Name (ARN) of the task definition to describe.
newDescribeTaskDefinition ::
  -- | 'taskDefinition'
  Prelude.Text ->
  DescribeTaskDefinition
newDescribeTaskDefinition pTaskDefinition_ =
  DescribeTaskDefinition'
    { include = Prelude.Nothing,
      taskDefinition = pTaskDefinition_
    }

-- | Determines whether to see the resource tags for the task definition. If
-- @TAGS@ is specified, the tags are included in the response. If this
-- field is omitted, tags aren\'t included in the response.
describeTaskDefinition_include :: Lens.Lens' DescribeTaskDefinition (Prelude.Maybe [TaskDefinitionField])
describeTaskDefinition_include = Lens.lens (\DescribeTaskDefinition' {include} -> include) (\s@DescribeTaskDefinition' {} a -> s {include = a} :: DescribeTaskDefinition) Prelude.. Lens.mapping Lens.coerced

-- | The @family@ for the latest @ACTIVE@ revision, @family@ and @revision@
-- (@family:revision@) for a specific revision in the family, or full
-- Amazon Resource Name (ARN) of the task definition to describe.
describeTaskDefinition_taskDefinition :: Lens.Lens' DescribeTaskDefinition Prelude.Text
describeTaskDefinition_taskDefinition = Lens.lens (\DescribeTaskDefinition' {taskDefinition} -> taskDefinition) (\s@DescribeTaskDefinition' {} a -> s {taskDefinition = a} :: DescribeTaskDefinition)

instance Core.AWSRequest DescribeTaskDefinition where
  type
    AWSResponse DescribeTaskDefinition =
      DescribeTaskDefinitionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTaskDefinitionResponse'
            Prelude.<$> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "taskDefinition")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeTaskDefinition where
  hashWithSalt _salt DescribeTaskDefinition' {..} =
    _salt
      `Prelude.hashWithSalt` include
      `Prelude.hashWithSalt` taskDefinition

instance Prelude.NFData DescribeTaskDefinition where
  rnf DescribeTaskDefinition' {..} =
    Prelude.rnf include
      `Prelude.seq` Prelude.rnf taskDefinition

instance Data.ToHeaders DescribeTaskDefinition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonEC2ContainerServiceV20141113.DescribeTaskDefinition" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeTaskDefinition where
  toJSON DescribeTaskDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("include" Data..=) Prelude.<$> include,
            Prelude.Just
              ("taskDefinition" Data..= taskDefinition)
          ]
      )

instance Data.ToPath DescribeTaskDefinition where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeTaskDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeTaskDefinitionResponse' smart constructor.
data DescribeTaskDefinitionResponse = DescribeTaskDefinitionResponse'
  { -- | The metadata that\'s applied to the task definition to help you
    -- categorize and organize them. Each tag consists of a key and an optional
    -- value. You define both.
    --
    -- The following basic restrictions apply to tags:
    --
    -- -   Maximum number of tags per resource - 50
    --
    -- -   For each resource, each tag key must be unique, and each tag key can
    --     have only one value.
    --
    -- -   Maximum key length - 128 Unicode characters in UTF-8
    --
    -- -   Maximum value length - 256 Unicode characters in UTF-8
    --
    -- -   If your tagging schema is used across multiple services and
    --     resources, remember that other services may have restrictions on
    --     allowed characters. Generally allowed characters are: letters,
    --     numbers, and spaces representable in UTF-8, and the following
    --     characters: + - = . _ : \/ \@.
    --
    -- -   Tag keys and values are case-sensitive.
    --
    -- -   Do not use @aws:@, @AWS:@, or any upper or lowercase combination of
    --     such as a prefix for either keys or values as it is reserved for
    --     Amazon Web Services use. You cannot edit or delete tag keys or
    --     values with this prefix. Tags with this prefix do not count against
    --     your tags per resource limit.
    tags :: Prelude.Maybe [Tag],
    -- | The full task definition description.
    taskDefinition :: Prelude.Maybe TaskDefinition,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTaskDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'describeTaskDefinitionResponse_tags' - The metadata that\'s applied to the task definition to help you
-- categorize and organize them. Each tag consists of a key and an optional
-- value. You define both.
--
-- The following basic restrictions apply to tags:
--
-- -   Maximum number of tags per resource - 50
--
-- -   For each resource, each tag key must be unique, and each tag key can
--     have only one value.
--
-- -   Maximum key length - 128 Unicode characters in UTF-8
--
-- -   Maximum value length - 256 Unicode characters in UTF-8
--
-- -   If your tagging schema is used across multiple services and
--     resources, remember that other services may have restrictions on
--     allowed characters. Generally allowed characters are: letters,
--     numbers, and spaces representable in UTF-8, and the following
--     characters: + - = . _ : \/ \@.
--
-- -   Tag keys and values are case-sensitive.
--
-- -   Do not use @aws:@, @AWS:@, or any upper or lowercase combination of
--     such as a prefix for either keys or values as it is reserved for
--     Amazon Web Services use. You cannot edit or delete tag keys or
--     values with this prefix. Tags with this prefix do not count against
--     your tags per resource limit.
--
-- 'taskDefinition', 'describeTaskDefinitionResponse_taskDefinition' - The full task definition description.
--
-- 'httpStatus', 'describeTaskDefinitionResponse_httpStatus' - The response's http status code.
newDescribeTaskDefinitionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeTaskDefinitionResponse
newDescribeTaskDefinitionResponse pHttpStatus_ =
  DescribeTaskDefinitionResponse'
    { tags =
        Prelude.Nothing,
      taskDefinition = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The metadata that\'s applied to the task definition to help you
-- categorize and organize them. Each tag consists of a key and an optional
-- value. You define both.
--
-- The following basic restrictions apply to tags:
--
-- -   Maximum number of tags per resource - 50
--
-- -   For each resource, each tag key must be unique, and each tag key can
--     have only one value.
--
-- -   Maximum key length - 128 Unicode characters in UTF-8
--
-- -   Maximum value length - 256 Unicode characters in UTF-8
--
-- -   If your tagging schema is used across multiple services and
--     resources, remember that other services may have restrictions on
--     allowed characters. Generally allowed characters are: letters,
--     numbers, and spaces representable in UTF-8, and the following
--     characters: + - = . _ : \/ \@.
--
-- -   Tag keys and values are case-sensitive.
--
-- -   Do not use @aws:@, @AWS:@, or any upper or lowercase combination of
--     such as a prefix for either keys or values as it is reserved for
--     Amazon Web Services use. You cannot edit or delete tag keys or
--     values with this prefix. Tags with this prefix do not count against
--     your tags per resource limit.
describeTaskDefinitionResponse_tags :: Lens.Lens' DescribeTaskDefinitionResponse (Prelude.Maybe [Tag])
describeTaskDefinitionResponse_tags = Lens.lens (\DescribeTaskDefinitionResponse' {tags} -> tags) (\s@DescribeTaskDefinitionResponse' {} a -> s {tags = a} :: DescribeTaskDefinitionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The full task definition description.
describeTaskDefinitionResponse_taskDefinition :: Lens.Lens' DescribeTaskDefinitionResponse (Prelude.Maybe TaskDefinition)
describeTaskDefinitionResponse_taskDefinition = Lens.lens (\DescribeTaskDefinitionResponse' {taskDefinition} -> taskDefinition) (\s@DescribeTaskDefinitionResponse' {} a -> s {taskDefinition = a} :: DescribeTaskDefinitionResponse)

-- | The response's http status code.
describeTaskDefinitionResponse_httpStatus :: Lens.Lens' DescribeTaskDefinitionResponse Prelude.Int
describeTaskDefinitionResponse_httpStatus = Lens.lens (\DescribeTaskDefinitionResponse' {httpStatus} -> httpStatus) (\s@DescribeTaskDefinitionResponse' {} a -> s {httpStatus = a} :: DescribeTaskDefinitionResponse)

instance
  Prelude.NFData
    DescribeTaskDefinitionResponse
  where
  rnf DescribeTaskDefinitionResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf taskDefinition
      `Prelude.seq` Prelude.rnf httpStatus
