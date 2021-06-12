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
-- Module      : Network.AWS.ECS.DescribeTaskDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a task definition. You can specify a @family@ and @revision@
-- to find information about a specific task definition, or you can simply
-- specify the family to find the latest @ACTIVE@ revision in that family.
--
-- You can only describe @INACTIVE@ task definitions while an active task
-- or service references them.
module Network.AWS.ECS.DescribeTaskDefinition
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

import qualified Network.AWS.Core as Core
import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeTaskDefinition' smart constructor.
data DescribeTaskDefinition = DescribeTaskDefinition'
  { -- | Specifies whether to see the resource tags for the task definition. If
    -- @TAGS@ is specified, the tags are included in the response. If this
    -- field is omitted, tags are not included in the response.
    include :: Core.Maybe [TaskDefinitionField],
    -- | The @family@ for the latest @ACTIVE@ revision, @family@ and @revision@
    -- (@family:revision@) for a specific revision in the family, or full
    -- Amazon Resource Name (ARN) of the task definition to describe.
    taskDefinition :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeTaskDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'include', 'describeTaskDefinition_include' - Specifies whether to see the resource tags for the task definition. If
-- @TAGS@ is specified, the tags are included in the response. If this
-- field is omitted, tags are not included in the response.
--
-- 'taskDefinition', 'describeTaskDefinition_taskDefinition' - The @family@ for the latest @ACTIVE@ revision, @family@ and @revision@
-- (@family:revision@) for a specific revision in the family, or full
-- Amazon Resource Name (ARN) of the task definition to describe.
newDescribeTaskDefinition ::
  -- | 'taskDefinition'
  Core.Text ->
  DescribeTaskDefinition
newDescribeTaskDefinition pTaskDefinition_ =
  DescribeTaskDefinition'
    { include = Core.Nothing,
      taskDefinition = pTaskDefinition_
    }

-- | Specifies whether to see the resource tags for the task definition. If
-- @TAGS@ is specified, the tags are included in the response. If this
-- field is omitted, tags are not included in the response.
describeTaskDefinition_include :: Lens.Lens' DescribeTaskDefinition (Core.Maybe [TaskDefinitionField])
describeTaskDefinition_include = Lens.lens (\DescribeTaskDefinition' {include} -> include) (\s@DescribeTaskDefinition' {} a -> s {include = a} :: DescribeTaskDefinition) Core.. Lens.mapping Lens._Coerce

-- | The @family@ for the latest @ACTIVE@ revision, @family@ and @revision@
-- (@family:revision@) for a specific revision in the family, or full
-- Amazon Resource Name (ARN) of the task definition to describe.
describeTaskDefinition_taskDefinition :: Lens.Lens' DescribeTaskDefinition Core.Text
describeTaskDefinition_taskDefinition = Lens.lens (\DescribeTaskDefinition' {taskDefinition} -> taskDefinition) (\s@DescribeTaskDefinition' {} a -> s {taskDefinition = a} :: DescribeTaskDefinition)

instance Core.AWSRequest DescribeTaskDefinition where
  type
    AWSResponse DescribeTaskDefinition =
      DescribeTaskDefinitionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTaskDefinitionResponse'
            Core.<$> (x Core..?> "tags" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "taskDefinition")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeTaskDefinition

instance Core.NFData DescribeTaskDefinition

instance Core.ToHeaders DescribeTaskDefinition where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerServiceV20141113.DescribeTaskDefinition" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeTaskDefinition where
  toJSON DescribeTaskDefinition' {..} =
    Core.object
      ( Core.catMaybes
          [ ("include" Core..=) Core.<$> include,
            Core.Just ("taskDefinition" Core..= taskDefinition)
          ]
      )

instance Core.ToPath DescribeTaskDefinition where
  toPath = Core.const "/"

instance Core.ToQuery DescribeTaskDefinition where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeTaskDefinitionResponse' smart constructor.
data DescribeTaskDefinitionResponse = DescribeTaskDefinitionResponse'
  { -- | The metadata that is applied to the task definition to help you
    -- categorize and organize them. Each tag consists of a key and an optional
    -- value, both of which you define.
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
    --     such as a prefix for either keys or values as it is reserved for AWS
    --     use. You cannot edit or delete tag keys or values with this prefix.
    --     Tags with this prefix do not count against your tags per resource
    --     limit.
    tags :: Core.Maybe [Tag],
    -- | The full task definition description.
    taskDefinition :: Core.Maybe TaskDefinition,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeTaskDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'describeTaskDefinitionResponse_tags' - The metadata that is applied to the task definition to help you
-- categorize and organize them. Each tag consists of a key and an optional
-- value, both of which you define.
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
--     such as a prefix for either keys or values as it is reserved for AWS
--     use. You cannot edit or delete tag keys or values with this prefix.
--     Tags with this prefix do not count against your tags per resource
--     limit.
--
-- 'taskDefinition', 'describeTaskDefinitionResponse_taskDefinition' - The full task definition description.
--
-- 'httpStatus', 'describeTaskDefinitionResponse_httpStatus' - The response's http status code.
newDescribeTaskDefinitionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeTaskDefinitionResponse
newDescribeTaskDefinitionResponse pHttpStatus_ =
  DescribeTaskDefinitionResponse'
    { tags =
        Core.Nothing,
      taskDefinition = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The metadata that is applied to the task definition to help you
-- categorize and organize them. Each tag consists of a key and an optional
-- value, both of which you define.
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
--     such as a prefix for either keys or values as it is reserved for AWS
--     use. You cannot edit or delete tag keys or values with this prefix.
--     Tags with this prefix do not count against your tags per resource
--     limit.
describeTaskDefinitionResponse_tags :: Lens.Lens' DescribeTaskDefinitionResponse (Core.Maybe [Tag])
describeTaskDefinitionResponse_tags = Lens.lens (\DescribeTaskDefinitionResponse' {tags} -> tags) (\s@DescribeTaskDefinitionResponse' {} a -> s {tags = a} :: DescribeTaskDefinitionResponse) Core.. Lens.mapping Lens._Coerce

-- | The full task definition description.
describeTaskDefinitionResponse_taskDefinition :: Lens.Lens' DescribeTaskDefinitionResponse (Core.Maybe TaskDefinition)
describeTaskDefinitionResponse_taskDefinition = Lens.lens (\DescribeTaskDefinitionResponse' {taskDefinition} -> taskDefinition) (\s@DescribeTaskDefinitionResponse' {} a -> s {taskDefinition = a} :: DescribeTaskDefinitionResponse)

-- | The response's http status code.
describeTaskDefinitionResponse_httpStatus :: Lens.Lens' DescribeTaskDefinitionResponse Core.Int
describeTaskDefinitionResponse_httpStatus = Lens.lens (\DescribeTaskDefinitionResponse' {httpStatus} -> httpStatus) (\s@DescribeTaskDefinitionResponse' {} a -> s {httpStatus = a} :: DescribeTaskDefinitionResponse)

instance Core.NFData DescribeTaskDefinitionResponse
