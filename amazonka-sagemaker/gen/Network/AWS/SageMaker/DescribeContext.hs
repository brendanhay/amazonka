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
-- Module      : Network.AWS.SageMaker.DescribeContext
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a context.
module Network.AWS.SageMaker.DescribeContext
  ( -- * Creating a Request
    DescribeContext (..),
    newDescribeContext,

    -- * Request Lenses
    describeContext_contextName,

    -- * Destructuring the Response
    DescribeContextResponse (..),
    newDescribeContextResponse,

    -- * Response Lenses
    describeContextResponse_contextType,
    describeContextResponse_creationTime,
    describeContextResponse_contextName,
    describeContextResponse_source,
    describeContextResponse_properties,
    describeContextResponse_lastModifiedTime,
    describeContextResponse_description,
    describeContextResponse_createdBy,
    describeContextResponse_lastModifiedBy,
    describeContextResponse_contextArn,
    describeContextResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDescribeContext' smart constructor.
data DescribeContext = DescribeContext'
  { -- | The name of the context to describe.
    contextName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeContext' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contextName', 'describeContext_contextName' - The name of the context to describe.
newDescribeContext ::
  -- | 'contextName'
  Core.Text ->
  DescribeContext
newDescribeContext pContextName_ =
  DescribeContext' {contextName = pContextName_}

-- | The name of the context to describe.
describeContext_contextName :: Lens.Lens' DescribeContext Core.Text
describeContext_contextName = Lens.lens (\DescribeContext' {contextName} -> contextName) (\s@DescribeContext' {} a -> s {contextName = a} :: DescribeContext)

instance Core.AWSRequest DescribeContext where
  type
    AWSResponse DescribeContext =
      DescribeContextResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeContextResponse'
            Core.<$> (x Core..?> "ContextType")
            Core.<*> (x Core..?> "CreationTime")
            Core.<*> (x Core..?> "ContextName")
            Core.<*> (x Core..?> "Source")
            Core.<*> (x Core..?> "Properties" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "LastModifiedTime")
            Core.<*> (x Core..?> "Description")
            Core.<*> (x Core..?> "CreatedBy")
            Core.<*> (x Core..?> "LastModifiedBy")
            Core.<*> (x Core..?> "ContextArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeContext

instance Core.NFData DescribeContext

instance Core.ToHeaders DescribeContext where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.DescribeContext" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeContext where
  toJSON DescribeContext' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("ContextName" Core..= contextName)]
      )

instance Core.ToPath DescribeContext where
  toPath = Core.const "/"

instance Core.ToQuery DescribeContext where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeContextResponse' smart constructor.
data DescribeContextResponse = DescribeContextResponse'
  { -- | The type of the context.
    contextType :: Core.Maybe Core.Text,
    -- | When the context was created.
    creationTime :: Core.Maybe Core.POSIX,
    -- | The name of the context.
    contextName :: Core.Maybe Core.Text,
    -- | The source of the context.
    source :: Core.Maybe ContextSource,
    -- | A list of the context\'s properties.
    properties :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | When the context was last modified.
    lastModifiedTime :: Core.Maybe Core.POSIX,
    -- | The description of the context.
    description :: Core.Maybe Core.Text,
    createdBy :: Core.Maybe UserContext,
    lastModifiedBy :: Core.Maybe UserContext,
    -- | The Amazon Resource Name (ARN) of the context.
    contextArn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeContextResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contextType', 'describeContextResponse_contextType' - The type of the context.
--
-- 'creationTime', 'describeContextResponse_creationTime' - When the context was created.
--
-- 'contextName', 'describeContextResponse_contextName' - The name of the context.
--
-- 'source', 'describeContextResponse_source' - The source of the context.
--
-- 'properties', 'describeContextResponse_properties' - A list of the context\'s properties.
--
-- 'lastModifiedTime', 'describeContextResponse_lastModifiedTime' - When the context was last modified.
--
-- 'description', 'describeContextResponse_description' - The description of the context.
--
-- 'createdBy', 'describeContextResponse_createdBy' - Undocumented member.
--
-- 'lastModifiedBy', 'describeContextResponse_lastModifiedBy' - Undocumented member.
--
-- 'contextArn', 'describeContextResponse_contextArn' - The Amazon Resource Name (ARN) of the context.
--
-- 'httpStatus', 'describeContextResponse_httpStatus' - The response's http status code.
newDescribeContextResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeContextResponse
newDescribeContextResponse pHttpStatus_ =
  DescribeContextResponse'
    { contextType =
        Core.Nothing,
      creationTime = Core.Nothing,
      contextName = Core.Nothing,
      source = Core.Nothing,
      properties = Core.Nothing,
      lastModifiedTime = Core.Nothing,
      description = Core.Nothing,
      createdBy = Core.Nothing,
      lastModifiedBy = Core.Nothing,
      contextArn = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The type of the context.
describeContextResponse_contextType :: Lens.Lens' DescribeContextResponse (Core.Maybe Core.Text)
describeContextResponse_contextType = Lens.lens (\DescribeContextResponse' {contextType} -> contextType) (\s@DescribeContextResponse' {} a -> s {contextType = a} :: DescribeContextResponse)

-- | When the context was created.
describeContextResponse_creationTime :: Lens.Lens' DescribeContextResponse (Core.Maybe Core.UTCTime)
describeContextResponse_creationTime = Lens.lens (\DescribeContextResponse' {creationTime} -> creationTime) (\s@DescribeContextResponse' {} a -> s {creationTime = a} :: DescribeContextResponse) Core.. Lens.mapping Core._Time

-- | The name of the context.
describeContextResponse_contextName :: Lens.Lens' DescribeContextResponse (Core.Maybe Core.Text)
describeContextResponse_contextName = Lens.lens (\DescribeContextResponse' {contextName} -> contextName) (\s@DescribeContextResponse' {} a -> s {contextName = a} :: DescribeContextResponse)

-- | The source of the context.
describeContextResponse_source :: Lens.Lens' DescribeContextResponse (Core.Maybe ContextSource)
describeContextResponse_source = Lens.lens (\DescribeContextResponse' {source} -> source) (\s@DescribeContextResponse' {} a -> s {source = a} :: DescribeContextResponse)

-- | A list of the context\'s properties.
describeContextResponse_properties :: Lens.Lens' DescribeContextResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
describeContextResponse_properties = Lens.lens (\DescribeContextResponse' {properties} -> properties) (\s@DescribeContextResponse' {} a -> s {properties = a} :: DescribeContextResponse) Core.. Lens.mapping Lens._Coerce

-- | When the context was last modified.
describeContextResponse_lastModifiedTime :: Lens.Lens' DescribeContextResponse (Core.Maybe Core.UTCTime)
describeContextResponse_lastModifiedTime = Lens.lens (\DescribeContextResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeContextResponse' {} a -> s {lastModifiedTime = a} :: DescribeContextResponse) Core.. Lens.mapping Core._Time

-- | The description of the context.
describeContextResponse_description :: Lens.Lens' DescribeContextResponse (Core.Maybe Core.Text)
describeContextResponse_description = Lens.lens (\DescribeContextResponse' {description} -> description) (\s@DescribeContextResponse' {} a -> s {description = a} :: DescribeContextResponse)

-- | Undocumented member.
describeContextResponse_createdBy :: Lens.Lens' DescribeContextResponse (Core.Maybe UserContext)
describeContextResponse_createdBy = Lens.lens (\DescribeContextResponse' {createdBy} -> createdBy) (\s@DescribeContextResponse' {} a -> s {createdBy = a} :: DescribeContextResponse)

-- | Undocumented member.
describeContextResponse_lastModifiedBy :: Lens.Lens' DescribeContextResponse (Core.Maybe UserContext)
describeContextResponse_lastModifiedBy = Lens.lens (\DescribeContextResponse' {lastModifiedBy} -> lastModifiedBy) (\s@DescribeContextResponse' {} a -> s {lastModifiedBy = a} :: DescribeContextResponse)

-- | The Amazon Resource Name (ARN) of the context.
describeContextResponse_contextArn :: Lens.Lens' DescribeContextResponse (Core.Maybe Core.Text)
describeContextResponse_contextArn = Lens.lens (\DescribeContextResponse' {contextArn} -> contextArn) (\s@DescribeContextResponse' {} a -> s {contextArn = a} :: DescribeContextResponse)

-- | The response's http status code.
describeContextResponse_httpStatus :: Lens.Lens' DescribeContextResponse Core.Int
describeContextResponse_httpStatus = Lens.lens (\DescribeContextResponse' {httpStatus} -> httpStatus) (\s@DescribeContextResponse' {} a -> s {httpStatus = a} :: DescribeContextResponse)

instance Core.NFData DescribeContextResponse
