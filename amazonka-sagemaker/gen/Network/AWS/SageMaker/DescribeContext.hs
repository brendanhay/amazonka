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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDescribeContext' smart constructor.
data DescribeContext = DescribeContext'
  { -- | The name of the context to describe.
    contextName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DescribeContext
newDescribeContext pContextName_ =
  DescribeContext' {contextName = pContextName_}

-- | The name of the context to describe.
describeContext_contextName :: Lens.Lens' DescribeContext Prelude.Text
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
            Prelude.<$> (x Core..?> "ContextType")
            Prelude.<*> (x Core..?> "CreationTime")
            Prelude.<*> (x Core..?> "ContextName")
            Prelude.<*> (x Core..?> "Source")
            Prelude.<*> (x Core..?> "Properties" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "LastModifiedTime")
            Prelude.<*> (x Core..?> "Description")
            Prelude.<*> (x Core..?> "CreatedBy")
            Prelude.<*> (x Core..?> "LastModifiedBy")
            Prelude.<*> (x Core..?> "ContextArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeContext

instance Prelude.NFData DescribeContext

instance Core.ToHeaders DescribeContext where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.DescribeContext" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeContext where
  toJSON DescribeContext' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("ContextName" Core..= contextName)]
      )

instance Core.ToPath DescribeContext where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeContext where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeContextResponse' smart constructor.
data DescribeContextResponse = DescribeContextResponse'
  { -- | The type of the context.
    contextType :: Prelude.Maybe Prelude.Text,
    -- | When the context was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The name of the context.
    contextName :: Prelude.Maybe Prelude.Text,
    -- | The source of the context.
    source :: Prelude.Maybe ContextSource,
    -- | A list of the context\'s properties.
    properties :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | When the context was last modified.
    lastModifiedTime :: Prelude.Maybe Core.POSIX,
    -- | The description of the context.
    description :: Prelude.Maybe Prelude.Text,
    createdBy :: Prelude.Maybe UserContext,
    lastModifiedBy :: Prelude.Maybe UserContext,
    -- | The Amazon Resource Name (ARN) of the context.
    contextArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeContextResponse
newDescribeContextResponse pHttpStatus_ =
  DescribeContextResponse'
    { contextType =
        Prelude.Nothing,
      creationTime = Prelude.Nothing,
      contextName = Prelude.Nothing,
      source = Prelude.Nothing,
      properties = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      description = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      lastModifiedBy = Prelude.Nothing,
      contextArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The type of the context.
describeContextResponse_contextType :: Lens.Lens' DescribeContextResponse (Prelude.Maybe Prelude.Text)
describeContextResponse_contextType = Lens.lens (\DescribeContextResponse' {contextType} -> contextType) (\s@DescribeContextResponse' {} a -> s {contextType = a} :: DescribeContextResponse)

-- | When the context was created.
describeContextResponse_creationTime :: Lens.Lens' DescribeContextResponse (Prelude.Maybe Prelude.UTCTime)
describeContextResponse_creationTime = Lens.lens (\DescribeContextResponse' {creationTime} -> creationTime) (\s@DescribeContextResponse' {} a -> s {creationTime = a} :: DescribeContextResponse) Prelude.. Lens.mapping Core._Time

-- | The name of the context.
describeContextResponse_contextName :: Lens.Lens' DescribeContextResponse (Prelude.Maybe Prelude.Text)
describeContextResponse_contextName = Lens.lens (\DescribeContextResponse' {contextName} -> contextName) (\s@DescribeContextResponse' {} a -> s {contextName = a} :: DescribeContextResponse)

-- | The source of the context.
describeContextResponse_source :: Lens.Lens' DescribeContextResponse (Prelude.Maybe ContextSource)
describeContextResponse_source = Lens.lens (\DescribeContextResponse' {source} -> source) (\s@DescribeContextResponse' {} a -> s {source = a} :: DescribeContextResponse)

-- | A list of the context\'s properties.
describeContextResponse_properties :: Lens.Lens' DescribeContextResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeContextResponse_properties = Lens.lens (\DescribeContextResponse' {properties} -> properties) (\s@DescribeContextResponse' {} a -> s {properties = a} :: DescribeContextResponse) Prelude.. Lens.mapping Lens._Coerce

-- | When the context was last modified.
describeContextResponse_lastModifiedTime :: Lens.Lens' DescribeContextResponse (Prelude.Maybe Prelude.UTCTime)
describeContextResponse_lastModifiedTime = Lens.lens (\DescribeContextResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeContextResponse' {} a -> s {lastModifiedTime = a} :: DescribeContextResponse) Prelude.. Lens.mapping Core._Time

-- | The description of the context.
describeContextResponse_description :: Lens.Lens' DescribeContextResponse (Prelude.Maybe Prelude.Text)
describeContextResponse_description = Lens.lens (\DescribeContextResponse' {description} -> description) (\s@DescribeContextResponse' {} a -> s {description = a} :: DescribeContextResponse)

-- | Undocumented member.
describeContextResponse_createdBy :: Lens.Lens' DescribeContextResponse (Prelude.Maybe UserContext)
describeContextResponse_createdBy = Lens.lens (\DescribeContextResponse' {createdBy} -> createdBy) (\s@DescribeContextResponse' {} a -> s {createdBy = a} :: DescribeContextResponse)

-- | Undocumented member.
describeContextResponse_lastModifiedBy :: Lens.Lens' DescribeContextResponse (Prelude.Maybe UserContext)
describeContextResponse_lastModifiedBy = Lens.lens (\DescribeContextResponse' {lastModifiedBy} -> lastModifiedBy) (\s@DescribeContextResponse' {} a -> s {lastModifiedBy = a} :: DescribeContextResponse)

-- | The Amazon Resource Name (ARN) of the context.
describeContextResponse_contextArn :: Lens.Lens' DescribeContextResponse (Prelude.Maybe Prelude.Text)
describeContextResponse_contextArn = Lens.lens (\DescribeContextResponse' {contextArn} -> contextArn) (\s@DescribeContextResponse' {} a -> s {contextArn = a} :: DescribeContextResponse)

-- | The response's http status code.
describeContextResponse_httpStatus :: Lens.Lens' DescribeContextResponse Prelude.Int
describeContextResponse_httpStatus = Lens.lens (\DescribeContextResponse' {httpStatus} -> httpStatus) (\s@DescribeContextResponse' {} a -> s {httpStatus = a} :: DescribeContextResponse)

instance Prelude.NFData DescribeContextResponse
