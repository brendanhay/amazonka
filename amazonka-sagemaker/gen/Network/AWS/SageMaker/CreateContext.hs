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
-- Module      : Network.AWS.SageMaker.CreateContext
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a /context/. A context is a lineage tracking entity that
-- represents a logical grouping of other tracking or experiment entities.
-- Some examples are an endpoint and a model package. For more information,
-- see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/lineage-tracking.html Amazon SageMaker ML Lineage Tracking>.
module Network.AWS.SageMaker.CreateContext
  ( -- * Creating a Request
    CreateContext (..),
    newCreateContext,

    -- * Request Lenses
    createContext_tags,
    createContext_properties,
    createContext_description,
    createContext_contextName,
    createContext_source,
    createContext_contextType,

    -- * Destructuring the Response
    CreateContextResponse (..),
    newCreateContextResponse,

    -- * Response Lenses
    createContextResponse_contextArn,
    createContextResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newCreateContext' smart constructor.
data CreateContext = CreateContext'
  { -- | A list of tags to apply to the context.
    tags :: Core.Maybe [Tag],
    -- | A list of properties to add to the context.
    properties :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The description of the context.
    description :: Core.Maybe Core.Text,
    -- | The name of the context. Must be unique to your account in an AWS
    -- Region.
    contextName :: Core.Text,
    -- | The source type, ID, and URI.
    source :: ContextSource,
    -- | The context type.
    contextType :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateContext' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createContext_tags' - A list of tags to apply to the context.
--
-- 'properties', 'createContext_properties' - A list of properties to add to the context.
--
-- 'description', 'createContext_description' - The description of the context.
--
-- 'contextName', 'createContext_contextName' - The name of the context. Must be unique to your account in an AWS
-- Region.
--
-- 'source', 'createContext_source' - The source type, ID, and URI.
--
-- 'contextType', 'createContext_contextType' - The context type.
newCreateContext ::
  -- | 'contextName'
  Core.Text ->
  -- | 'source'
  ContextSource ->
  -- | 'contextType'
  Core.Text ->
  CreateContext
newCreateContext pContextName_ pSource_ pContextType_ =
  CreateContext'
    { tags = Core.Nothing,
      properties = Core.Nothing,
      description = Core.Nothing,
      contextName = pContextName_,
      source = pSource_,
      contextType = pContextType_
    }

-- | A list of tags to apply to the context.
createContext_tags :: Lens.Lens' CreateContext (Core.Maybe [Tag])
createContext_tags = Lens.lens (\CreateContext' {tags} -> tags) (\s@CreateContext' {} a -> s {tags = a} :: CreateContext) Core.. Lens.mapping Lens._Coerce

-- | A list of properties to add to the context.
createContext_properties :: Lens.Lens' CreateContext (Core.Maybe (Core.HashMap Core.Text Core.Text))
createContext_properties = Lens.lens (\CreateContext' {properties} -> properties) (\s@CreateContext' {} a -> s {properties = a} :: CreateContext) Core.. Lens.mapping Lens._Coerce

-- | The description of the context.
createContext_description :: Lens.Lens' CreateContext (Core.Maybe Core.Text)
createContext_description = Lens.lens (\CreateContext' {description} -> description) (\s@CreateContext' {} a -> s {description = a} :: CreateContext)

-- | The name of the context. Must be unique to your account in an AWS
-- Region.
createContext_contextName :: Lens.Lens' CreateContext Core.Text
createContext_contextName = Lens.lens (\CreateContext' {contextName} -> contextName) (\s@CreateContext' {} a -> s {contextName = a} :: CreateContext)

-- | The source type, ID, and URI.
createContext_source :: Lens.Lens' CreateContext ContextSource
createContext_source = Lens.lens (\CreateContext' {source} -> source) (\s@CreateContext' {} a -> s {source = a} :: CreateContext)

-- | The context type.
createContext_contextType :: Lens.Lens' CreateContext Core.Text
createContext_contextType = Lens.lens (\CreateContext' {contextType} -> contextType) (\s@CreateContext' {} a -> s {contextType = a} :: CreateContext)

instance Core.AWSRequest CreateContext where
  type
    AWSResponse CreateContext =
      CreateContextResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateContextResponse'
            Core.<$> (x Core..?> "ContextArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateContext

instance Core.NFData CreateContext

instance Core.ToHeaders CreateContext where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.CreateContext" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateContext where
  toJSON CreateContext' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Tags" Core..=) Core.<$> tags,
            ("Properties" Core..=) Core.<$> properties,
            ("Description" Core..=) Core.<$> description,
            Core.Just ("ContextName" Core..= contextName),
            Core.Just ("Source" Core..= source),
            Core.Just ("ContextType" Core..= contextType)
          ]
      )

instance Core.ToPath CreateContext where
  toPath = Core.const "/"

instance Core.ToQuery CreateContext where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateContextResponse' smart constructor.
data CreateContextResponse = CreateContextResponse'
  { -- | The Amazon Resource Name (ARN) of the context.
    contextArn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateContextResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contextArn', 'createContextResponse_contextArn' - The Amazon Resource Name (ARN) of the context.
--
-- 'httpStatus', 'createContextResponse_httpStatus' - The response's http status code.
newCreateContextResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateContextResponse
newCreateContextResponse pHttpStatus_ =
  CreateContextResponse'
    { contextArn = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the context.
createContextResponse_contextArn :: Lens.Lens' CreateContextResponse (Core.Maybe Core.Text)
createContextResponse_contextArn = Lens.lens (\CreateContextResponse' {contextArn} -> contextArn) (\s@CreateContextResponse' {} a -> s {contextArn = a} :: CreateContextResponse)

-- | The response's http status code.
createContextResponse_httpStatus :: Lens.Lens' CreateContextResponse Core.Int
createContextResponse_httpStatus = Lens.lens (\CreateContextResponse' {httpStatus} -> httpStatus) (\s@CreateContextResponse' {} a -> s {httpStatus = a} :: CreateContextResponse)

instance Core.NFData CreateContextResponse
