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
-- Module      : Amazonka.SageMaker.DescribeContext
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a context.
module Amazonka.SageMaker.DescribeContext
  ( -- * Creating a Request
    DescribeContext (..),
    newDescribeContext,

    -- * Request Lenses
    describeContext_contextName,

    -- * Destructuring the Response
    DescribeContextResponse (..),
    newDescribeContextResponse,

    -- * Response Lenses
    describeContextResponse_creationTime,
    describeContextResponse_createdBy,
    describeContextResponse_lastModifiedTime,
    describeContextResponse_contextType,
    describeContextResponse_contextArn,
    describeContextResponse_source,
    describeContextResponse_contextName,
    describeContextResponse_lastModifiedBy,
    describeContextResponse_description,
    describeContextResponse_properties,
    describeContextResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

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
            Prelude.<$> (x Core..?> "CreationTime")
            Prelude.<*> (x Core..?> "CreatedBy")
            Prelude.<*> (x Core..?> "LastModifiedTime")
            Prelude.<*> (x Core..?> "ContextType")
            Prelude.<*> (x Core..?> "ContextArn")
            Prelude.<*> (x Core..?> "Source")
            Prelude.<*> (x Core..?> "ContextName")
            Prelude.<*> (x Core..?> "LastModifiedBy")
            Prelude.<*> (x Core..?> "Description")
            Prelude.<*> (x Core..?> "Properties" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeContext where
  hashWithSalt salt' DescribeContext' {..} =
    salt' `Prelude.hashWithSalt` contextName

instance Prelude.NFData DescribeContext where
  rnf DescribeContext' {..} = Prelude.rnf contextName

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
  { -- | When the context was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    createdBy :: Prelude.Maybe UserContext,
    -- | When the context was last modified.
    lastModifiedTime :: Prelude.Maybe Core.POSIX,
    -- | The type of the context.
    contextType :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the context.
    contextArn :: Prelude.Maybe Prelude.Text,
    -- | The source of the context.
    source :: Prelude.Maybe ContextSource,
    -- | The name of the context.
    contextName :: Prelude.Maybe Prelude.Text,
    lastModifiedBy :: Prelude.Maybe UserContext,
    -- | The description of the context.
    description :: Prelude.Maybe Prelude.Text,
    -- | A list of the context\'s properties.
    properties :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
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
-- 'creationTime', 'describeContextResponse_creationTime' - When the context was created.
--
-- 'createdBy', 'describeContextResponse_createdBy' - Undocumented member.
--
-- 'lastModifiedTime', 'describeContextResponse_lastModifiedTime' - When the context was last modified.
--
-- 'contextType', 'describeContextResponse_contextType' - The type of the context.
--
-- 'contextArn', 'describeContextResponse_contextArn' - The Amazon Resource Name (ARN) of the context.
--
-- 'source', 'describeContextResponse_source' - The source of the context.
--
-- 'contextName', 'describeContextResponse_contextName' - The name of the context.
--
-- 'lastModifiedBy', 'describeContextResponse_lastModifiedBy' - Undocumented member.
--
-- 'description', 'describeContextResponse_description' - The description of the context.
--
-- 'properties', 'describeContextResponse_properties' - A list of the context\'s properties.
--
-- 'httpStatus', 'describeContextResponse_httpStatus' - The response's http status code.
newDescribeContextResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeContextResponse
newDescribeContextResponse pHttpStatus_ =
  DescribeContextResponse'
    { creationTime =
        Prelude.Nothing,
      createdBy = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      contextType = Prelude.Nothing,
      contextArn = Prelude.Nothing,
      source = Prelude.Nothing,
      contextName = Prelude.Nothing,
      lastModifiedBy = Prelude.Nothing,
      description = Prelude.Nothing,
      properties = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | When the context was created.
describeContextResponse_creationTime :: Lens.Lens' DescribeContextResponse (Prelude.Maybe Prelude.UTCTime)
describeContextResponse_creationTime = Lens.lens (\DescribeContextResponse' {creationTime} -> creationTime) (\s@DescribeContextResponse' {} a -> s {creationTime = a} :: DescribeContextResponse) Prelude.. Lens.mapping Core._Time

-- | Undocumented member.
describeContextResponse_createdBy :: Lens.Lens' DescribeContextResponse (Prelude.Maybe UserContext)
describeContextResponse_createdBy = Lens.lens (\DescribeContextResponse' {createdBy} -> createdBy) (\s@DescribeContextResponse' {} a -> s {createdBy = a} :: DescribeContextResponse)

-- | When the context was last modified.
describeContextResponse_lastModifiedTime :: Lens.Lens' DescribeContextResponse (Prelude.Maybe Prelude.UTCTime)
describeContextResponse_lastModifiedTime = Lens.lens (\DescribeContextResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeContextResponse' {} a -> s {lastModifiedTime = a} :: DescribeContextResponse) Prelude.. Lens.mapping Core._Time

-- | The type of the context.
describeContextResponse_contextType :: Lens.Lens' DescribeContextResponse (Prelude.Maybe Prelude.Text)
describeContextResponse_contextType = Lens.lens (\DescribeContextResponse' {contextType} -> contextType) (\s@DescribeContextResponse' {} a -> s {contextType = a} :: DescribeContextResponse)

-- | The Amazon Resource Name (ARN) of the context.
describeContextResponse_contextArn :: Lens.Lens' DescribeContextResponse (Prelude.Maybe Prelude.Text)
describeContextResponse_contextArn = Lens.lens (\DescribeContextResponse' {contextArn} -> contextArn) (\s@DescribeContextResponse' {} a -> s {contextArn = a} :: DescribeContextResponse)

-- | The source of the context.
describeContextResponse_source :: Lens.Lens' DescribeContextResponse (Prelude.Maybe ContextSource)
describeContextResponse_source = Lens.lens (\DescribeContextResponse' {source} -> source) (\s@DescribeContextResponse' {} a -> s {source = a} :: DescribeContextResponse)

-- | The name of the context.
describeContextResponse_contextName :: Lens.Lens' DescribeContextResponse (Prelude.Maybe Prelude.Text)
describeContextResponse_contextName = Lens.lens (\DescribeContextResponse' {contextName} -> contextName) (\s@DescribeContextResponse' {} a -> s {contextName = a} :: DescribeContextResponse)

-- | Undocumented member.
describeContextResponse_lastModifiedBy :: Lens.Lens' DescribeContextResponse (Prelude.Maybe UserContext)
describeContextResponse_lastModifiedBy = Lens.lens (\DescribeContextResponse' {lastModifiedBy} -> lastModifiedBy) (\s@DescribeContextResponse' {} a -> s {lastModifiedBy = a} :: DescribeContextResponse)

-- | The description of the context.
describeContextResponse_description :: Lens.Lens' DescribeContextResponse (Prelude.Maybe Prelude.Text)
describeContextResponse_description = Lens.lens (\DescribeContextResponse' {description} -> description) (\s@DescribeContextResponse' {} a -> s {description = a} :: DescribeContextResponse)

-- | A list of the context\'s properties.
describeContextResponse_properties :: Lens.Lens' DescribeContextResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeContextResponse_properties = Lens.lens (\DescribeContextResponse' {properties} -> properties) (\s@DescribeContextResponse' {} a -> s {properties = a} :: DescribeContextResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeContextResponse_httpStatus :: Lens.Lens' DescribeContextResponse Prelude.Int
describeContextResponse_httpStatus = Lens.lens (\DescribeContextResponse' {httpStatus} -> httpStatus) (\s@DescribeContextResponse' {} a -> s {httpStatus = a} :: DescribeContextResponse)

instance Prelude.NFData DescribeContextResponse where
  rnf DescribeContextResponse' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf properties
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf lastModifiedBy
      `Prelude.seq` Prelude.rnf contextName
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf contextArn
      `Prelude.seq` Prelude.rnf contextType
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf createdBy
