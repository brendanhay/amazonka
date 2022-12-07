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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
    describeContextResponse_properties,
    describeContextResponse_contextName,
    describeContextResponse_description,
    describeContextResponse_lastModifiedTime,
    describeContextResponse_source,
    describeContextResponse_lineageGroupArn,
    describeContextResponse_creationTime,
    describeContextResponse_lastModifiedBy,
    describeContextResponse_contextType,
    describeContextResponse_createdBy,
    describeContextResponse_contextArn,
    describeContextResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeContextResponse'
            Prelude.<$> (x Data..?> "Properties" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "ContextName")
            Prelude.<*> (x Data..?> "Description")
            Prelude.<*> (x Data..?> "LastModifiedTime")
            Prelude.<*> (x Data..?> "Source")
            Prelude.<*> (x Data..?> "LineageGroupArn")
            Prelude.<*> (x Data..?> "CreationTime")
            Prelude.<*> (x Data..?> "LastModifiedBy")
            Prelude.<*> (x Data..?> "ContextType")
            Prelude.<*> (x Data..?> "CreatedBy")
            Prelude.<*> (x Data..?> "ContextArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeContext where
  hashWithSalt _salt DescribeContext' {..} =
    _salt `Prelude.hashWithSalt` contextName

instance Prelude.NFData DescribeContext where
  rnf DescribeContext' {..} = Prelude.rnf contextName

instance Data.ToHeaders DescribeContext where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SageMaker.DescribeContext" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeContext where
  toJSON DescribeContext' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ContextName" Data..= contextName)]
      )

instance Data.ToPath DescribeContext where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeContext where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeContextResponse' smart constructor.
data DescribeContextResponse = DescribeContextResponse'
  { -- | A list of the context\'s properties.
    properties :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the context.
    contextName :: Prelude.Maybe Prelude.Text,
    -- | The description of the context.
    description :: Prelude.Maybe Prelude.Text,
    -- | When the context was last modified.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The source of the context.
    source :: Prelude.Maybe ContextSource,
    -- | The Amazon Resource Name (ARN) of the lineage group.
    lineageGroupArn :: Prelude.Maybe Prelude.Text,
    -- | When the context was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    lastModifiedBy :: Prelude.Maybe UserContext,
    -- | The type of the context.
    contextType :: Prelude.Maybe Prelude.Text,
    createdBy :: Prelude.Maybe UserContext,
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
-- 'properties', 'describeContextResponse_properties' - A list of the context\'s properties.
--
-- 'contextName', 'describeContextResponse_contextName' - The name of the context.
--
-- 'description', 'describeContextResponse_description' - The description of the context.
--
-- 'lastModifiedTime', 'describeContextResponse_lastModifiedTime' - When the context was last modified.
--
-- 'source', 'describeContextResponse_source' - The source of the context.
--
-- 'lineageGroupArn', 'describeContextResponse_lineageGroupArn' - The Amazon Resource Name (ARN) of the lineage group.
--
-- 'creationTime', 'describeContextResponse_creationTime' - When the context was created.
--
-- 'lastModifiedBy', 'describeContextResponse_lastModifiedBy' - Undocumented member.
--
-- 'contextType', 'describeContextResponse_contextType' - The type of the context.
--
-- 'createdBy', 'describeContextResponse_createdBy' - Undocumented member.
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
    { properties =
        Prelude.Nothing,
      contextName = Prelude.Nothing,
      description = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      source = Prelude.Nothing,
      lineageGroupArn = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      lastModifiedBy = Prelude.Nothing,
      contextType = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      contextArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of the context\'s properties.
describeContextResponse_properties :: Lens.Lens' DescribeContextResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeContextResponse_properties = Lens.lens (\DescribeContextResponse' {properties} -> properties) (\s@DescribeContextResponse' {} a -> s {properties = a} :: DescribeContextResponse) Prelude.. Lens.mapping Lens.coerced

-- | The name of the context.
describeContextResponse_contextName :: Lens.Lens' DescribeContextResponse (Prelude.Maybe Prelude.Text)
describeContextResponse_contextName = Lens.lens (\DescribeContextResponse' {contextName} -> contextName) (\s@DescribeContextResponse' {} a -> s {contextName = a} :: DescribeContextResponse)

-- | The description of the context.
describeContextResponse_description :: Lens.Lens' DescribeContextResponse (Prelude.Maybe Prelude.Text)
describeContextResponse_description = Lens.lens (\DescribeContextResponse' {description} -> description) (\s@DescribeContextResponse' {} a -> s {description = a} :: DescribeContextResponse)

-- | When the context was last modified.
describeContextResponse_lastModifiedTime :: Lens.Lens' DescribeContextResponse (Prelude.Maybe Prelude.UTCTime)
describeContextResponse_lastModifiedTime = Lens.lens (\DescribeContextResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeContextResponse' {} a -> s {lastModifiedTime = a} :: DescribeContextResponse) Prelude.. Lens.mapping Data._Time

-- | The source of the context.
describeContextResponse_source :: Lens.Lens' DescribeContextResponse (Prelude.Maybe ContextSource)
describeContextResponse_source = Lens.lens (\DescribeContextResponse' {source} -> source) (\s@DescribeContextResponse' {} a -> s {source = a} :: DescribeContextResponse)

-- | The Amazon Resource Name (ARN) of the lineage group.
describeContextResponse_lineageGroupArn :: Lens.Lens' DescribeContextResponse (Prelude.Maybe Prelude.Text)
describeContextResponse_lineageGroupArn = Lens.lens (\DescribeContextResponse' {lineageGroupArn} -> lineageGroupArn) (\s@DescribeContextResponse' {} a -> s {lineageGroupArn = a} :: DescribeContextResponse)

-- | When the context was created.
describeContextResponse_creationTime :: Lens.Lens' DescribeContextResponse (Prelude.Maybe Prelude.UTCTime)
describeContextResponse_creationTime = Lens.lens (\DescribeContextResponse' {creationTime} -> creationTime) (\s@DescribeContextResponse' {} a -> s {creationTime = a} :: DescribeContextResponse) Prelude.. Lens.mapping Data._Time

-- | Undocumented member.
describeContextResponse_lastModifiedBy :: Lens.Lens' DescribeContextResponse (Prelude.Maybe UserContext)
describeContextResponse_lastModifiedBy = Lens.lens (\DescribeContextResponse' {lastModifiedBy} -> lastModifiedBy) (\s@DescribeContextResponse' {} a -> s {lastModifiedBy = a} :: DescribeContextResponse)

-- | The type of the context.
describeContextResponse_contextType :: Lens.Lens' DescribeContextResponse (Prelude.Maybe Prelude.Text)
describeContextResponse_contextType = Lens.lens (\DescribeContextResponse' {contextType} -> contextType) (\s@DescribeContextResponse' {} a -> s {contextType = a} :: DescribeContextResponse)

-- | Undocumented member.
describeContextResponse_createdBy :: Lens.Lens' DescribeContextResponse (Prelude.Maybe UserContext)
describeContextResponse_createdBy = Lens.lens (\DescribeContextResponse' {createdBy} -> createdBy) (\s@DescribeContextResponse' {} a -> s {createdBy = a} :: DescribeContextResponse)

-- | The Amazon Resource Name (ARN) of the context.
describeContextResponse_contextArn :: Lens.Lens' DescribeContextResponse (Prelude.Maybe Prelude.Text)
describeContextResponse_contextArn = Lens.lens (\DescribeContextResponse' {contextArn} -> contextArn) (\s@DescribeContextResponse' {} a -> s {contextArn = a} :: DescribeContextResponse)

-- | The response's http status code.
describeContextResponse_httpStatus :: Lens.Lens' DescribeContextResponse Prelude.Int
describeContextResponse_httpStatus = Lens.lens (\DescribeContextResponse' {httpStatus} -> httpStatus) (\s@DescribeContextResponse' {} a -> s {httpStatus = a} :: DescribeContextResponse)

instance Prelude.NFData DescribeContextResponse where
  rnf DescribeContextResponse' {..} =
    Prelude.rnf properties
      `Prelude.seq` Prelude.rnf contextName
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf lineageGroupArn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf lastModifiedBy
      `Prelude.seq` Prelude.rnf contextType
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf contextArn
      `Prelude.seq` Prelude.rnf httpStatus
