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
-- Module      : Amazonka.SageMaker.DescribeAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an action.
module Amazonka.SageMaker.DescribeAction
  ( -- * Creating a Request
    DescribeAction (..),
    newDescribeAction,

    -- * Request Lenses
    describeAction_actionName,

    -- * Destructuring the Response
    DescribeActionResponse (..),
    newDescribeActionResponse,

    -- * Response Lenses
    describeActionResponse_creationTime,
    describeActionResponse_metadataProperties,
    describeActionResponse_status,
    describeActionResponse_createdBy,
    describeActionResponse_lastModifiedTime,
    describeActionResponse_actionName,
    describeActionResponse_source,
    describeActionResponse_lastModifiedBy,
    describeActionResponse_actionArn,
    describeActionResponse_description,
    describeActionResponse_actionType,
    describeActionResponse_properties,
    describeActionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDescribeAction' smart constructor.
data DescribeAction = DescribeAction'
  { -- | The name of the action to describe.
    actionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actionName', 'describeAction_actionName' - The name of the action to describe.
newDescribeAction ::
  -- | 'actionName'
  Prelude.Text ->
  DescribeAction
newDescribeAction pActionName_ =
  DescribeAction' {actionName = pActionName_}

-- | The name of the action to describe.
describeAction_actionName :: Lens.Lens' DescribeAction Prelude.Text
describeAction_actionName = Lens.lens (\DescribeAction' {actionName} -> actionName) (\s@DescribeAction' {} a -> s {actionName = a} :: DescribeAction)

instance Core.AWSRequest DescribeAction where
  type
    AWSResponse DescribeAction =
      DescribeActionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeActionResponse'
            Prelude.<$> (x Core..?> "CreationTime")
            Prelude.<*> (x Core..?> "MetadataProperties")
            Prelude.<*> (x Core..?> "Status")
            Prelude.<*> (x Core..?> "CreatedBy")
            Prelude.<*> (x Core..?> "LastModifiedTime")
            Prelude.<*> (x Core..?> "ActionName")
            Prelude.<*> (x Core..?> "Source")
            Prelude.<*> (x Core..?> "LastModifiedBy")
            Prelude.<*> (x Core..?> "ActionArn")
            Prelude.<*> (x Core..?> "Description")
            Prelude.<*> (x Core..?> "ActionType")
            Prelude.<*> (x Core..?> "Properties" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeAction

instance Prelude.NFData DescribeAction

instance Core.ToHeaders DescribeAction where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.DescribeAction" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeAction where
  toJSON DescribeAction' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("ActionName" Core..= actionName)]
      )

instance Core.ToPath DescribeAction where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeAction where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeActionResponse' smart constructor.
data DescribeActionResponse = DescribeActionResponse'
  { -- | When the action was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    metadataProperties :: Prelude.Maybe MetadataProperties,
    -- | The status of the action.
    status :: Prelude.Maybe ActionStatus,
    createdBy :: Prelude.Maybe UserContext,
    -- | When the action was last modified.
    lastModifiedTime :: Prelude.Maybe Core.POSIX,
    -- | The name of the action.
    actionName :: Prelude.Maybe Prelude.Text,
    -- | The source of the action.
    source :: Prelude.Maybe ActionSource,
    lastModifiedBy :: Prelude.Maybe UserContext,
    -- | The Amazon Resource Name (ARN) of the action.
    actionArn :: Prelude.Maybe Prelude.Text,
    -- | The description of the action.
    description :: Prelude.Maybe Prelude.Text,
    -- | The type of the action.
    actionType :: Prelude.Maybe Prelude.Text,
    -- | A list of the action\'s properties.
    properties :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeActionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'describeActionResponse_creationTime' - When the action was created.
--
-- 'metadataProperties', 'describeActionResponse_metadataProperties' - Undocumented member.
--
-- 'status', 'describeActionResponse_status' - The status of the action.
--
-- 'createdBy', 'describeActionResponse_createdBy' - Undocumented member.
--
-- 'lastModifiedTime', 'describeActionResponse_lastModifiedTime' - When the action was last modified.
--
-- 'actionName', 'describeActionResponse_actionName' - The name of the action.
--
-- 'source', 'describeActionResponse_source' - The source of the action.
--
-- 'lastModifiedBy', 'describeActionResponse_lastModifiedBy' - Undocumented member.
--
-- 'actionArn', 'describeActionResponse_actionArn' - The Amazon Resource Name (ARN) of the action.
--
-- 'description', 'describeActionResponse_description' - The description of the action.
--
-- 'actionType', 'describeActionResponse_actionType' - The type of the action.
--
-- 'properties', 'describeActionResponse_properties' - A list of the action\'s properties.
--
-- 'httpStatus', 'describeActionResponse_httpStatus' - The response's http status code.
newDescribeActionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeActionResponse
newDescribeActionResponse pHttpStatus_ =
  DescribeActionResponse'
    { creationTime =
        Prelude.Nothing,
      metadataProperties = Prelude.Nothing,
      status = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      actionName = Prelude.Nothing,
      source = Prelude.Nothing,
      lastModifiedBy = Prelude.Nothing,
      actionArn = Prelude.Nothing,
      description = Prelude.Nothing,
      actionType = Prelude.Nothing,
      properties = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | When the action was created.
describeActionResponse_creationTime :: Lens.Lens' DescribeActionResponse (Prelude.Maybe Prelude.UTCTime)
describeActionResponse_creationTime = Lens.lens (\DescribeActionResponse' {creationTime} -> creationTime) (\s@DescribeActionResponse' {} a -> s {creationTime = a} :: DescribeActionResponse) Prelude.. Lens.mapping Core._Time

-- | Undocumented member.
describeActionResponse_metadataProperties :: Lens.Lens' DescribeActionResponse (Prelude.Maybe MetadataProperties)
describeActionResponse_metadataProperties = Lens.lens (\DescribeActionResponse' {metadataProperties} -> metadataProperties) (\s@DescribeActionResponse' {} a -> s {metadataProperties = a} :: DescribeActionResponse)

-- | The status of the action.
describeActionResponse_status :: Lens.Lens' DescribeActionResponse (Prelude.Maybe ActionStatus)
describeActionResponse_status = Lens.lens (\DescribeActionResponse' {status} -> status) (\s@DescribeActionResponse' {} a -> s {status = a} :: DescribeActionResponse)

-- | Undocumented member.
describeActionResponse_createdBy :: Lens.Lens' DescribeActionResponse (Prelude.Maybe UserContext)
describeActionResponse_createdBy = Lens.lens (\DescribeActionResponse' {createdBy} -> createdBy) (\s@DescribeActionResponse' {} a -> s {createdBy = a} :: DescribeActionResponse)

-- | When the action was last modified.
describeActionResponse_lastModifiedTime :: Lens.Lens' DescribeActionResponse (Prelude.Maybe Prelude.UTCTime)
describeActionResponse_lastModifiedTime = Lens.lens (\DescribeActionResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeActionResponse' {} a -> s {lastModifiedTime = a} :: DescribeActionResponse) Prelude.. Lens.mapping Core._Time

-- | The name of the action.
describeActionResponse_actionName :: Lens.Lens' DescribeActionResponse (Prelude.Maybe Prelude.Text)
describeActionResponse_actionName = Lens.lens (\DescribeActionResponse' {actionName} -> actionName) (\s@DescribeActionResponse' {} a -> s {actionName = a} :: DescribeActionResponse)

-- | The source of the action.
describeActionResponse_source :: Lens.Lens' DescribeActionResponse (Prelude.Maybe ActionSource)
describeActionResponse_source = Lens.lens (\DescribeActionResponse' {source} -> source) (\s@DescribeActionResponse' {} a -> s {source = a} :: DescribeActionResponse)

-- | Undocumented member.
describeActionResponse_lastModifiedBy :: Lens.Lens' DescribeActionResponse (Prelude.Maybe UserContext)
describeActionResponse_lastModifiedBy = Lens.lens (\DescribeActionResponse' {lastModifiedBy} -> lastModifiedBy) (\s@DescribeActionResponse' {} a -> s {lastModifiedBy = a} :: DescribeActionResponse)

-- | The Amazon Resource Name (ARN) of the action.
describeActionResponse_actionArn :: Lens.Lens' DescribeActionResponse (Prelude.Maybe Prelude.Text)
describeActionResponse_actionArn = Lens.lens (\DescribeActionResponse' {actionArn} -> actionArn) (\s@DescribeActionResponse' {} a -> s {actionArn = a} :: DescribeActionResponse)

-- | The description of the action.
describeActionResponse_description :: Lens.Lens' DescribeActionResponse (Prelude.Maybe Prelude.Text)
describeActionResponse_description = Lens.lens (\DescribeActionResponse' {description} -> description) (\s@DescribeActionResponse' {} a -> s {description = a} :: DescribeActionResponse)

-- | The type of the action.
describeActionResponse_actionType :: Lens.Lens' DescribeActionResponse (Prelude.Maybe Prelude.Text)
describeActionResponse_actionType = Lens.lens (\DescribeActionResponse' {actionType} -> actionType) (\s@DescribeActionResponse' {} a -> s {actionType = a} :: DescribeActionResponse)

-- | A list of the action\'s properties.
describeActionResponse_properties :: Lens.Lens' DescribeActionResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeActionResponse_properties = Lens.lens (\DescribeActionResponse' {properties} -> properties) (\s@DescribeActionResponse' {} a -> s {properties = a} :: DescribeActionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeActionResponse_httpStatus :: Lens.Lens' DescribeActionResponse Prelude.Int
describeActionResponse_httpStatus = Lens.lens (\DescribeActionResponse' {httpStatus} -> httpStatus) (\s@DescribeActionResponse' {} a -> s {httpStatus = a} :: DescribeActionResponse)

instance Prelude.NFData DescribeActionResponse
