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
-- Module      : Network.AWS.SageMaker.DescribeAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an action.
module Network.AWS.SageMaker.DescribeAction
  ( -- * Creating a Request
    DescribeAction (..),
    newDescribeAction,

    -- * Request Lenses
    describeAction_actionName,

    -- * Destructuring the Response
    DescribeActionResponse (..),
    newDescribeActionResponse,

    -- * Response Lenses
    describeActionResponse_status,
    describeActionResponse_metadataProperties,
    describeActionResponse_creationTime,
    describeActionResponse_actionName,
    describeActionResponse_actionType,
    describeActionResponse_actionArn,
    describeActionResponse_source,
    describeActionResponse_properties,
    describeActionResponse_lastModifiedTime,
    describeActionResponse_description,
    describeActionResponse_createdBy,
    describeActionResponse_lastModifiedBy,
    describeActionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDescribeAction' smart constructor.
data DescribeAction = DescribeAction'
  { -- | The name of the action to describe.
    actionName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DescribeAction
newDescribeAction pActionName_ =
  DescribeAction' {actionName = pActionName_}

-- | The name of the action to describe.
describeAction_actionName :: Lens.Lens' DescribeAction Core.Text
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
            Core.<$> (x Core..?> "Status")
            Core.<*> (x Core..?> "MetadataProperties")
            Core.<*> (x Core..?> "CreationTime")
            Core.<*> (x Core..?> "ActionName")
            Core.<*> (x Core..?> "ActionType")
            Core.<*> (x Core..?> "ActionArn")
            Core.<*> (x Core..?> "Source")
            Core.<*> (x Core..?> "Properties" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "LastModifiedTime")
            Core.<*> (x Core..?> "Description")
            Core.<*> (x Core..?> "CreatedBy")
            Core.<*> (x Core..?> "LastModifiedBy")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeAction

instance Core.NFData DescribeAction

instance Core.ToHeaders DescribeAction where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.DescribeAction" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeAction where
  toJSON DescribeAction' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("ActionName" Core..= actionName)]
      )

instance Core.ToPath DescribeAction where
  toPath = Core.const "/"

instance Core.ToQuery DescribeAction where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeActionResponse' smart constructor.
data DescribeActionResponse = DescribeActionResponse'
  { -- | The status of the action.
    status :: Core.Maybe ActionStatus,
    metadataProperties :: Core.Maybe MetadataProperties,
    -- | When the action was created.
    creationTime :: Core.Maybe Core.POSIX,
    -- | The name of the action.
    actionName :: Core.Maybe Core.Text,
    -- | The type of the action.
    actionType :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the action.
    actionArn :: Core.Maybe Core.Text,
    -- | The source of the action.
    source :: Core.Maybe ActionSource,
    -- | A list of the action\'s properties.
    properties :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | When the action was last modified.
    lastModifiedTime :: Core.Maybe Core.POSIX,
    -- | The description of the action.
    description :: Core.Maybe Core.Text,
    createdBy :: Core.Maybe UserContext,
    lastModifiedBy :: Core.Maybe UserContext,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeActionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'describeActionResponse_status' - The status of the action.
--
-- 'metadataProperties', 'describeActionResponse_metadataProperties' - Undocumented member.
--
-- 'creationTime', 'describeActionResponse_creationTime' - When the action was created.
--
-- 'actionName', 'describeActionResponse_actionName' - The name of the action.
--
-- 'actionType', 'describeActionResponse_actionType' - The type of the action.
--
-- 'actionArn', 'describeActionResponse_actionArn' - The Amazon Resource Name (ARN) of the action.
--
-- 'source', 'describeActionResponse_source' - The source of the action.
--
-- 'properties', 'describeActionResponse_properties' - A list of the action\'s properties.
--
-- 'lastModifiedTime', 'describeActionResponse_lastModifiedTime' - When the action was last modified.
--
-- 'description', 'describeActionResponse_description' - The description of the action.
--
-- 'createdBy', 'describeActionResponse_createdBy' - Undocumented member.
--
-- 'lastModifiedBy', 'describeActionResponse_lastModifiedBy' - Undocumented member.
--
-- 'httpStatus', 'describeActionResponse_httpStatus' - The response's http status code.
newDescribeActionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeActionResponse
newDescribeActionResponse pHttpStatus_ =
  DescribeActionResponse'
    { status = Core.Nothing,
      metadataProperties = Core.Nothing,
      creationTime = Core.Nothing,
      actionName = Core.Nothing,
      actionType = Core.Nothing,
      actionArn = Core.Nothing,
      source = Core.Nothing,
      properties = Core.Nothing,
      lastModifiedTime = Core.Nothing,
      description = Core.Nothing,
      createdBy = Core.Nothing,
      lastModifiedBy = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of the action.
describeActionResponse_status :: Lens.Lens' DescribeActionResponse (Core.Maybe ActionStatus)
describeActionResponse_status = Lens.lens (\DescribeActionResponse' {status} -> status) (\s@DescribeActionResponse' {} a -> s {status = a} :: DescribeActionResponse)

-- | Undocumented member.
describeActionResponse_metadataProperties :: Lens.Lens' DescribeActionResponse (Core.Maybe MetadataProperties)
describeActionResponse_metadataProperties = Lens.lens (\DescribeActionResponse' {metadataProperties} -> metadataProperties) (\s@DescribeActionResponse' {} a -> s {metadataProperties = a} :: DescribeActionResponse)

-- | When the action was created.
describeActionResponse_creationTime :: Lens.Lens' DescribeActionResponse (Core.Maybe Core.UTCTime)
describeActionResponse_creationTime = Lens.lens (\DescribeActionResponse' {creationTime} -> creationTime) (\s@DescribeActionResponse' {} a -> s {creationTime = a} :: DescribeActionResponse) Core.. Lens.mapping Core._Time

-- | The name of the action.
describeActionResponse_actionName :: Lens.Lens' DescribeActionResponse (Core.Maybe Core.Text)
describeActionResponse_actionName = Lens.lens (\DescribeActionResponse' {actionName} -> actionName) (\s@DescribeActionResponse' {} a -> s {actionName = a} :: DescribeActionResponse)

-- | The type of the action.
describeActionResponse_actionType :: Lens.Lens' DescribeActionResponse (Core.Maybe Core.Text)
describeActionResponse_actionType = Lens.lens (\DescribeActionResponse' {actionType} -> actionType) (\s@DescribeActionResponse' {} a -> s {actionType = a} :: DescribeActionResponse)

-- | The Amazon Resource Name (ARN) of the action.
describeActionResponse_actionArn :: Lens.Lens' DescribeActionResponse (Core.Maybe Core.Text)
describeActionResponse_actionArn = Lens.lens (\DescribeActionResponse' {actionArn} -> actionArn) (\s@DescribeActionResponse' {} a -> s {actionArn = a} :: DescribeActionResponse)

-- | The source of the action.
describeActionResponse_source :: Lens.Lens' DescribeActionResponse (Core.Maybe ActionSource)
describeActionResponse_source = Lens.lens (\DescribeActionResponse' {source} -> source) (\s@DescribeActionResponse' {} a -> s {source = a} :: DescribeActionResponse)

-- | A list of the action\'s properties.
describeActionResponse_properties :: Lens.Lens' DescribeActionResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
describeActionResponse_properties = Lens.lens (\DescribeActionResponse' {properties} -> properties) (\s@DescribeActionResponse' {} a -> s {properties = a} :: DescribeActionResponse) Core.. Lens.mapping Lens._Coerce

-- | When the action was last modified.
describeActionResponse_lastModifiedTime :: Lens.Lens' DescribeActionResponse (Core.Maybe Core.UTCTime)
describeActionResponse_lastModifiedTime = Lens.lens (\DescribeActionResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeActionResponse' {} a -> s {lastModifiedTime = a} :: DescribeActionResponse) Core.. Lens.mapping Core._Time

-- | The description of the action.
describeActionResponse_description :: Lens.Lens' DescribeActionResponse (Core.Maybe Core.Text)
describeActionResponse_description = Lens.lens (\DescribeActionResponse' {description} -> description) (\s@DescribeActionResponse' {} a -> s {description = a} :: DescribeActionResponse)

-- | Undocumented member.
describeActionResponse_createdBy :: Lens.Lens' DescribeActionResponse (Core.Maybe UserContext)
describeActionResponse_createdBy = Lens.lens (\DescribeActionResponse' {createdBy} -> createdBy) (\s@DescribeActionResponse' {} a -> s {createdBy = a} :: DescribeActionResponse)

-- | Undocumented member.
describeActionResponse_lastModifiedBy :: Lens.Lens' DescribeActionResponse (Core.Maybe UserContext)
describeActionResponse_lastModifiedBy = Lens.lens (\DescribeActionResponse' {lastModifiedBy} -> lastModifiedBy) (\s@DescribeActionResponse' {} a -> s {lastModifiedBy = a} :: DescribeActionResponse)

-- | The response's http status code.
describeActionResponse_httpStatus :: Lens.Lens' DescribeActionResponse Core.Int
describeActionResponse_httpStatus = Lens.lens (\DescribeActionResponse' {httpStatus} -> httpStatus) (\s@DescribeActionResponse' {} a -> s {httpStatus = a} :: DescribeActionResponse)

instance Core.NFData DescribeActionResponse
