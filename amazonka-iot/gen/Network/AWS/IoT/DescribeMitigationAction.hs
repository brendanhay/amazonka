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
-- Module      : Network.AWS.IoT.DescribeMitigationAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a mitigation action.
module Network.AWS.IoT.DescribeMitigationAction
  ( -- * Creating a Request
    DescribeMitigationAction (..),
    newDescribeMitigationAction,

    -- * Request Lenses
    describeMitigationAction_actionName,

    -- * Destructuring the Response
    DescribeMitigationActionResponse (..),
    newDescribeMitigationActionResponse,

    -- * Response Lenses
    describeMitigationActionResponse_lastModifiedDate,
    describeMitigationActionResponse_actionName,
    describeMitigationActionResponse_roleArn,
    describeMitigationActionResponse_actionType,
    describeMitigationActionResponse_actionArn,
    describeMitigationActionResponse_actionId,
    describeMitigationActionResponse_creationDate,
    describeMitigationActionResponse_actionParams,
    describeMitigationActionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeMitigationAction' smart constructor.
data DescribeMitigationAction = DescribeMitigationAction'
  { -- | The friendly name that uniquely identifies the mitigation action.
    actionName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeMitigationAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actionName', 'describeMitigationAction_actionName' - The friendly name that uniquely identifies the mitigation action.
newDescribeMitigationAction ::
  -- | 'actionName'
  Core.Text ->
  DescribeMitigationAction
newDescribeMitigationAction pActionName_ =
  DescribeMitigationAction'
    { actionName =
        pActionName_
    }

-- | The friendly name that uniquely identifies the mitigation action.
describeMitigationAction_actionName :: Lens.Lens' DescribeMitigationAction Core.Text
describeMitigationAction_actionName = Lens.lens (\DescribeMitigationAction' {actionName} -> actionName) (\s@DescribeMitigationAction' {} a -> s {actionName = a} :: DescribeMitigationAction)

instance Core.AWSRequest DescribeMitigationAction where
  type
    AWSResponse DescribeMitigationAction =
      DescribeMitigationActionResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeMitigationActionResponse'
            Core.<$> (x Core..?> "lastModifiedDate")
            Core.<*> (x Core..?> "actionName")
            Core.<*> (x Core..?> "roleArn")
            Core.<*> (x Core..?> "actionType")
            Core.<*> (x Core..?> "actionArn")
            Core.<*> (x Core..?> "actionId")
            Core.<*> (x Core..?> "creationDate")
            Core.<*> (x Core..?> "actionParams")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeMitigationAction

instance Core.NFData DescribeMitigationAction

instance Core.ToHeaders DescribeMitigationAction where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeMitigationAction where
  toPath DescribeMitigationAction' {..} =
    Core.mconcat
      ["/mitigationactions/actions/", Core.toBS actionName]

instance Core.ToQuery DescribeMitigationAction where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeMitigationActionResponse' smart constructor.
data DescribeMitigationActionResponse = DescribeMitigationActionResponse'
  { -- | The date and time when the mitigation action was last changed.
    lastModifiedDate :: Core.Maybe Core.POSIX,
    -- | The friendly name that uniquely identifies the mitigation action.
    actionName :: Core.Maybe Core.Text,
    -- | The ARN of the IAM role used to apply this action.
    roleArn :: Core.Maybe Core.Text,
    -- | The type of mitigation action.
    actionType :: Core.Maybe MitigationActionType,
    -- | The ARN that identifies this migration action.
    actionArn :: Core.Maybe Core.Text,
    -- | A unique identifier for this action.
    actionId :: Core.Maybe Core.Text,
    -- | The date and time when the mitigation action was added to your AWS
    -- account.
    creationDate :: Core.Maybe Core.POSIX,
    -- | Parameters that control how the mitigation action is applied, specific
    -- to the type of mitigation action.
    actionParams :: Core.Maybe MitigationActionParams,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeMitigationActionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedDate', 'describeMitigationActionResponse_lastModifiedDate' - The date and time when the mitigation action was last changed.
--
-- 'actionName', 'describeMitigationActionResponse_actionName' - The friendly name that uniquely identifies the mitigation action.
--
-- 'roleArn', 'describeMitigationActionResponse_roleArn' - The ARN of the IAM role used to apply this action.
--
-- 'actionType', 'describeMitigationActionResponse_actionType' - The type of mitigation action.
--
-- 'actionArn', 'describeMitigationActionResponse_actionArn' - The ARN that identifies this migration action.
--
-- 'actionId', 'describeMitigationActionResponse_actionId' - A unique identifier for this action.
--
-- 'creationDate', 'describeMitigationActionResponse_creationDate' - The date and time when the mitigation action was added to your AWS
-- account.
--
-- 'actionParams', 'describeMitigationActionResponse_actionParams' - Parameters that control how the mitigation action is applied, specific
-- to the type of mitigation action.
--
-- 'httpStatus', 'describeMitigationActionResponse_httpStatus' - The response's http status code.
newDescribeMitigationActionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeMitigationActionResponse
newDescribeMitigationActionResponse pHttpStatus_ =
  DescribeMitigationActionResponse'
    { lastModifiedDate =
        Core.Nothing,
      actionName = Core.Nothing,
      roleArn = Core.Nothing,
      actionType = Core.Nothing,
      actionArn = Core.Nothing,
      actionId = Core.Nothing,
      creationDate = Core.Nothing,
      actionParams = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The date and time when the mitigation action was last changed.
describeMitigationActionResponse_lastModifiedDate :: Lens.Lens' DescribeMitigationActionResponse (Core.Maybe Core.UTCTime)
describeMitigationActionResponse_lastModifiedDate = Lens.lens (\DescribeMitigationActionResponse' {lastModifiedDate} -> lastModifiedDate) (\s@DescribeMitigationActionResponse' {} a -> s {lastModifiedDate = a} :: DescribeMitigationActionResponse) Core.. Lens.mapping Core._Time

-- | The friendly name that uniquely identifies the mitigation action.
describeMitigationActionResponse_actionName :: Lens.Lens' DescribeMitigationActionResponse (Core.Maybe Core.Text)
describeMitigationActionResponse_actionName = Lens.lens (\DescribeMitigationActionResponse' {actionName} -> actionName) (\s@DescribeMitigationActionResponse' {} a -> s {actionName = a} :: DescribeMitigationActionResponse)

-- | The ARN of the IAM role used to apply this action.
describeMitigationActionResponse_roleArn :: Lens.Lens' DescribeMitigationActionResponse (Core.Maybe Core.Text)
describeMitigationActionResponse_roleArn = Lens.lens (\DescribeMitigationActionResponse' {roleArn} -> roleArn) (\s@DescribeMitigationActionResponse' {} a -> s {roleArn = a} :: DescribeMitigationActionResponse)

-- | The type of mitigation action.
describeMitigationActionResponse_actionType :: Lens.Lens' DescribeMitigationActionResponse (Core.Maybe MitigationActionType)
describeMitigationActionResponse_actionType = Lens.lens (\DescribeMitigationActionResponse' {actionType} -> actionType) (\s@DescribeMitigationActionResponse' {} a -> s {actionType = a} :: DescribeMitigationActionResponse)

-- | The ARN that identifies this migration action.
describeMitigationActionResponse_actionArn :: Lens.Lens' DescribeMitigationActionResponse (Core.Maybe Core.Text)
describeMitigationActionResponse_actionArn = Lens.lens (\DescribeMitigationActionResponse' {actionArn} -> actionArn) (\s@DescribeMitigationActionResponse' {} a -> s {actionArn = a} :: DescribeMitigationActionResponse)

-- | A unique identifier for this action.
describeMitigationActionResponse_actionId :: Lens.Lens' DescribeMitigationActionResponse (Core.Maybe Core.Text)
describeMitigationActionResponse_actionId = Lens.lens (\DescribeMitigationActionResponse' {actionId} -> actionId) (\s@DescribeMitigationActionResponse' {} a -> s {actionId = a} :: DescribeMitigationActionResponse)

-- | The date and time when the mitigation action was added to your AWS
-- account.
describeMitigationActionResponse_creationDate :: Lens.Lens' DescribeMitigationActionResponse (Core.Maybe Core.UTCTime)
describeMitigationActionResponse_creationDate = Lens.lens (\DescribeMitigationActionResponse' {creationDate} -> creationDate) (\s@DescribeMitigationActionResponse' {} a -> s {creationDate = a} :: DescribeMitigationActionResponse) Core.. Lens.mapping Core._Time

-- | Parameters that control how the mitigation action is applied, specific
-- to the type of mitigation action.
describeMitigationActionResponse_actionParams :: Lens.Lens' DescribeMitigationActionResponse (Core.Maybe MitigationActionParams)
describeMitigationActionResponse_actionParams = Lens.lens (\DescribeMitigationActionResponse' {actionParams} -> actionParams) (\s@DescribeMitigationActionResponse' {} a -> s {actionParams = a} :: DescribeMitigationActionResponse)

-- | The response's http status code.
describeMitigationActionResponse_httpStatus :: Lens.Lens' DescribeMitigationActionResponse Core.Int
describeMitigationActionResponse_httpStatus = Lens.lens (\DescribeMitigationActionResponse' {httpStatus} -> httpStatus) (\s@DescribeMitigationActionResponse' {} a -> s {httpStatus = a} :: DescribeMitigationActionResponse)

instance Core.NFData DescribeMitigationActionResponse
