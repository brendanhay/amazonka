{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeMitigationAction' smart constructor.
data DescribeMitigationAction = DescribeMitigationAction'
  { -- | The friendly name that uniquely identifies the mitigation action.
    actionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DescribeMitigationAction
newDescribeMitigationAction pActionName_ =
  DescribeMitigationAction'
    { actionName =
        pActionName_
    }

-- | The friendly name that uniquely identifies the mitigation action.
describeMitigationAction_actionName :: Lens.Lens' DescribeMitigationAction Prelude.Text
describeMitigationAction_actionName = Lens.lens (\DescribeMitigationAction' {actionName} -> actionName) (\s@DescribeMitigationAction' {} a -> s {actionName = a} :: DescribeMitigationAction)

instance Prelude.AWSRequest DescribeMitigationAction where
  type
    Rs DescribeMitigationAction =
      DescribeMitigationActionResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeMitigationActionResponse'
            Prelude.<$> (x Prelude..?> "lastModifiedDate")
            Prelude.<*> (x Prelude..?> "actionName")
            Prelude.<*> (x Prelude..?> "roleArn")
            Prelude.<*> (x Prelude..?> "actionType")
            Prelude.<*> (x Prelude..?> "actionArn")
            Prelude.<*> (x Prelude..?> "actionId")
            Prelude.<*> (x Prelude..?> "creationDate")
            Prelude.<*> (x Prelude..?> "actionParams")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeMitigationAction

instance Prelude.NFData DescribeMitigationAction

instance Prelude.ToHeaders DescribeMitigationAction where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DescribeMitigationAction where
  toPath DescribeMitigationAction' {..} =
    Prelude.mconcat
      [ "/mitigationactions/actions/",
        Prelude.toBS actionName
      ]

instance Prelude.ToQuery DescribeMitigationAction where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeMitigationActionResponse' smart constructor.
data DescribeMitigationActionResponse = DescribeMitigationActionResponse'
  { -- | The date and time when the mitigation action was last changed.
    lastModifiedDate :: Prelude.Maybe Prelude.POSIX,
    -- | The friendly name that uniquely identifies the mitigation action.
    actionName :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the IAM role used to apply this action.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The type of mitigation action.
    actionType :: Prelude.Maybe MitigationActionType,
    -- | The ARN that identifies this migration action.
    actionArn :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for this action.
    actionId :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the mitigation action was added to your AWS
    -- account.
    creationDate :: Prelude.Maybe Prelude.POSIX,
    -- | Parameters that control how the mitigation action is applied, specific
    -- to the type of mitigation action.
    actionParams :: Prelude.Maybe MitigationActionParams,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DescribeMitigationActionResponse
newDescribeMitigationActionResponse pHttpStatus_ =
  DescribeMitigationActionResponse'
    { lastModifiedDate =
        Prelude.Nothing,
      actionName = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      actionType = Prelude.Nothing,
      actionArn = Prelude.Nothing,
      actionId = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      actionParams = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The date and time when the mitigation action was last changed.
describeMitigationActionResponse_lastModifiedDate :: Lens.Lens' DescribeMitigationActionResponse (Prelude.Maybe Prelude.UTCTime)
describeMitigationActionResponse_lastModifiedDate = Lens.lens (\DescribeMitigationActionResponse' {lastModifiedDate} -> lastModifiedDate) (\s@DescribeMitigationActionResponse' {} a -> s {lastModifiedDate = a} :: DescribeMitigationActionResponse) Prelude.. Lens.mapping Prelude._Time

-- | The friendly name that uniquely identifies the mitigation action.
describeMitigationActionResponse_actionName :: Lens.Lens' DescribeMitigationActionResponse (Prelude.Maybe Prelude.Text)
describeMitigationActionResponse_actionName = Lens.lens (\DescribeMitigationActionResponse' {actionName} -> actionName) (\s@DescribeMitigationActionResponse' {} a -> s {actionName = a} :: DescribeMitigationActionResponse)

-- | The ARN of the IAM role used to apply this action.
describeMitigationActionResponse_roleArn :: Lens.Lens' DescribeMitigationActionResponse (Prelude.Maybe Prelude.Text)
describeMitigationActionResponse_roleArn = Lens.lens (\DescribeMitigationActionResponse' {roleArn} -> roleArn) (\s@DescribeMitigationActionResponse' {} a -> s {roleArn = a} :: DescribeMitigationActionResponse)

-- | The type of mitigation action.
describeMitigationActionResponse_actionType :: Lens.Lens' DescribeMitigationActionResponse (Prelude.Maybe MitigationActionType)
describeMitigationActionResponse_actionType = Lens.lens (\DescribeMitigationActionResponse' {actionType} -> actionType) (\s@DescribeMitigationActionResponse' {} a -> s {actionType = a} :: DescribeMitigationActionResponse)

-- | The ARN that identifies this migration action.
describeMitigationActionResponse_actionArn :: Lens.Lens' DescribeMitigationActionResponse (Prelude.Maybe Prelude.Text)
describeMitigationActionResponse_actionArn = Lens.lens (\DescribeMitigationActionResponse' {actionArn} -> actionArn) (\s@DescribeMitigationActionResponse' {} a -> s {actionArn = a} :: DescribeMitigationActionResponse)

-- | A unique identifier for this action.
describeMitigationActionResponse_actionId :: Lens.Lens' DescribeMitigationActionResponse (Prelude.Maybe Prelude.Text)
describeMitigationActionResponse_actionId = Lens.lens (\DescribeMitigationActionResponse' {actionId} -> actionId) (\s@DescribeMitigationActionResponse' {} a -> s {actionId = a} :: DescribeMitigationActionResponse)

-- | The date and time when the mitigation action was added to your AWS
-- account.
describeMitigationActionResponse_creationDate :: Lens.Lens' DescribeMitigationActionResponse (Prelude.Maybe Prelude.UTCTime)
describeMitigationActionResponse_creationDate = Lens.lens (\DescribeMitigationActionResponse' {creationDate} -> creationDate) (\s@DescribeMitigationActionResponse' {} a -> s {creationDate = a} :: DescribeMitigationActionResponse) Prelude.. Lens.mapping Prelude._Time

-- | Parameters that control how the mitigation action is applied, specific
-- to the type of mitigation action.
describeMitigationActionResponse_actionParams :: Lens.Lens' DescribeMitigationActionResponse (Prelude.Maybe MitigationActionParams)
describeMitigationActionResponse_actionParams = Lens.lens (\DescribeMitigationActionResponse' {actionParams} -> actionParams) (\s@DescribeMitigationActionResponse' {} a -> s {actionParams = a} :: DescribeMitigationActionResponse)

-- | The response's http status code.
describeMitigationActionResponse_httpStatus :: Lens.Lens' DescribeMitigationActionResponse Prelude.Int
describeMitigationActionResponse_httpStatus = Lens.lens (\DescribeMitigationActionResponse' {httpStatus} -> httpStatus) (\s@DescribeMitigationActionResponse' {} a -> s {httpStatus = a} :: DescribeMitigationActionResponse)

instance
  Prelude.NFData
    DescribeMitigationActionResponse
