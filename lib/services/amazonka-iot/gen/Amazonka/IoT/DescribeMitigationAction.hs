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
-- Module      : Amazonka.IoT.DescribeMitigationAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a mitigation action.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions DescribeMitigationAction>
-- action.
module Amazonka.IoT.DescribeMitigationAction
  ( -- * Creating a Request
    DescribeMitigationAction (..),
    newDescribeMitigationAction,

    -- * Request Lenses
    describeMitigationAction_actionName,

    -- * Destructuring the Response
    DescribeMitigationActionResponse (..),
    newDescribeMitigationActionResponse,

    -- * Response Lenses
    describeMitigationActionResponse_actionArn,
    describeMitigationActionResponse_actionId,
    describeMitigationActionResponse_actionName,
    describeMitigationActionResponse_actionParams,
    describeMitigationActionResponse_actionType,
    describeMitigationActionResponse_creationDate,
    describeMitigationActionResponse_lastModifiedDate,
    describeMitigationActionResponse_roleArn,
    describeMitigationActionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeMitigationAction' smart constructor.
data DescribeMitigationAction = DescribeMitigationAction'
  { -- | The friendly name that uniquely identifies the mitigation action.
    actionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest DescribeMitigationAction where
  type
    AWSResponse DescribeMitigationAction =
      DescribeMitigationActionResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeMitigationActionResponse'
            Prelude.<$> (x Data..?> "actionArn")
            Prelude.<*> (x Data..?> "actionId")
            Prelude.<*> (x Data..?> "actionName")
            Prelude.<*> (x Data..?> "actionParams")
            Prelude.<*> (x Data..?> "actionType")
            Prelude.<*> (x Data..?> "creationDate")
            Prelude.<*> (x Data..?> "lastModifiedDate")
            Prelude.<*> (x Data..?> "roleArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeMitigationAction where
  hashWithSalt _salt DescribeMitigationAction' {..} =
    _salt `Prelude.hashWithSalt` actionName

instance Prelude.NFData DescribeMitigationAction where
  rnf DescribeMitigationAction' {..} =
    Prelude.rnf actionName

instance Data.ToHeaders DescribeMitigationAction where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeMitigationAction where
  toPath DescribeMitigationAction' {..} =
    Prelude.mconcat
      ["/mitigationactions/actions/", Data.toBS actionName]

instance Data.ToQuery DescribeMitigationAction where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeMitigationActionResponse' smart constructor.
data DescribeMitigationActionResponse = DescribeMitigationActionResponse'
  { -- | The ARN that identifies this migration action.
    actionArn :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for this action.
    actionId :: Prelude.Maybe Prelude.Text,
    -- | The friendly name that uniquely identifies the mitigation action.
    actionName :: Prelude.Maybe Prelude.Text,
    -- | Parameters that control how the mitigation action is applied, specific
    -- to the type of mitigation action.
    actionParams :: Prelude.Maybe MitigationActionParams,
    -- | The type of mitigation action.
    actionType :: Prelude.Maybe MitigationActionType,
    -- | The date and time when the mitigation action was added to your Amazon
    -- Web Services accounts.
    creationDate :: Prelude.Maybe Data.POSIX,
    -- | The date and time when the mitigation action was last changed.
    lastModifiedDate :: Prelude.Maybe Data.POSIX,
    -- | The ARN of the IAM role used to apply this action.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeMitigationActionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actionArn', 'describeMitigationActionResponse_actionArn' - The ARN that identifies this migration action.
--
-- 'actionId', 'describeMitigationActionResponse_actionId' - A unique identifier for this action.
--
-- 'actionName', 'describeMitigationActionResponse_actionName' - The friendly name that uniquely identifies the mitigation action.
--
-- 'actionParams', 'describeMitigationActionResponse_actionParams' - Parameters that control how the mitigation action is applied, specific
-- to the type of mitigation action.
--
-- 'actionType', 'describeMitigationActionResponse_actionType' - The type of mitigation action.
--
-- 'creationDate', 'describeMitigationActionResponse_creationDate' - The date and time when the mitigation action was added to your Amazon
-- Web Services accounts.
--
-- 'lastModifiedDate', 'describeMitigationActionResponse_lastModifiedDate' - The date and time when the mitigation action was last changed.
--
-- 'roleArn', 'describeMitigationActionResponse_roleArn' - The ARN of the IAM role used to apply this action.
--
-- 'httpStatus', 'describeMitigationActionResponse_httpStatus' - The response's http status code.
newDescribeMitigationActionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeMitigationActionResponse
newDescribeMitigationActionResponse pHttpStatus_ =
  DescribeMitigationActionResponse'
    { actionArn =
        Prelude.Nothing,
      actionId = Prelude.Nothing,
      actionName = Prelude.Nothing,
      actionParams = Prelude.Nothing,
      actionType = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN that identifies this migration action.
describeMitigationActionResponse_actionArn :: Lens.Lens' DescribeMitigationActionResponse (Prelude.Maybe Prelude.Text)
describeMitigationActionResponse_actionArn = Lens.lens (\DescribeMitigationActionResponse' {actionArn} -> actionArn) (\s@DescribeMitigationActionResponse' {} a -> s {actionArn = a} :: DescribeMitigationActionResponse)

-- | A unique identifier for this action.
describeMitigationActionResponse_actionId :: Lens.Lens' DescribeMitigationActionResponse (Prelude.Maybe Prelude.Text)
describeMitigationActionResponse_actionId = Lens.lens (\DescribeMitigationActionResponse' {actionId} -> actionId) (\s@DescribeMitigationActionResponse' {} a -> s {actionId = a} :: DescribeMitigationActionResponse)

-- | The friendly name that uniquely identifies the mitigation action.
describeMitigationActionResponse_actionName :: Lens.Lens' DescribeMitigationActionResponse (Prelude.Maybe Prelude.Text)
describeMitigationActionResponse_actionName = Lens.lens (\DescribeMitigationActionResponse' {actionName} -> actionName) (\s@DescribeMitigationActionResponse' {} a -> s {actionName = a} :: DescribeMitigationActionResponse)

-- | Parameters that control how the mitigation action is applied, specific
-- to the type of mitigation action.
describeMitigationActionResponse_actionParams :: Lens.Lens' DescribeMitigationActionResponse (Prelude.Maybe MitigationActionParams)
describeMitigationActionResponse_actionParams = Lens.lens (\DescribeMitigationActionResponse' {actionParams} -> actionParams) (\s@DescribeMitigationActionResponse' {} a -> s {actionParams = a} :: DescribeMitigationActionResponse)

-- | The type of mitigation action.
describeMitigationActionResponse_actionType :: Lens.Lens' DescribeMitigationActionResponse (Prelude.Maybe MitigationActionType)
describeMitigationActionResponse_actionType = Lens.lens (\DescribeMitigationActionResponse' {actionType} -> actionType) (\s@DescribeMitigationActionResponse' {} a -> s {actionType = a} :: DescribeMitigationActionResponse)

-- | The date and time when the mitigation action was added to your Amazon
-- Web Services accounts.
describeMitigationActionResponse_creationDate :: Lens.Lens' DescribeMitigationActionResponse (Prelude.Maybe Prelude.UTCTime)
describeMitigationActionResponse_creationDate = Lens.lens (\DescribeMitigationActionResponse' {creationDate} -> creationDate) (\s@DescribeMitigationActionResponse' {} a -> s {creationDate = a} :: DescribeMitigationActionResponse) Prelude.. Lens.mapping Data._Time

-- | The date and time when the mitigation action was last changed.
describeMitigationActionResponse_lastModifiedDate :: Lens.Lens' DescribeMitigationActionResponse (Prelude.Maybe Prelude.UTCTime)
describeMitigationActionResponse_lastModifiedDate = Lens.lens (\DescribeMitigationActionResponse' {lastModifiedDate} -> lastModifiedDate) (\s@DescribeMitigationActionResponse' {} a -> s {lastModifiedDate = a} :: DescribeMitigationActionResponse) Prelude.. Lens.mapping Data._Time

-- | The ARN of the IAM role used to apply this action.
describeMitigationActionResponse_roleArn :: Lens.Lens' DescribeMitigationActionResponse (Prelude.Maybe Prelude.Text)
describeMitigationActionResponse_roleArn = Lens.lens (\DescribeMitigationActionResponse' {roleArn} -> roleArn) (\s@DescribeMitigationActionResponse' {} a -> s {roleArn = a} :: DescribeMitigationActionResponse)

-- | The response's http status code.
describeMitigationActionResponse_httpStatus :: Lens.Lens' DescribeMitigationActionResponse Prelude.Int
describeMitigationActionResponse_httpStatus = Lens.lens (\DescribeMitigationActionResponse' {httpStatus} -> httpStatus) (\s@DescribeMitigationActionResponse' {} a -> s {httpStatus = a} :: DescribeMitigationActionResponse)

instance
  Prelude.NFData
    DescribeMitigationActionResponse
  where
  rnf DescribeMitigationActionResponse' {..} =
    Prelude.rnf actionArn
      `Prelude.seq` Prelude.rnf actionId
      `Prelude.seq` Prelude.rnf actionName
      `Prelude.seq` Prelude.rnf actionParams
      `Prelude.seq` Prelude.rnf actionType
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf lastModifiedDate
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf httpStatus
