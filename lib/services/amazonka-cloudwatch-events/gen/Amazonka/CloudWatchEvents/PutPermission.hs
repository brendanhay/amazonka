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
-- Module      : Amazonka.CloudWatchEvents.PutPermission
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Running @PutPermission@ permits the specified Amazon Web Services
-- account or Amazon Web Services organization to put events to the
-- specified /event bus/. Amazon EventBridge (CloudWatch Events) rules in
-- your account are triggered by these events arriving to an event bus in
-- your account.
--
-- For another account to send events to your account, that external
-- account must have an EventBridge rule with your account\'s event bus as
-- a target.
--
-- To enable multiple Amazon Web Services accounts to put events to your
-- event bus, run @PutPermission@ once for each of these accounts. Or, if
-- all the accounts are members of the same Amazon Web Services
-- organization, you can run @PutPermission@ once specifying @Principal@ as
-- \"*\" and specifying the Amazon Web Services organization ID in
-- @Condition@, to grant permissions to all accounts in that organization.
--
-- If you grant permissions using an organization, then accounts in that
-- organization must specify a @RoleArn@ with proper permissions when they
-- use @PutTarget@ to add your account\'s event bus as a target. For more
-- information, see
-- <https://docs.aws.amazon.com/eventbridge/latest/userguide/eventbridge-cross-account-event-delivery.html Sending and Receiving Events Between Amazon Web Services Accounts>
-- in the /Amazon EventBridge User Guide/.
--
-- The permission policy on the event bus cannot exceed 10 KB in size.
module Amazonka.CloudWatchEvents.PutPermission
  ( -- * Creating a Request
    PutPermission (..),
    newPutPermission,

    -- * Request Lenses
    putPermission_principal,
    putPermission_policy,
    putPermission_eventBusName,
    putPermission_statementId,
    putPermission_condition,
    putPermission_action,

    -- * Destructuring the Response
    PutPermissionResponse (..),
    newPutPermissionResponse,
  )
where

import Amazonka.CloudWatchEvents.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutPermission' smart constructor.
data PutPermission = PutPermission'
  { -- | The 12-digit Amazon Web Services account ID that you are permitting to
    -- put events to your default event bus. Specify \"*\" to permit any
    -- account to put events to your default event bus.
    --
    -- If you specify \"*\" without specifying @Condition@, avoid creating
    -- rules that may match undesirable events. To create more secure rules,
    -- make sure that the event pattern for each rule contains an @account@
    -- field with a specific account ID from which to receive events. Rules
    -- with an account field do not match any events sent from other accounts.
    principal :: Prelude.Maybe Prelude.Text,
    -- | A JSON string that describes the permission policy statement. You can
    -- include a @Policy@ parameter in the request instead of using the
    -- @StatementId@, @Action@, @Principal@, or @Condition@ parameters.
    policy :: Prelude.Maybe Prelude.Text,
    -- | The name of the event bus associated with the rule. If you omit this,
    -- the default event bus is used.
    eventBusName :: Prelude.Maybe Prelude.Text,
    -- | An identifier string for the external account that you are granting
    -- permissions to. If you later want to revoke the permission for this
    -- external account, specify this @StatementId@ when you run
    -- <https://docs.aws.amazon.com/eventbridge/latest/APIReference/API_RemovePermission.html RemovePermission>.
    --
    -- Each @StatementId@ must be unique.
    statementId :: Prelude.Maybe Prelude.Text,
    -- | This parameter enables you to limit the permission to accounts that
    -- fulfill a certain condition, such as being a member of a certain Amazon
    -- Web Services organization. For more information about Amazon Web
    -- Services Organizations, see
    -- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_introduction.html What Is Amazon Web Services Organizations>
    -- in the /Amazon Web Services Organizations User Guide/.
    --
    -- If you specify @Condition@ with an Amazon Web Services organization ID,
    -- and specify \"*\" as the value for @Principal@, you grant permission to
    -- all the accounts in the named organization.
    --
    -- The @Condition@ is a JSON string which must contain @Type@, @Key@, and
    -- @Value@ fields.
    condition :: Prelude.Maybe Condition,
    -- | The action that you are enabling the other account to perform.
    action :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutPermission' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'principal', 'putPermission_principal' - The 12-digit Amazon Web Services account ID that you are permitting to
-- put events to your default event bus. Specify \"*\" to permit any
-- account to put events to your default event bus.
--
-- If you specify \"*\" without specifying @Condition@, avoid creating
-- rules that may match undesirable events. To create more secure rules,
-- make sure that the event pattern for each rule contains an @account@
-- field with a specific account ID from which to receive events. Rules
-- with an account field do not match any events sent from other accounts.
--
-- 'policy', 'putPermission_policy' - A JSON string that describes the permission policy statement. You can
-- include a @Policy@ parameter in the request instead of using the
-- @StatementId@, @Action@, @Principal@, or @Condition@ parameters.
--
-- 'eventBusName', 'putPermission_eventBusName' - The name of the event bus associated with the rule. If you omit this,
-- the default event bus is used.
--
-- 'statementId', 'putPermission_statementId' - An identifier string for the external account that you are granting
-- permissions to. If you later want to revoke the permission for this
-- external account, specify this @StatementId@ when you run
-- <https://docs.aws.amazon.com/eventbridge/latest/APIReference/API_RemovePermission.html RemovePermission>.
--
-- Each @StatementId@ must be unique.
--
-- 'condition', 'putPermission_condition' - This parameter enables you to limit the permission to accounts that
-- fulfill a certain condition, such as being a member of a certain Amazon
-- Web Services organization. For more information about Amazon Web
-- Services Organizations, see
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_introduction.html What Is Amazon Web Services Organizations>
-- in the /Amazon Web Services Organizations User Guide/.
--
-- If you specify @Condition@ with an Amazon Web Services organization ID,
-- and specify \"*\" as the value for @Principal@, you grant permission to
-- all the accounts in the named organization.
--
-- The @Condition@ is a JSON string which must contain @Type@, @Key@, and
-- @Value@ fields.
--
-- 'action', 'putPermission_action' - The action that you are enabling the other account to perform.
newPutPermission ::
  PutPermission
newPutPermission =
  PutPermission'
    { principal = Prelude.Nothing,
      policy = Prelude.Nothing,
      eventBusName = Prelude.Nothing,
      statementId = Prelude.Nothing,
      condition = Prelude.Nothing,
      action = Prelude.Nothing
    }

-- | The 12-digit Amazon Web Services account ID that you are permitting to
-- put events to your default event bus. Specify \"*\" to permit any
-- account to put events to your default event bus.
--
-- If you specify \"*\" without specifying @Condition@, avoid creating
-- rules that may match undesirable events. To create more secure rules,
-- make sure that the event pattern for each rule contains an @account@
-- field with a specific account ID from which to receive events. Rules
-- with an account field do not match any events sent from other accounts.
putPermission_principal :: Lens.Lens' PutPermission (Prelude.Maybe Prelude.Text)
putPermission_principal = Lens.lens (\PutPermission' {principal} -> principal) (\s@PutPermission' {} a -> s {principal = a} :: PutPermission)

-- | A JSON string that describes the permission policy statement. You can
-- include a @Policy@ parameter in the request instead of using the
-- @StatementId@, @Action@, @Principal@, or @Condition@ parameters.
putPermission_policy :: Lens.Lens' PutPermission (Prelude.Maybe Prelude.Text)
putPermission_policy = Lens.lens (\PutPermission' {policy} -> policy) (\s@PutPermission' {} a -> s {policy = a} :: PutPermission)

-- | The name of the event bus associated with the rule. If you omit this,
-- the default event bus is used.
putPermission_eventBusName :: Lens.Lens' PutPermission (Prelude.Maybe Prelude.Text)
putPermission_eventBusName = Lens.lens (\PutPermission' {eventBusName} -> eventBusName) (\s@PutPermission' {} a -> s {eventBusName = a} :: PutPermission)

-- | An identifier string for the external account that you are granting
-- permissions to. If you later want to revoke the permission for this
-- external account, specify this @StatementId@ when you run
-- <https://docs.aws.amazon.com/eventbridge/latest/APIReference/API_RemovePermission.html RemovePermission>.
--
-- Each @StatementId@ must be unique.
putPermission_statementId :: Lens.Lens' PutPermission (Prelude.Maybe Prelude.Text)
putPermission_statementId = Lens.lens (\PutPermission' {statementId} -> statementId) (\s@PutPermission' {} a -> s {statementId = a} :: PutPermission)

-- | This parameter enables you to limit the permission to accounts that
-- fulfill a certain condition, such as being a member of a certain Amazon
-- Web Services organization. For more information about Amazon Web
-- Services Organizations, see
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_introduction.html What Is Amazon Web Services Organizations>
-- in the /Amazon Web Services Organizations User Guide/.
--
-- If you specify @Condition@ with an Amazon Web Services organization ID,
-- and specify \"*\" as the value for @Principal@, you grant permission to
-- all the accounts in the named organization.
--
-- The @Condition@ is a JSON string which must contain @Type@, @Key@, and
-- @Value@ fields.
putPermission_condition :: Lens.Lens' PutPermission (Prelude.Maybe Condition)
putPermission_condition = Lens.lens (\PutPermission' {condition} -> condition) (\s@PutPermission' {} a -> s {condition = a} :: PutPermission)

-- | The action that you are enabling the other account to perform.
putPermission_action :: Lens.Lens' PutPermission (Prelude.Maybe Prelude.Text)
putPermission_action = Lens.lens (\PutPermission' {action} -> action) (\s@PutPermission' {} a -> s {action = a} :: PutPermission)

instance Core.AWSRequest PutPermission where
  type
    AWSResponse PutPermission =
      PutPermissionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull PutPermissionResponse'

instance Prelude.Hashable PutPermission where
  hashWithSalt _salt PutPermission' {..} =
    _salt `Prelude.hashWithSalt` principal
      `Prelude.hashWithSalt` policy
      `Prelude.hashWithSalt` eventBusName
      `Prelude.hashWithSalt` statementId
      `Prelude.hashWithSalt` condition
      `Prelude.hashWithSalt` action

instance Prelude.NFData PutPermission where
  rnf PutPermission' {..} =
    Prelude.rnf principal
      `Prelude.seq` Prelude.rnf policy
      `Prelude.seq` Prelude.rnf eventBusName
      `Prelude.seq` Prelude.rnf statementId
      `Prelude.seq` Prelude.rnf condition
      `Prelude.seq` Prelude.rnf action

instance Data.ToHeaders PutPermission where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSEvents.PutPermission" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutPermission where
  toJSON PutPermission' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Principal" Data..=) Prelude.<$> principal,
            ("Policy" Data..=) Prelude.<$> policy,
            ("EventBusName" Data..=) Prelude.<$> eventBusName,
            ("StatementId" Data..=) Prelude.<$> statementId,
            ("Condition" Data..=) Prelude.<$> condition,
            ("Action" Data..=) Prelude.<$> action
          ]
      )

instance Data.ToPath PutPermission where
  toPath = Prelude.const "/"

instance Data.ToQuery PutPermission where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutPermissionResponse' smart constructor.
data PutPermissionResponse = PutPermissionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutPermissionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutPermissionResponse ::
  PutPermissionResponse
newPutPermissionResponse = PutPermissionResponse'

instance Prelude.NFData PutPermissionResponse where
  rnf _ = ()
