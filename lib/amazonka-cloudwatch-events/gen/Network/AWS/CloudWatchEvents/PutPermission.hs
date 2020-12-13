{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.PutPermission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Running @PutPermission@ permits the specified AWS account or AWS organization to put events to the specified /event bus/ . Amazon EventBridge (CloudWatch Events) rules in your account are triggered by these events arriving to an event bus in your account.
--
-- For another account to send events to your account, that external account must have an EventBridge rule with your account's event bus as a target.
-- To enable multiple AWS accounts to put events to your event bus, run @PutPermission@ once for each of these accounts. Or, if all the accounts are members of the same AWS organization, you can run @PutPermission@ once specifying @Principal@ as "*" and specifying the AWS organization ID in @Condition@ , to grant permissions to all accounts in that organization.
-- If you grant permissions using an organization, then accounts in that organization must specify a @RoleArn@ with proper permissions when they use @PutTarget@ to add your account's event bus as a target. For more information, see <https://docs.aws.amazon.com/eventbridge/latest/userguide/eventbridge-cross-account-event-delivery.html Sending and Receiving Events Between AWS Accounts> in the /Amazon EventBridge User Guide/ .
-- The permission policy on the default event bus cannot exceed 10 KB in size.
module Network.AWS.CloudWatchEvents.PutPermission
  ( -- * Creating a request
    PutPermission (..),
    mkPutPermission,

    -- ** Request lenses
    ppAction,
    ppEventBusName,
    ppPrincipal,
    ppPolicy,
    ppStatementId,
    ppCondition,

    -- * Destructuring the response
    PutPermissionResponse (..),
    mkPutPermissionResponse,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutPermission' smart constructor.
data PutPermission = PutPermission'
  { -- | The action that you are enabling the other account to perform. Currently, this must be @events:PutEvents@ .
    action :: Lude.Maybe Lude.Text,
    -- | The name of the event bus associated with the rule. If you omit this, the default event bus is used.
    eventBusName :: Lude.Maybe Lude.Text,
    -- | The 12-digit AWS account ID that you are permitting to put events to your default event bus. Specify "*" to permit any account to put events to your default event bus.
    --
    -- If you specify "*" without specifying @Condition@ , avoid creating rules that may match undesirable events. To create more secure rules, make sure that the event pattern for each rule contains an @account@ field with a specific account ID from which to receive events. Rules with an account field do not match any events sent from other accounts.
    principal :: Lude.Maybe Lude.Text,
    -- | A JSON string that describes the permission policy statement. You can include a @Policy@ parameter in the request instead of using the @StatementId@ , @Action@ , @Principal@ , or @Condition@ parameters.
    policy :: Lude.Maybe Lude.Text,
    -- | An identifier string for the external account that you are granting permissions to. If you later want to revoke the permission for this external account, specify this @StatementId@ when you run 'RemovePermission' .
    statementId :: Lude.Maybe Lude.Text,
    -- | This parameter enables you to limit the permission to accounts that fulfill a certain condition, such as being a member of a certain AWS organization. For more information about AWS Organizations, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_introduction.html What Is AWS Organizations> in the /AWS Organizations User Guide/ .
    --
    -- If you specify @Condition@ with an AWS organization ID, and specify "*" as the value for @Principal@ , you grant permission to all the accounts in the named organization.
    -- The @Condition@ is a JSON string which must contain @Type@ , @Key@ , and @Value@ fields.
    condition :: Lude.Maybe Condition
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutPermission' with the minimum fields required to make a request.
--
-- * 'action' - The action that you are enabling the other account to perform. Currently, this must be @events:PutEvents@ .
-- * 'eventBusName' - The name of the event bus associated with the rule. If you omit this, the default event bus is used.
-- * 'principal' - The 12-digit AWS account ID that you are permitting to put events to your default event bus. Specify "*" to permit any account to put events to your default event bus.
--
-- If you specify "*" without specifying @Condition@ , avoid creating rules that may match undesirable events. To create more secure rules, make sure that the event pattern for each rule contains an @account@ field with a specific account ID from which to receive events. Rules with an account field do not match any events sent from other accounts.
-- * 'policy' - A JSON string that describes the permission policy statement. You can include a @Policy@ parameter in the request instead of using the @StatementId@ , @Action@ , @Principal@ , or @Condition@ parameters.
-- * 'statementId' - An identifier string for the external account that you are granting permissions to. If you later want to revoke the permission for this external account, specify this @StatementId@ when you run 'RemovePermission' .
-- * 'condition' - This parameter enables you to limit the permission to accounts that fulfill a certain condition, such as being a member of a certain AWS organization. For more information about AWS Organizations, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_introduction.html What Is AWS Organizations> in the /AWS Organizations User Guide/ .
--
-- If you specify @Condition@ with an AWS organization ID, and specify "*" as the value for @Principal@ , you grant permission to all the accounts in the named organization.
-- The @Condition@ is a JSON string which must contain @Type@ , @Key@ , and @Value@ fields.
mkPutPermission ::
  PutPermission
mkPutPermission =
  PutPermission'
    { action = Lude.Nothing,
      eventBusName = Lude.Nothing,
      principal = Lude.Nothing,
      policy = Lude.Nothing,
      statementId = Lude.Nothing,
      condition = Lude.Nothing
    }

-- | The action that you are enabling the other account to perform. Currently, this must be @events:PutEvents@ .
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppAction :: Lens.Lens' PutPermission (Lude.Maybe Lude.Text)
ppAction = Lens.lens (action :: PutPermission -> Lude.Maybe Lude.Text) (\s a -> s {action = a} :: PutPermission)
{-# DEPRECATED ppAction "Use generic-lens or generic-optics with 'action' instead." #-}

-- | The name of the event bus associated with the rule. If you omit this, the default event bus is used.
--
-- /Note:/ Consider using 'eventBusName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppEventBusName :: Lens.Lens' PutPermission (Lude.Maybe Lude.Text)
ppEventBusName = Lens.lens (eventBusName :: PutPermission -> Lude.Maybe Lude.Text) (\s a -> s {eventBusName = a} :: PutPermission)
{-# DEPRECATED ppEventBusName "Use generic-lens or generic-optics with 'eventBusName' instead." #-}

-- | The 12-digit AWS account ID that you are permitting to put events to your default event bus. Specify "*" to permit any account to put events to your default event bus.
--
-- If you specify "*" without specifying @Condition@ , avoid creating rules that may match undesirable events. To create more secure rules, make sure that the event pattern for each rule contains an @account@ field with a specific account ID from which to receive events. Rules with an account field do not match any events sent from other accounts.
--
-- /Note:/ Consider using 'principal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppPrincipal :: Lens.Lens' PutPermission (Lude.Maybe Lude.Text)
ppPrincipal = Lens.lens (principal :: PutPermission -> Lude.Maybe Lude.Text) (\s a -> s {principal = a} :: PutPermission)
{-# DEPRECATED ppPrincipal "Use generic-lens or generic-optics with 'principal' instead." #-}

-- | A JSON string that describes the permission policy statement. You can include a @Policy@ parameter in the request instead of using the @StatementId@ , @Action@ , @Principal@ , or @Condition@ parameters.
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppPolicy :: Lens.Lens' PutPermission (Lude.Maybe Lude.Text)
ppPolicy = Lens.lens (policy :: PutPermission -> Lude.Maybe Lude.Text) (\s a -> s {policy = a} :: PutPermission)
{-# DEPRECATED ppPolicy "Use generic-lens or generic-optics with 'policy' instead." #-}

-- | An identifier string for the external account that you are granting permissions to. If you later want to revoke the permission for this external account, specify this @StatementId@ when you run 'RemovePermission' .
--
-- /Note:/ Consider using 'statementId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppStatementId :: Lens.Lens' PutPermission (Lude.Maybe Lude.Text)
ppStatementId = Lens.lens (statementId :: PutPermission -> Lude.Maybe Lude.Text) (\s a -> s {statementId = a} :: PutPermission)
{-# DEPRECATED ppStatementId "Use generic-lens or generic-optics with 'statementId' instead." #-}

-- | This parameter enables you to limit the permission to accounts that fulfill a certain condition, such as being a member of a certain AWS organization. For more information about AWS Organizations, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_introduction.html What Is AWS Organizations> in the /AWS Organizations User Guide/ .
--
-- If you specify @Condition@ with an AWS organization ID, and specify "*" as the value for @Principal@ , you grant permission to all the accounts in the named organization.
-- The @Condition@ is a JSON string which must contain @Type@ , @Key@ , and @Value@ fields.
--
-- /Note:/ Consider using 'condition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppCondition :: Lens.Lens' PutPermission (Lude.Maybe Condition)
ppCondition = Lens.lens (condition :: PutPermission -> Lude.Maybe Condition) (\s a -> s {condition = a} :: PutPermission)
{-# DEPRECATED ppCondition "Use generic-lens or generic-optics with 'condition' instead." #-}

instance Lude.AWSRequest PutPermission where
  type Rs PutPermission = PutPermissionResponse
  request = Req.postJSON cloudWatchEventsService
  response = Res.receiveNull PutPermissionResponse'

instance Lude.ToHeaders PutPermission where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSEvents.PutPermission" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutPermission where
  toJSON PutPermission' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Action" Lude..=) Lude.<$> action,
            ("EventBusName" Lude..=) Lude.<$> eventBusName,
            ("Principal" Lude..=) Lude.<$> principal,
            ("Policy" Lude..=) Lude.<$> policy,
            ("StatementId" Lude..=) Lude.<$> statementId,
            ("Condition" Lude..=) Lude.<$> condition
          ]
      )

instance Lude.ToPath PutPermission where
  toPath = Lude.const "/"

instance Lude.ToQuery PutPermission where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutPermissionResponse' smart constructor.
data PutPermissionResponse = PutPermissionResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutPermissionResponse' with the minimum fields required to make a request.
mkPutPermissionResponse ::
  PutPermissionResponse
mkPutPermissionResponse = PutPermissionResponse'
