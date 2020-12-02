{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
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
--
-- For another account to send events to your account, that external account must have an EventBridge rule with your account's event bus as a target.
--
-- To enable multiple AWS accounts to put events to your event bus, run @PutPermission@ once for each of these accounts. Or, if all the accounts are members of the same AWS organization, you can run @PutPermission@ once specifying @Principal@ as "*" and specifying the AWS organization ID in @Condition@ , to grant permissions to all accounts in that organization.
--
-- If you grant permissions using an organization, then accounts in that organization must specify a @RoleArn@ with proper permissions when they use @PutTarget@ to add your account's event bus as a target. For more information, see <https://docs.aws.amazon.com/eventbridge/latest/userguide/eventbridge-cross-account-event-delivery.html Sending and Receiving Events Between AWS Accounts> in the /Amazon EventBridge User Guide/ .
--
-- The permission policy on the default event bus cannot exceed 10 KB in size.
module Network.AWS.CloudWatchEvents.PutPermission
  ( -- * Creating a Request
    putPermission,
    PutPermission,

    -- * Request Lenses
    ppAction,
    ppEventBusName,
    ppPrincipal,
    ppPolicy,
    ppStatementId,
    ppCondition,

    -- * Destructuring the Response
    putPermissionResponse,
    PutPermissionResponse,
  )
where

import Network.AWS.CloudWatchEvents.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putPermission' smart constructor.
data PutPermission = PutPermission'
  { _ppAction :: !(Maybe Text),
    _ppEventBusName :: !(Maybe Text),
    _ppPrincipal :: !(Maybe Text),
    _ppPolicy :: !(Maybe Text),
    _ppStatementId :: !(Maybe Text),
    _ppCondition :: !(Maybe Condition)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutPermission' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ppAction' - The action that you are enabling the other account to perform. Currently, this must be @events:PutEvents@ .
--
-- * 'ppEventBusName' - The name of the event bus associated with the rule. If you omit this, the default event bus is used.
--
-- * 'ppPrincipal' - The 12-digit AWS account ID that you are permitting to put events to your default event bus. Specify "*" to permit any account to put events to your default event bus. If you specify "*" without specifying @Condition@ , avoid creating rules that may match undesirable events. To create more secure rules, make sure that the event pattern for each rule contains an @account@ field with a specific account ID from which to receive events. Rules with an account field do not match any events sent from other accounts.
--
-- * 'ppPolicy' - A JSON string that describes the permission policy statement. You can include a @Policy@ parameter in the request instead of using the @StatementId@ , @Action@ , @Principal@ , or @Condition@ parameters.
--
-- * 'ppStatementId' - An identifier string for the external account that you are granting permissions to. If you later want to revoke the permission for this external account, specify this @StatementId@ when you run 'RemovePermission' .
--
-- * 'ppCondition' - This parameter enables you to limit the permission to accounts that fulfill a certain condition, such as being a member of a certain AWS organization. For more information about AWS Organizations, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_introduction.html What Is AWS Organizations> in the /AWS Organizations User Guide/ . If you specify @Condition@ with an AWS organization ID, and specify "*" as the value for @Principal@ , you grant permission to all the accounts in the named organization. The @Condition@ is a JSON string which must contain @Type@ , @Key@ , and @Value@ fields.
putPermission ::
  PutPermission
putPermission =
  PutPermission'
    { _ppAction = Nothing,
      _ppEventBusName = Nothing,
      _ppPrincipal = Nothing,
      _ppPolicy = Nothing,
      _ppStatementId = Nothing,
      _ppCondition = Nothing
    }

-- | The action that you are enabling the other account to perform. Currently, this must be @events:PutEvents@ .
ppAction :: Lens' PutPermission (Maybe Text)
ppAction = lens _ppAction (\s a -> s {_ppAction = a})

-- | The name of the event bus associated with the rule. If you omit this, the default event bus is used.
ppEventBusName :: Lens' PutPermission (Maybe Text)
ppEventBusName = lens _ppEventBusName (\s a -> s {_ppEventBusName = a})

-- | The 12-digit AWS account ID that you are permitting to put events to your default event bus. Specify "*" to permit any account to put events to your default event bus. If you specify "*" without specifying @Condition@ , avoid creating rules that may match undesirable events. To create more secure rules, make sure that the event pattern for each rule contains an @account@ field with a specific account ID from which to receive events. Rules with an account field do not match any events sent from other accounts.
ppPrincipal :: Lens' PutPermission (Maybe Text)
ppPrincipal = lens _ppPrincipal (\s a -> s {_ppPrincipal = a})

-- | A JSON string that describes the permission policy statement. You can include a @Policy@ parameter in the request instead of using the @StatementId@ , @Action@ , @Principal@ , or @Condition@ parameters.
ppPolicy :: Lens' PutPermission (Maybe Text)
ppPolicy = lens _ppPolicy (\s a -> s {_ppPolicy = a})

-- | An identifier string for the external account that you are granting permissions to. If you later want to revoke the permission for this external account, specify this @StatementId@ when you run 'RemovePermission' .
ppStatementId :: Lens' PutPermission (Maybe Text)
ppStatementId = lens _ppStatementId (\s a -> s {_ppStatementId = a})

-- | This parameter enables you to limit the permission to accounts that fulfill a certain condition, such as being a member of a certain AWS organization. For more information about AWS Organizations, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_introduction.html What Is AWS Organizations> in the /AWS Organizations User Guide/ . If you specify @Condition@ with an AWS organization ID, and specify "*" as the value for @Principal@ , you grant permission to all the accounts in the named organization. The @Condition@ is a JSON string which must contain @Type@ , @Key@ , and @Value@ fields.
ppCondition :: Lens' PutPermission (Maybe Condition)
ppCondition = lens _ppCondition (\s a -> s {_ppCondition = a})

instance AWSRequest PutPermission where
  type Rs PutPermission = PutPermissionResponse
  request = postJSON cloudWatchEvents
  response = receiveNull PutPermissionResponse'

instance Hashable PutPermission

instance NFData PutPermission

instance ToHeaders PutPermission where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AWSEvents.PutPermission" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON PutPermission where
  toJSON PutPermission' {..} =
    object
      ( catMaybes
          [ ("Action" .=) <$> _ppAction,
            ("EventBusName" .=) <$> _ppEventBusName,
            ("Principal" .=) <$> _ppPrincipal,
            ("Policy" .=) <$> _ppPolicy,
            ("StatementId" .=) <$> _ppStatementId,
            ("Condition" .=) <$> _ppCondition
          ]
      )

instance ToPath PutPermission where
  toPath = const "/"

instance ToQuery PutPermission where
  toQuery = const mempty

-- | /See:/ 'putPermissionResponse' smart constructor.
data PutPermissionResponse = PutPermissionResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutPermissionResponse' with the minimum fields required to make a request.
putPermissionResponse ::
  PutPermissionResponse
putPermissionResponse = PutPermissionResponse'

instance NFData PutPermissionResponse
