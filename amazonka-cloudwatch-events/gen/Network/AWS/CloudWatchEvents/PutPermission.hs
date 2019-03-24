{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.PutPermission
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Running @PutPermission@ permits the specified AWS account or AWS organization to put events to your account's default /event bus/ . CloudWatch Events rules in your account are triggered by these events arriving to your default event bus.
--
--
-- For another account to send events to your account, that external account must have a CloudWatch Events rule with your account's default event bus as a target.
--
-- To enable multiple AWS accounts to put events to your default event bus, run @PutPermission@ once for each of these accounts. Or, if all the accounts are members of the same AWS organization, you can run @PutPermission@ once specifying @Principal@ as "*" and specifying the AWS organization ID in @Condition@ , to grant permissions to all accounts in that organization.
--
-- If you grant permissions using an organization, then accounts in that organization must specify a @RoleArn@ with proper permissions when they use @PutTarget@ to add your account's event bus as a target. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/events/CloudWatchEvents-CrossAccountEventDelivery.html Sending and Receiving Events Between AWS Accounts> in the /Amazon CloudWatch Events User Guide/ .
--
-- The permission policy on the default event bus cannot exceed 10 KB in size.
--
module Network.AWS.CloudWatchEvents.PutPermission
    (
    -- * Creating a Request
      putPermission
    , PutPermission
    -- * Request Lenses
    , ppCondition
    , ppAction
    , ppPrincipal
    , ppStatementId

    -- * Destructuring the Response
    , putPermissionResponse
    , PutPermissionResponse
    ) where

import Network.AWS.CloudWatchEvents.Types
import Network.AWS.CloudWatchEvents.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putPermission' smart constructor.
data PutPermission = PutPermission'
  { _ppCondition   :: !(Maybe Condition)
  , _ppAction      :: !Text
  , _ppPrincipal   :: !Text
  , _ppStatementId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutPermission' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ppCondition' - This parameter enables you to limit the permission to accounts that fulfill a certain condition, such as being a member of a certain AWS organization. For more information about AWS Organizations, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_introduction.html What Is AWS Organizations> in the /AWS Organizations User Guide/ . If you specify @Condition@ with an AWS organization ID, and specify "*" as the value for @Principal@ , you grant permission to all the accounts in the named organization. The @Condition@ is a JSON string which must contain @Type@ , @Key@ , and @Value@ fields.
--
-- * 'ppAction' - The action that you are enabling the other account to perform. Currently, this must be @events:PutEvents@ .
--
-- * 'ppPrincipal' - The 12-digit AWS account ID that you are permitting to put events to your default event bus. Specify "*" to permit any account to put events to your default event bus. If you specify "*" without specifying @Condition@ , avoid creating rules that may match undesirable events. To create more secure rules, make sure that the event pattern for each rule contains an @account@ field with a specific account ID from which to receive events. Rules with an account field do not match any events sent from other accounts.
--
-- * 'ppStatementId' - An identifier string for the external account that you are granting permissions to. If you later want to revoke the permission for this external account, specify this @StatementId@ when you run 'RemovePermission' .
putPermission
    :: Text -- ^ 'ppAction'
    -> Text -- ^ 'ppPrincipal'
    -> Text -- ^ 'ppStatementId'
    -> PutPermission
putPermission pAction_ pPrincipal_ pStatementId_ =
  PutPermission'
    { _ppCondition = Nothing
    , _ppAction = pAction_
    , _ppPrincipal = pPrincipal_
    , _ppStatementId = pStatementId_
    }


-- | This parameter enables you to limit the permission to accounts that fulfill a certain condition, such as being a member of a certain AWS organization. For more information about AWS Organizations, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_introduction.html What Is AWS Organizations> in the /AWS Organizations User Guide/ . If you specify @Condition@ with an AWS organization ID, and specify "*" as the value for @Principal@ , you grant permission to all the accounts in the named organization. The @Condition@ is a JSON string which must contain @Type@ , @Key@ , and @Value@ fields.
ppCondition :: Lens' PutPermission (Maybe Condition)
ppCondition = lens _ppCondition (\ s a -> s{_ppCondition = a})

-- | The action that you are enabling the other account to perform. Currently, this must be @events:PutEvents@ .
ppAction :: Lens' PutPermission Text
ppAction = lens _ppAction (\ s a -> s{_ppAction = a})

-- | The 12-digit AWS account ID that you are permitting to put events to your default event bus. Specify "*" to permit any account to put events to your default event bus. If you specify "*" without specifying @Condition@ , avoid creating rules that may match undesirable events. To create more secure rules, make sure that the event pattern for each rule contains an @account@ field with a specific account ID from which to receive events. Rules with an account field do not match any events sent from other accounts.
ppPrincipal :: Lens' PutPermission Text
ppPrincipal = lens _ppPrincipal (\ s a -> s{_ppPrincipal = a})

-- | An identifier string for the external account that you are granting permissions to. If you later want to revoke the permission for this external account, specify this @StatementId@ when you run 'RemovePermission' .
ppStatementId :: Lens' PutPermission Text
ppStatementId = lens _ppStatementId (\ s a -> s{_ppStatementId = a})

instance AWSRequest PutPermission where
        type Rs PutPermission = PutPermissionResponse
        request = postJSON cloudWatchEvents
        response = receiveNull PutPermissionResponse'

instance Hashable PutPermission where

instance NFData PutPermission where

instance ToHeaders PutPermission where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSEvents.PutPermission" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutPermission where
        toJSON PutPermission'{..}
          = object
              (catMaybes
                 [("Condition" .=) <$> _ppCondition,
                  Just ("Action" .= _ppAction),
                  Just ("Principal" .= _ppPrincipal),
                  Just ("StatementId" .= _ppStatementId)])

instance ToPath PutPermission where
        toPath = const "/"

instance ToQuery PutPermission where
        toQuery = const mempty

-- | /See:/ 'putPermissionResponse' smart constructor.
data PutPermissionResponse =
  PutPermissionResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutPermissionResponse' with the minimum fields required to make a request.
--
putPermissionResponse
    :: PutPermissionResponse
putPermissionResponse = PutPermissionResponse'


instance NFData PutPermissionResponse where
