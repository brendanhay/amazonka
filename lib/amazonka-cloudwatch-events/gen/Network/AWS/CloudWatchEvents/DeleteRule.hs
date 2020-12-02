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
-- Module      : Network.AWS.CloudWatchEvents.DeleteRule
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified rule.
--
--
-- Before you can delete the rule, you must remove all targets, using 'RemoveTargets' .
--
-- When you delete a rule, incoming events might continue to match to the deleted rule. Allow a short period of time for changes to take effect.
--
-- Managed rules are rules created and managed by another AWS service on your behalf. These rules are created by those other AWS services to support functionality in those services. You can delete these rules using the @Force@ option, but you should do so only if you're sure that the other service isn't still using that rule.
--
module Network.AWS.CloudWatchEvents.DeleteRule
    (
    -- * Creating a Request
      deleteRule
    , DeleteRule
    -- * Request Lenses
    , drForce
    , drEventBusName
    , drName

    -- * Destructuring the Response
    , deleteRuleResponse
    , DeleteRuleResponse
    ) where

import Network.AWS.CloudWatchEvents.Types
import Network.AWS.CloudWatchEvents.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteRule' smart constructor.
data DeleteRule = DeleteRule'
  { _drForce        :: !(Maybe Bool)
  , _drEventBusName :: !(Maybe Text)
  , _drName         :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drForce' - If this is a managed rule, created by an AWS service on your behalf, you must specify @Force@ as @True@ to delete the rule. This parameter is ignored for rules that are not managed rules. You can check whether a rule is a managed rule by using @DescribeRule@ or @ListRules@ and checking the @ManagedBy@ field of the response.
--
-- * 'drEventBusName' - The event bus associated with the rule. If you omit this, the default event bus is used.
--
-- * 'drName' - The name of the rule.
deleteRule
    :: Text -- ^ 'drName'
    -> DeleteRule
deleteRule pName_ =
  DeleteRule' {_drForce = Nothing, _drEventBusName = Nothing, _drName = pName_}


-- | If this is a managed rule, created by an AWS service on your behalf, you must specify @Force@ as @True@ to delete the rule. This parameter is ignored for rules that are not managed rules. You can check whether a rule is a managed rule by using @DescribeRule@ or @ListRules@ and checking the @ManagedBy@ field of the response.
drForce :: Lens' DeleteRule (Maybe Bool)
drForce = lens _drForce (\ s a -> s{_drForce = a})

-- | The event bus associated with the rule. If you omit this, the default event bus is used.
drEventBusName :: Lens' DeleteRule (Maybe Text)
drEventBusName = lens _drEventBusName (\ s a -> s{_drEventBusName = a})

-- | The name of the rule.
drName :: Lens' DeleteRule Text
drName = lens _drName (\ s a -> s{_drName = a})

instance AWSRequest DeleteRule where
        type Rs DeleteRule = DeleteRuleResponse
        request = postJSON cloudWatchEvents
        response = receiveNull DeleteRuleResponse'

instance Hashable DeleteRule where

instance NFData DeleteRule where

instance ToHeaders DeleteRule where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSEvents.DeleteRule" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteRule where
        toJSON DeleteRule'{..}
          = object
              (catMaybes
                 [("Force" .=) <$> _drForce,
                  ("EventBusName" .=) <$> _drEventBusName,
                  Just ("Name" .= _drName)])

instance ToPath DeleteRule where
        toPath = const "/"

instance ToQuery DeleteRule where
        toQuery = const mempty

-- | /See:/ 'deleteRuleResponse' smart constructor.
data DeleteRuleResponse =
  DeleteRuleResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteRuleResponse' with the minimum fields required to make a request.
--
deleteRuleResponse
    :: DeleteRuleResponse
deleteRuleResponse = DeleteRuleResponse'


instance NFData DeleteRuleResponse where
