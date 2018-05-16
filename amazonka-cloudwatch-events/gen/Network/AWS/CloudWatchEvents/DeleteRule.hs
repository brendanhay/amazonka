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
-- You must remove all targets from a rule using 'RemoveTargets' before you can delete the rule.
--
-- When you delete a rule, incoming events might continue to match to the deleted rule. Please allow a short period of time for changes to take effect.
--
module Network.AWS.CloudWatchEvents.DeleteRule
    (
    -- * Creating a Request
      deleteRule
    , DeleteRule
    -- * Request Lenses
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
newtype DeleteRule = DeleteRule'
  { _drName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drName' - The name of the rule.
deleteRule
    :: Text -- ^ 'drName'
    -> DeleteRule
deleteRule pName_ = DeleteRule' {_drName = pName_}


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
          = object (catMaybes [Just ("Name" .= _drName)])

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
