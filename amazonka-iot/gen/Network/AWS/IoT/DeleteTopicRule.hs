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
-- Module      : Network.AWS.IoT.DeleteTopicRule
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified rule.
--
-- /See:/ <https://aws.amazon.com/iot#DeleteTopicRule.html AWS API Reference> for DeleteTopicRule.
module Network.AWS.IoT.DeleteTopicRule
    (
    -- * Creating a Request
      deleteTopicRule
    , DeleteTopicRule
    -- * Request Lenses
    , dtrRuleName

    -- * Destructuring the Response
    , deleteTopicRuleResponse
    , DeleteTopicRuleResponse
    ) where

import           Network.AWS.IoT.Types
import           Network.AWS.IoT.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The input for the DeleteTopicRule operation.
--
-- /See:/ 'deleteTopicRule' smart constructor.
newtype DeleteTopicRule = DeleteTopicRule'
    { _dtrRuleName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteTopicRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtrRuleName'
deleteTopicRule
    :: Text -- ^ 'dtrRuleName'
    -> DeleteTopicRule
deleteTopicRule pRuleName_ =
    DeleteTopicRule'
    { _dtrRuleName = pRuleName_
    }

-- | The name of the rule.
dtrRuleName :: Lens' DeleteTopicRule Text
dtrRuleName = lens _dtrRuleName (\ s a -> s{_dtrRuleName = a});

instance AWSRequest DeleteTopicRule where
        type Rs DeleteTopicRule = DeleteTopicRuleResponse
        request = delete ioT
        response = receiveNull DeleteTopicRuleResponse'

instance ToHeaders DeleteTopicRule where
        toHeaders = const mempty

instance ToPath DeleteTopicRule where
        toPath DeleteTopicRule'{..}
          = mconcat ["/rules/", toBS _dtrRuleName]

instance ToQuery DeleteTopicRule where
        toQuery = const mempty

-- | /See:/ 'deleteTopicRuleResponse' smart constructor.
data DeleteTopicRuleResponse =
    DeleteTopicRuleResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteTopicRuleResponse' with the minimum fields required to make a request.
--
deleteTopicRuleResponse
    :: DeleteTopicRuleResponse
deleteTopicRuleResponse = DeleteTopicRuleResponse'
