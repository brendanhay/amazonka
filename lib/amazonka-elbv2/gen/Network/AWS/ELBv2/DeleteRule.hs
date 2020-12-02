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
-- Module      : Network.AWS.ELBv2.DeleteRule
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified rule.
--
--
module Network.AWS.ELBv2.DeleteRule
    (
    -- * Creating a Request
      deleteRule
    , DeleteRule
    -- * Request Lenses
    , drRuleARN

    -- * Destructuring the Response
    , deleteRuleResponse
    , DeleteRuleResponse
    -- * Response Lenses
    , drrsResponseStatus
    ) where

import Network.AWS.ELBv2.Types
import Network.AWS.ELBv2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteRule' smart constructor.
newtype DeleteRule = DeleteRule'
  { _drRuleARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drRuleARN' - The Amazon Resource Name (ARN) of the rule.
deleteRule
    :: Text -- ^ 'drRuleARN'
    -> DeleteRule
deleteRule pRuleARN_ = DeleteRule' {_drRuleARN = pRuleARN_}


-- | The Amazon Resource Name (ARN) of the rule.
drRuleARN :: Lens' DeleteRule Text
drRuleARN = lens _drRuleARN (\ s a -> s{_drRuleARN = a})

instance AWSRequest DeleteRule where
        type Rs DeleteRule = DeleteRuleResponse
        request = postQuery eLBv2
        response
          = receiveXMLWrapper "DeleteRuleResult"
              (\ s h x ->
                 DeleteRuleResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteRule where

instance NFData DeleteRule where

instance ToHeaders DeleteRule where
        toHeaders = const mempty

instance ToPath DeleteRule where
        toPath = const "/"

instance ToQuery DeleteRule where
        toQuery DeleteRule'{..}
          = mconcat
              ["Action" =: ("DeleteRule" :: ByteString),
               "Version" =: ("2015-12-01" :: ByteString),
               "RuleArn" =: _drRuleARN]

-- | /See:/ 'deleteRuleResponse' smart constructor.
newtype DeleteRuleResponse = DeleteRuleResponse'
  { _drrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteRuleResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drrsResponseStatus' - -- | The response status code.
deleteRuleResponse
    :: Int -- ^ 'drrsResponseStatus'
    -> DeleteRuleResponse
deleteRuleResponse pResponseStatus_ =
  DeleteRuleResponse' {_drrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
drrsResponseStatus :: Lens' DeleteRuleResponse Int
drrsResponseStatus = lens _drrsResponseStatus (\ s a -> s{_drrsResponseStatus = a})

instance NFData DeleteRuleResponse where
