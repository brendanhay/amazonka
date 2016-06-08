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
-- Module      : Network.AWS.SES.SetActiveReceiptRuleSet
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the specified receipt rule set as the active receipt rule set.
--
-- To disable your email-receiving through Amazon SES completely, you can call this API with RuleSetName set to null.
--
-- For information about managing receipt rule sets, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-managing-receipt-rule-sets.html Amazon SES Developer Guide>.
--
-- This action is throttled at one request per second.
module Network.AWS.SES.SetActiveReceiptRuleSet
    (
    -- * Creating a Request
      setActiveReceiptRuleSet
    , SetActiveReceiptRuleSet
    -- * Request Lenses
    , sarrsRuleSetName

    -- * Destructuring the Response
    , setActiveReceiptRuleSetResponse
    , SetActiveReceiptRuleSetResponse
    -- * Response Lenses
    , sarrsrsResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SES.Types
import           Network.AWS.SES.Types.Product

-- | /See:/ 'setActiveReceiptRuleSet' smart constructor.
newtype SetActiveReceiptRuleSet = SetActiveReceiptRuleSet'
    { _sarrsRuleSetName :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SetActiveReceiptRuleSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sarrsRuleSetName'
setActiveReceiptRuleSet
    :: SetActiveReceiptRuleSet
setActiveReceiptRuleSet =
    SetActiveReceiptRuleSet'
    { _sarrsRuleSetName = Nothing
    }

-- | The name of the receipt rule set to make active. Setting this value to null disables all email receiving.
sarrsRuleSetName :: Lens' SetActiveReceiptRuleSet (Maybe Text)
sarrsRuleSetName = lens _sarrsRuleSetName (\ s a -> s{_sarrsRuleSetName = a});

instance AWSRequest SetActiveReceiptRuleSet where
        type Rs SetActiveReceiptRuleSet =
             SetActiveReceiptRuleSetResponse
        request = postQuery ses
        response
          = receiveXMLWrapper "SetActiveReceiptRuleSetResult"
              (\ s h x ->
                 SetActiveReceiptRuleSetResponse' <$>
                   (pure (fromEnum s)))

instance Hashable SetActiveReceiptRuleSet

instance NFData SetActiveReceiptRuleSet

instance ToHeaders SetActiveReceiptRuleSet where
        toHeaders = const mempty

instance ToPath SetActiveReceiptRuleSet where
        toPath = const "/"

instance ToQuery SetActiveReceiptRuleSet where
        toQuery SetActiveReceiptRuleSet'{..}
          = mconcat
              ["Action" =:
                 ("SetActiveReceiptRuleSet" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "RuleSetName" =: _sarrsRuleSetName]

-- | /See:/ 'setActiveReceiptRuleSetResponse' smart constructor.
newtype SetActiveReceiptRuleSetResponse = SetActiveReceiptRuleSetResponse'
    { _sarrsrsResponseStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SetActiveReceiptRuleSetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sarrsrsResponseStatus'
setActiveReceiptRuleSetResponse
    :: Int -- ^ 'sarrsrsResponseStatus'
    -> SetActiveReceiptRuleSetResponse
setActiveReceiptRuleSetResponse pResponseStatus_ =
    SetActiveReceiptRuleSetResponse'
    { _sarrsrsResponseStatus = pResponseStatus_
    }

-- | The response status code.
sarrsrsResponseStatus :: Lens' SetActiveReceiptRuleSetResponse Int
sarrsrsResponseStatus = lens _sarrsrsResponseStatus (\ s a -> s{_sarrsrsResponseStatus = a});

instance NFData SetActiveReceiptRuleSetResponse
