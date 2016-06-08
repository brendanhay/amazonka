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
-- Module      : Network.AWS.SES.CreateReceiptRuleSet
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an empty receipt rule set.
--
-- For information about setting up receipt rule sets, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-receipt-rule-set.html Amazon SES Developer Guide>.
--
-- This action is throttled at one request per second.
module Network.AWS.SES.CreateReceiptRuleSet
    (
    -- * Creating a Request
      createReceiptRuleSet
    , CreateReceiptRuleSet
    -- * Request Lenses
    , crrsRuleSetName

    -- * Destructuring the Response
    , createReceiptRuleSetResponse
    , CreateReceiptRuleSetResponse
    -- * Response Lenses
    , crrsrsResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SES.Types
import           Network.AWS.SES.Types.Product

-- | /See:/ 'createReceiptRuleSet' smart constructor.
newtype CreateReceiptRuleSet = CreateReceiptRuleSet'
    { _crrsRuleSetName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateReceiptRuleSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crrsRuleSetName'
createReceiptRuleSet
    :: Text -- ^ 'crrsRuleSetName'
    -> CreateReceiptRuleSet
createReceiptRuleSet pRuleSetName_ =
    CreateReceiptRuleSet'
    { _crrsRuleSetName = pRuleSetName_
    }

-- | The name of the rule set to create. The name must:
--
-- -   Contain only ASCII letters (a-z, A-Z), numbers (0-9), periods (.), underscores (_), or dashes (-).
-- -   Start and end with a letter or number.
-- -   Contain less than 64 characters.
crrsRuleSetName :: Lens' CreateReceiptRuleSet Text
crrsRuleSetName = lens _crrsRuleSetName (\ s a -> s{_crrsRuleSetName = a});

instance AWSRequest CreateReceiptRuleSet where
        type Rs CreateReceiptRuleSet =
             CreateReceiptRuleSetResponse
        request = postQuery ses
        response
          = receiveXMLWrapper "CreateReceiptRuleSetResult"
              (\ s h x ->
                 CreateReceiptRuleSetResponse' <$>
                   (pure (fromEnum s)))

instance Hashable CreateReceiptRuleSet

instance NFData CreateReceiptRuleSet

instance ToHeaders CreateReceiptRuleSet where
        toHeaders = const mempty

instance ToPath CreateReceiptRuleSet where
        toPath = const "/"

instance ToQuery CreateReceiptRuleSet where
        toQuery CreateReceiptRuleSet'{..}
          = mconcat
              ["Action" =: ("CreateReceiptRuleSet" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "RuleSetName" =: _crrsRuleSetName]

-- | /See:/ 'createReceiptRuleSetResponse' smart constructor.
newtype CreateReceiptRuleSetResponse = CreateReceiptRuleSetResponse'
    { _crrsrsResponseStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateReceiptRuleSetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crrsrsResponseStatus'
createReceiptRuleSetResponse
    :: Int -- ^ 'crrsrsResponseStatus'
    -> CreateReceiptRuleSetResponse
createReceiptRuleSetResponse pResponseStatus_ =
    CreateReceiptRuleSetResponse'
    { _crrsrsResponseStatus = pResponseStatus_
    }

-- | The response status code.
crrsrsResponseStatus :: Lens' CreateReceiptRuleSetResponse Int
crrsrsResponseStatus = lens _crrsrsResponseStatus (\ s a -> s{_crrsrsResponseStatus = a});

instance NFData CreateReceiptRuleSetResponse
