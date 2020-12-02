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
-- Module      : Network.AWS.SES.SetReceiptRulePosition
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the position of the specified receipt rule in the receipt rule set.
--
--
-- For information about managing receipt rules, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-managing-receipt-rules.html Amazon SES Developer Guide> .
--
-- You can execute this operation no more than once per second.
--
module Network.AWS.SES.SetReceiptRulePosition
    (
    -- * Creating a Request
      setReceiptRulePosition
    , SetReceiptRulePosition
    -- * Request Lenses
    , srrpAfter
    , srrpRuleSetName
    , srrpRuleName

    -- * Destructuring the Response
    , setReceiptRulePositionResponse
    , SetReceiptRulePositionResponse
    -- * Response Lenses
    , srrprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SES.Types
import Network.AWS.SES.Types.Product

-- | Represents a request to set the position of a receipt rule in a receipt rule set. You use receipt rule sets to receive email with Amazon SES. For more information, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html Amazon SES Developer Guide> .
--
--
--
-- /See:/ 'setReceiptRulePosition' smart constructor.
data SetReceiptRulePosition = SetReceiptRulePosition'
  { _srrpAfter       :: !(Maybe Text)
  , _srrpRuleSetName :: !Text
  , _srrpRuleName    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetReceiptRulePosition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srrpAfter' - The name of the receipt rule after which to place the specified receipt rule.
--
-- * 'srrpRuleSetName' - The name of the receipt rule set that contains the receipt rule to reposition.
--
-- * 'srrpRuleName' - The name of the receipt rule to reposition.
setReceiptRulePosition
    :: Text -- ^ 'srrpRuleSetName'
    -> Text -- ^ 'srrpRuleName'
    -> SetReceiptRulePosition
setReceiptRulePosition pRuleSetName_ pRuleName_ =
  SetReceiptRulePosition'
    { _srrpAfter = Nothing
    , _srrpRuleSetName = pRuleSetName_
    , _srrpRuleName = pRuleName_
    }


-- | The name of the receipt rule after which to place the specified receipt rule.
srrpAfter :: Lens' SetReceiptRulePosition (Maybe Text)
srrpAfter = lens _srrpAfter (\ s a -> s{_srrpAfter = a})

-- | The name of the receipt rule set that contains the receipt rule to reposition.
srrpRuleSetName :: Lens' SetReceiptRulePosition Text
srrpRuleSetName = lens _srrpRuleSetName (\ s a -> s{_srrpRuleSetName = a})

-- | The name of the receipt rule to reposition.
srrpRuleName :: Lens' SetReceiptRulePosition Text
srrpRuleName = lens _srrpRuleName (\ s a -> s{_srrpRuleName = a})

instance AWSRequest SetReceiptRulePosition where
        type Rs SetReceiptRulePosition =
             SetReceiptRulePositionResponse
        request = postQuery ses
        response
          = receiveXMLWrapper "SetReceiptRulePositionResult"
              (\ s h x ->
                 SetReceiptRulePositionResponse' <$>
                   (pure (fromEnum s)))

instance Hashable SetReceiptRulePosition where

instance NFData SetReceiptRulePosition where

instance ToHeaders SetReceiptRulePosition where
        toHeaders = const mempty

instance ToPath SetReceiptRulePosition where
        toPath = const "/"

instance ToQuery SetReceiptRulePosition where
        toQuery SetReceiptRulePosition'{..}
          = mconcat
              ["Action" =:
                 ("SetReceiptRulePosition" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "After" =: _srrpAfter,
               "RuleSetName" =: _srrpRuleSetName,
               "RuleName" =: _srrpRuleName]

-- | An empty element returned on a successful request.
--
--
--
-- /See:/ 'setReceiptRulePositionResponse' smart constructor.
newtype SetReceiptRulePositionResponse = SetReceiptRulePositionResponse'
  { _srrprsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetReceiptRulePositionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srrprsResponseStatus' - -- | The response status code.
setReceiptRulePositionResponse
    :: Int -- ^ 'srrprsResponseStatus'
    -> SetReceiptRulePositionResponse
setReceiptRulePositionResponse pResponseStatus_ =
  SetReceiptRulePositionResponse' {_srrprsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
srrprsResponseStatus :: Lens' SetReceiptRulePositionResponse Int
srrprsResponseStatus = lens _srrprsResponseStatus (\ s a -> s{_srrprsResponseStatus = a})

instance NFData SetReceiptRulePositionResponse where
