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
-- Module      : Network.AWS.SES.CloneReceiptRuleSet
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a receipt rule set by cloning an existing one. All receipt rules and configurations are copied to the new receipt rule set and are completely independent of the source rule set.
--
--
-- For information about setting up rule sets, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-receipt-rule-set.html Amazon SES Developer Guide> .
--
-- You can execute this operation no more than once per second.
--
module Network.AWS.SES.CloneReceiptRuleSet
    (
    -- * Creating a Request
      cloneReceiptRuleSet
    , CloneReceiptRuleSet
    -- * Request Lenses
    , cRuleSetName
    , cOriginalRuleSetName

    -- * Destructuring the Response
    , cloneReceiptRuleSetResponse
    , CloneReceiptRuleSetResponse
    -- * Response Lenses
    , crsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SES.Types
import Network.AWS.SES.Types.Product

-- | Represents a request to create a receipt rule set by cloning an existing one. You use receipt rule sets to receive email with Amazon SES. For more information, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html Amazon SES Developer Guide> .
--
--
--
-- /See:/ 'cloneReceiptRuleSet' smart constructor.
data CloneReceiptRuleSet = CloneReceiptRuleSet'
  { _cRuleSetName         :: !Text
  , _cOriginalRuleSetName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CloneReceiptRuleSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cRuleSetName' - The name of the rule set to create. The name must:     * This value can only contain ASCII letters (a-z, A-Z), numbers (0-9), underscores (_), or dashes (-).     * Start and end with a letter or number.     * Contain less than 64 characters.
--
-- * 'cOriginalRuleSetName' - The name of the rule set to clone.
cloneReceiptRuleSet
    :: Text -- ^ 'cRuleSetName'
    -> Text -- ^ 'cOriginalRuleSetName'
    -> CloneReceiptRuleSet
cloneReceiptRuleSet pRuleSetName_ pOriginalRuleSetName_ =
  CloneReceiptRuleSet'
    { _cRuleSetName = pRuleSetName_
    , _cOriginalRuleSetName = pOriginalRuleSetName_
    }


-- | The name of the rule set to create. The name must:     * This value can only contain ASCII letters (a-z, A-Z), numbers (0-9), underscores (_), or dashes (-).     * Start and end with a letter or number.     * Contain less than 64 characters.
cRuleSetName :: Lens' CloneReceiptRuleSet Text
cRuleSetName = lens _cRuleSetName (\ s a -> s{_cRuleSetName = a})

-- | The name of the rule set to clone.
cOriginalRuleSetName :: Lens' CloneReceiptRuleSet Text
cOriginalRuleSetName = lens _cOriginalRuleSetName (\ s a -> s{_cOriginalRuleSetName = a})

instance AWSRequest CloneReceiptRuleSet where
        type Rs CloneReceiptRuleSet =
             CloneReceiptRuleSetResponse
        request = postQuery ses
        response
          = receiveXMLWrapper "CloneReceiptRuleSetResult"
              (\ s h x ->
                 CloneReceiptRuleSetResponse' <$> (pure (fromEnum s)))

instance Hashable CloneReceiptRuleSet where

instance NFData CloneReceiptRuleSet where

instance ToHeaders CloneReceiptRuleSet where
        toHeaders = const mempty

instance ToPath CloneReceiptRuleSet where
        toPath = const "/"

instance ToQuery CloneReceiptRuleSet where
        toQuery CloneReceiptRuleSet'{..}
          = mconcat
              ["Action" =: ("CloneReceiptRuleSet" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "RuleSetName" =: _cRuleSetName,
               "OriginalRuleSetName" =: _cOriginalRuleSetName]

-- | An empty element returned on a successful request.
--
--
--
-- /See:/ 'cloneReceiptRuleSetResponse' smart constructor.
newtype CloneReceiptRuleSetResponse = CloneReceiptRuleSetResponse'
  { _crsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CloneReceiptRuleSetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crsResponseStatus' - -- | The response status code.
cloneReceiptRuleSetResponse
    :: Int -- ^ 'crsResponseStatus'
    -> CloneReceiptRuleSetResponse
cloneReceiptRuleSetResponse pResponseStatus_ =
  CloneReceiptRuleSetResponse' {_crsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
crsResponseStatus :: Lens' CloneReceiptRuleSetResponse Int
crsResponseStatus = lens _crsResponseStatus (\ s a -> s{_crsResponseStatus = a})

instance NFData CloneReceiptRuleSetResponse where
