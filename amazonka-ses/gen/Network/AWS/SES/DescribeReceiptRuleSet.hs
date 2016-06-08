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
-- Module      : Network.AWS.SES.DescribeReceiptRuleSet
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the details of the specified receipt rule set.
--
-- For information about managing receipt rule sets, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-managing-receipt-rule-sets.html Amazon SES Developer Guide>.
--
-- This action is throttled at one request per second.
module Network.AWS.SES.DescribeReceiptRuleSet
    (
    -- * Creating a Request
      describeReceiptRuleSet
    , DescribeReceiptRuleSet
    -- * Request Lenses
    , drrsRuleSetName

    -- * Destructuring the Response
    , describeReceiptRuleSetResponse
    , DescribeReceiptRuleSetResponse
    -- * Response Lenses
    , desrsRules
    , desrsMetadata
    , desrsResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SES.Types
import           Network.AWS.SES.Types.Product

-- | /See:/ 'describeReceiptRuleSet' smart constructor.
newtype DescribeReceiptRuleSet = DescribeReceiptRuleSet'
    { _drrsRuleSetName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeReceiptRuleSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drrsRuleSetName'
describeReceiptRuleSet
    :: Text -- ^ 'drrsRuleSetName'
    -> DescribeReceiptRuleSet
describeReceiptRuleSet pRuleSetName_ =
    DescribeReceiptRuleSet'
    { _drrsRuleSetName = pRuleSetName_
    }

-- | The name of the receipt rule set to describe.
drrsRuleSetName :: Lens' DescribeReceiptRuleSet Text
drrsRuleSetName = lens _drrsRuleSetName (\ s a -> s{_drrsRuleSetName = a});

instance AWSRequest DescribeReceiptRuleSet where
        type Rs DescribeReceiptRuleSet =
             DescribeReceiptRuleSetResponse
        request = postQuery ses
        response
          = receiveXMLWrapper "DescribeReceiptRuleSetResult"
              (\ s h x ->
                 DescribeReceiptRuleSetResponse' <$>
                   (x .@? "Rules" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (x .@? "Metadata")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeReceiptRuleSet

instance NFData DescribeReceiptRuleSet

instance ToHeaders DescribeReceiptRuleSet where
        toHeaders = const mempty

instance ToPath DescribeReceiptRuleSet where
        toPath = const "/"

instance ToQuery DescribeReceiptRuleSet where
        toQuery DescribeReceiptRuleSet'{..}
          = mconcat
              ["Action" =:
                 ("DescribeReceiptRuleSet" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "RuleSetName" =: _drrsRuleSetName]

-- | /See:/ 'describeReceiptRuleSetResponse' smart constructor.
data DescribeReceiptRuleSetResponse = DescribeReceiptRuleSetResponse'
    { _desrsRules          :: !(Maybe [ReceiptRule])
    , _desrsMetadata       :: !(Maybe ReceiptRuleSetMetadata)
    , _desrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeReceiptRuleSetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desrsRules'
--
-- * 'desrsMetadata'
--
-- * 'desrsResponseStatus'
describeReceiptRuleSetResponse
    :: Int -- ^ 'desrsResponseStatus'
    -> DescribeReceiptRuleSetResponse
describeReceiptRuleSetResponse pResponseStatus_ =
    DescribeReceiptRuleSetResponse'
    { _desrsRules = Nothing
    , _desrsMetadata = Nothing
    , _desrsResponseStatus = pResponseStatus_
    }

-- | A list of the receipt rules that belong to the specified receipt rule set.
desrsRules :: Lens' DescribeReceiptRuleSetResponse [ReceiptRule]
desrsRules = lens _desrsRules (\ s a -> s{_desrsRules = a}) . _Default . _Coerce;

-- | The metadata for the receipt rule set, which consists of the rule set name and the timestamp of when the rule set was created.
desrsMetadata :: Lens' DescribeReceiptRuleSetResponse (Maybe ReceiptRuleSetMetadata)
desrsMetadata = lens _desrsMetadata (\ s a -> s{_desrsMetadata = a});

-- | The response status code.
desrsResponseStatus :: Lens' DescribeReceiptRuleSetResponse Int
desrsResponseStatus = lens _desrsResponseStatus (\ s a -> s{_desrsResponseStatus = a});

instance NFData DescribeReceiptRuleSetResponse
