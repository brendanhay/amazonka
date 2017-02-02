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
-- Module      : Network.AWS.ELBv2.DescribeRules
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified rules or the rules for the specified listener. You must specify either a listener or one or more rules.
--
--
module Network.AWS.ELBv2.DescribeRules
    (
    -- * Creating a Request
      describeRules
    , DescribeRules
    -- * Request Lenses
    , drListenerARN
    , drRuleARNs

    -- * Destructuring the Response
    , describeRulesResponse
    , DescribeRulesResponse
    -- * Response Lenses
    , drsrsRules
    , drsrsResponseStatus
    ) where

import           Network.AWS.ELBv2.Types
import           Network.AWS.ELBv2.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeRules' smart constructor.
data DescribeRules = DescribeRules'
    { _drListenerARN :: !(Maybe Text)
    , _drRuleARNs    :: !(Maybe [Text])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeRules' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drListenerARN' - The Amazon Resource Name (ARN) of the listener.
--
-- * 'drRuleARNs' - The Amazon Resource Names (ARN) of the rules.
describeRules
    :: DescribeRules
describeRules =
    DescribeRules'
    { _drListenerARN = Nothing
    , _drRuleARNs = Nothing
    }

-- | The Amazon Resource Name (ARN) of the listener.
drListenerARN :: Lens' DescribeRules (Maybe Text)
drListenerARN = lens _drListenerARN (\ s a -> s{_drListenerARN = a});

-- | The Amazon Resource Names (ARN) of the rules.
drRuleARNs :: Lens' DescribeRules [Text]
drRuleARNs = lens _drRuleARNs (\ s a -> s{_drRuleARNs = a}) . _Default . _Coerce;

instance AWSRequest DescribeRules where
        type Rs DescribeRules = DescribeRulesResponse
        request = postQuery eLBv2
        response
          = receiveXMLWrapper "DescribeRulesResult"
              (\ s h x ->
                 DescribeRulesResponse' <$>
                   (x .@? "Rules" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance Hashable DescribeRules

instance NFData DescribeRules

instance ToHeaders DescribeRules where
        toHeaders = const mempty

instance ToPath DescribeRules where
        toPath = const "/"

instance ToQuery DescribeRules where
        toQuery DescribeRules'{..}
          = mconcat
              ["Action" =: ("DescribeRules" :: ByteString),
               "Version" =: ("2015-12-01" :: ByteString),
               "ListenerArn" =: _drListenerARN,
               "RuleArns" =:
                 toQuery (toQueryList "member" <$> _drRuleARNs)]

-- | /See:/ 'describeRulesResponse' smart constructor.
data DescribeRulesResponse = DescribeRulesResponse'
    { _drsrsRules          :: !(Maybe [Rule])
    , _drsrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeRulesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsrsRules' - Information about the rules.
--
-- * 'drsrsResponseStatus' - -- | The response status code.
describeRulesResponse
    :: Int -- ^ 'drsrsResponseStatus'
    -> DescribeRulesResponse
describeRulesResponse pResponseStatus_ =
    DescribeRulesResponse'
    { _drsrsRules = Nothing
    , _drsrsResponseStatus = pResponseStatus_
    }

-- | Information about the rules.
drsrsRules :: Lens' DescribeRulesResponse [Rule]
drsrsRules = lens _drsrsRules (\ s a -> s{_drsrsRules = a}) . _Default . _Coerce;

-- | -- | The response status code.
drsrsResponseStatus :: Lens' DescribeRulesResponse Int
drsrsResponseStatus = lens _drsrsResponseStatus (\ s a -> s{_drsrsResponseStatus = a});

instance NFData DescribeRulesResponse
