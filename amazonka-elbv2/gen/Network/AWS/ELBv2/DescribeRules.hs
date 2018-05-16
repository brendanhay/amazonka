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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
    , drMarker
    , drRuleARNs
    , drPageSize

    -- * Destructuring the Response
    , describeRulesResponse
    , DescribeRulesResponse
    -- * Response Lenses
    , drsrsRules
    , drsrsNextMarker
    , drsrsResponseStatus
    ) where

import Network.AWS.ELBv2.Types
import Network.AWS.ELBv2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeRules' smart constructor.
data DescribeRules = DescribeRules'
  { _drListenerARN :: !(Maybe Text)
  , _drMarker      :: !(Maybe Text)
  , _drRuleARNs    :: !(Maybe [Text])
  , _drPageSize    :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeRules' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drListenerARN' - The Amazon Resource Name (ARN) of the listener.
--
-- * 'drMarker' - The marker for the next set of results. (You received this marker from a previous call.)
--
-- * 'drRuleARNs' - The Amazon Resource Names (ARN) of the rules.
--
-- * 'drPageSize' - The maximum number of results to return with this call.
describeRules
    :: DescribeRules
describeRules =
  DescribeRules'
    { _drListenerARN = Nothing
    , _drMarker = Nothing
    , _drRuleARNs = Nothing
    , _drPageSize = Nothing
    }


-- | The Amazon Resource Name (ARN) of the listener.
drListenerARN :: Lens' DescribeRules (Maybe Text)
drListenerARN = lens _drListenerARN (\ s a -> s{_drListenerARN = a})

-- | The marker for the next set of results. (You received this marker from a previous call.)
drMarker :: Lens' DescribeRules (Maybe Text)
drMarker = lens _drMarker (\ s a -> s{_drMarker = a})

-- | The Amazon Resource Names (ARN) of the rules.
drRuleARNs :: Lens' DescribeRules [Text]
drRuleARNs = lens _drRuleARNs (\ s a -> s{_drRuleARNs = a}) . _Default . _Coerce

-- | The maximum number of results to return with this call.
drPageSize :: Lens' DescribeRules (Maybe Natural)
drPageSize = lens _drPageSize (\ s a -> s{_drPageSize = a}) . mapping _Nat

instance AWSRequest DescribeRules where
        type Rs DescribeRules = DescribeRulesResponse
        request = postQuery eLBv2
        response
          = receiveXMLWrapper "DescribeRulesResult"
              (\ s h x ->
                 DescribeRulesResponse' <$>
                   (x .@? "Rules" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (x .@? "NextMarker")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeRules where

instance NFData DescribeRules where

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
               "Marker" =: _drMarker,
               "RuleArns" =:
                 toQuery (toQueryList "member" <$> _drRuleARNs),
               "PageSize" =: _drPageSize]

-- | /See:/ 'describeRulesResponse' smart constructor.
data DescribeRulesResponse = DescribeRulesResponse'
  { _drsrsRules          :: !(Maybe [Rule])
  , _drsrsNextMarker     :: !(Maybe Text)
  , _drsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeRulesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsrsRules' - Information about the rules.
--
-- * 'drsrsNextMarker' - The marker to use when requesting the next set of results. If there are no additional results, the string is empty.
--
-- * 'drsrsResponseStatus' - -- | The response status code.
describeRulesResponse
    :: Int -- ^ 'drsrsResponseStatus'
    -> DescribeRulesResponse
describeRulesResponse pResponseStatus_ =
  DescribeRulesResponse'
    { _drsrsRules = Nothing
    , _drsrsNextMarker = Nothing
    , _drsrsResponseStatus = pResponseStatus_
    }


-- | Information about the rules.
drsrsRules :: Lens' DescribeRulesResponse [Rule]
drsrsRules = lens _drsrsRules (\ s a -> s{_drsrsRules = a}) . _Default . _Coerce

-- | The marker to use when requesting the next set of results. If there are no additional results, the string is empty.
drsrsNextMarker :: Lens' DescribeRulesResponse (Maybe Text)
drsrsNextMarker = lens _drsrsNextMarker (\ s a -> s{_drsrsNextMarker = a})

-- | -- | The response status code.
drsrsResponseStatus :: Lens' DescribeRulesResponse Int
drsrsResponseStatus = lens _drsrsResponseStatus (\ s a -> s{_drsrsResponseStatus = a})

instance NFData DescribeRulesResponse where
