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
-- Module      : Network.AWS.WAFRegional.GetRateBasedRuleManagedKeys
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of IP addresses currently being blocked by the 'RateBasedRule' that is specified by the @RuleId@ . The maximum number of managed keys that will be blocked is 10,000. If more than 10,000 addresses exceed the rate limit, the 10,000 addresses with the highest rates will be blocked.
--
--
module Network.AWS.WAFRegional.GetRateBasedRuleManagedKeys
    (
    -- * Creating a Request
      getRateBasedRuleManagedKeys
    , GetRateBasedRuleManagedKeys
    -- * Request Lenses
    , grbrmkNextMarker
    , grbrmkRuleId

    -- * Destructuring the Response
    , getRateBasedRuleManagedKeysResponse
    , GetRateBasedRuleManagedKeysResponse
    -- * Response Lenses
    , grbrmkrsNextMarker
    , grbrmkrsManagedKeys
    , grbrmkrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WAFRegional.Types
import Network.AWS.WAFRegional.Types.Product

-- | /See:/ 'getRateBasedRuleManagedKeys' smart constructor.
data GetRateBasedRuleManagedKeys = GetRateBasedRuleManagedKeys'
  { _grbrmkNextMarker :: !(Maybe Text)
  , _grbrmkRuleId     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetRateBasedRuleManagedKeys' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grbrmkNextMarker' - A null value and not currently used. Do not include this in your request.
--
-- * 'grbrmkRuleId' - The @RuleId@ of the 'RateBasedRule' for which you want to get a list of @ManagedKeys@ . @RuleId@ is returned by 'CreateRateBasedRule' and by 'ListRateBasedRules' .
getRateBasedRuleManagedKeys
    :: Text -- ^ 'grbrmkRuleId'
    -> GetRateBasedRuleManagedKeys
getRateBasedRuleManagedKeys pRuleId_ =
  GetRateBasedRuleManagedKeys'
    {_grbrmkNextMarker = Nothing, _grbrmkRuleId = pRuleId_}


-- | A null value and not currently used. Do not include this in your request.
grbrmkNextMarker :: Lens' GetRateBasedRuleManagedKeys (Maybe Text)
grbrmkNextMarker = lens _grbrmkNextMarker (\ s a -> s{_grbrmkNextMarker = a})

-- | The @RuleId@ of the 'RateBasedRule' for which you want to get a list of @ManagedKeys@ . @RuleId@ is returned by 'CreateRateBasedRule' and by 'ListRateBasedRules' .
grbrmkRuleId :: Lens' GetRateBasedRuleManagedKeys Text
grbrmkRuleId = lens _grbrmkRuleId (\ s a -> s{_grbrmkRuleId = a})

instance AWSRequest GetRateBasedRuleManagedKeys where
        type Rs GetRateBasedRuleManagedKeys =
             GetRateBasedRuleManagedKeysResponse
        request = postJSON wAFRegional
        response
          = receiveJSON
              (\ s h x ->
                 GetRateBasedRuleManagedKeysResponse' <$>
                   (x .?> "NextMarker") <*>
                     (x .?> "ManagedKeys" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable GetRateBasedRuleManagedKeys where

instance NFData GetRateBasedRuleManagedKeys where

instance ToHeaders GetRateBasedRuleManagedKeys where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSWAF_Regional_20161128.GetRateBasedRuleManagedKeys"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetRateBasedRuleManagedKeys where
        toJSON GetRateBasedRuleManagedKeys'{..}
          = object
              (catMaybes
                 [("NextMarker" .=) <$> _grbrmkNextMarker,
                  Just ("RuleId" .= _grbrmkRuleId)])

instance ToPath GetRateBasedRuleManagedKeys where
        toPath = const "/"

instance ToQuery GetRateBasedRuleManagedKeys where
        toQuery = const mempty

-- | /See:/ 'getRateBasedRuleManagedKeysResponse' smart constructor.
data GetRateBasedRuleManagedKeysResponse = GetRateBasedRuleManagedKeysResponse'
  { _grbrmkrsNextMarker     :: !(Maybe Text)
  , _grbrmkrsManagedKeys    :: !(Maybe [Text])
  , _grbrmkrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetRateBasedRuleManagedKeysResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grbrmkrsNextMarker' - A null value and not currently used.
--
-- * 'grbrmkrsManagedKeys' - An array of IP addresses that currently are blocked by the specified 'RateBasedRule' .
--
-- * 'grbrmkrsResponseStatus' - -- | The response status code.
getRateBasedRuleManagedKeysResponse
    :: Int -- ^ 'grbrmkrsResponseStatus'
    -> GetRateBasedRuleManagedKeysResponse
getRateBasedRuleManagedKeysResponse pResponseStatus_ =
  GetRateBasedRuleManagedKeysResponse'
    { _grbrmkrsNextMarker = Nothing
    , _grbrmkrsManagedKeys = Nothing
    , _grbrmkrsResponseStatus = pResponseStatus_
    }


-- | A null value and not currently used.
grbrmkrsNextMarker :: Lens' GetRateBasedRuleManagedKeysResponse (Maybe Text)
grbrmkrsNextMarker = lens _grbrmkrsNextMarker (\ s a -> s{_grbrmkrsNextMarker = a})

-- | An array of IP addresses that currently are blocked by the specified 'RateBasedRule' .
grbrmkrsManagedKeys :: Lens' GetRateBasedRuleManagedKeysResponse [Text]
grbrmkrsManagedKeys = lens _grbrmkrsManagedKeys (\ s a -> s{_grbrmkrsManagedKeys = a}) . _Default . _Coerce

-- | -- | The response status code.
grbrmkrsResponseStatus :: Lens' GetRateBasedRuleManagedKeysResponse Int
grbrmkrsResponseStatus = lens _grbrmkrsResponseStatus (\ s a -> s{_grbrmkrsResponseStatus = a})

instance NFData GetRateBasedRuleManagedKeysResponse
         where
