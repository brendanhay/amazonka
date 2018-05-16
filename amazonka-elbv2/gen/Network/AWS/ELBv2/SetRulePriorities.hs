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
-- Module      : Network.AWS.ELBv2.SetRulePriorities
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the priorities of the specified rules.
--
--
-- You can reorder the rules as long as there are no priority conflicts in the new order. Any existing rules that you do not specify retain their current priority.
--
module Network.AWS.ELBv2.SetRulePriorities
    (
    -- * Creating a Request
      setRulePriorities
    , SetRulePriorities
    -- * Request Lenses
    , srpRulePriorities

    -- * Destructuring the Response
    , setRulePrioritiesResponse
    , SetRulePrioritiesResponse
    -- * Response Lenses
    , srprsRules
    , srprsResponseStatus
    ) where

import Network.AWS.ELBv2.Types
import Network.AWS.ELBv2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'setRulePriorities' smart constructor.
newtype SetRulePriorities = SetRulePriorities'
  { _srpRulePriorities :: [RulePriorityPair]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetRulePriorities' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srpRulePriorities' - The rule priorities.
setRulePriorities
    :: SetRulePriorities
setRulePriorities = SetRulePriorities' {_srpRulePriorities = mempty}


-- | The rule priorities.
srpRulePriorities :: Lens' SetRulePriorities [RulePriorityPair]
srpRulePriorities = lens _srpRulePriorities (\ s a -> s{_srpRulePriorities = a}) . _Coerce

instance AWSRequest SetRulePriorities where
        type Rs SetRulePriorities = SetRulePrioritiesResponse
        request = postQuery eLBv2
        response
          = receiveXMLWrapper "SetRulePrioritiesResult"
              (\ s h x ->
                 SetRulePrioritiesResponse' <$>
                   (x .@? "Rules" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance Hashable SetRulePriorities where

instance NFData SetRulePriorities where

instance ToHeaders SetRulePriorities where
        toHeaders = const mempty

instance ToPath SetRulePriorities where
        toPath = const "/"

instance ToQuery SetRulePriorities where
        toQuery SetRulePriorities'{..}
          = mconcat
              ["Action" =: ("SetRulePriorities" :: ByteString),
               "Version" =: ("2015-12-01" :: ByteString),
               "RulePriorities" =:
                 toQueryList "member" _srpRulePriorities]

-- | /See:/ 'setRulePrioritiesResponse' smart constructor.
data SetRulePrioritiesResponse = SetRulePrioritiesResponse'
  { _srprsRules          :: !(Maybe [Rule])
  , _srprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetRulePrioritiesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srprsRules' - Information about the rules.
--
-- * 'srprsResponseStatus' - -- | The response status code.
setRulePrioritiesResponse
    :: Int -- ^ 'srprsResponseStatus'
    -> SetRulePrioritiesResponse
setRulePrioritiesResponse pResponseStatus_ =
  SetRulePrioritiesResponse'
    {_srprsRules = Nothing, _srprsResponseStatus = pResponseStatus_}


-- | Information about the rules.
srprsRules :: Lens' SetRulePrioritiesResponse [Rule]
srprsRules = lens _srprsRules (\ s a -> s{_srprsRules = a}) . _Default . _Coerce

-- | -- | The response status code.
srprsResponseStatus :: Lens' SetRulePrioritiesResponse Int
srprsResponseStatus = lens _srprsResponseStatus (\ s a -> s{_srprsResponseStatus = a})

instance NFData SetRulePrioritiesResponse where
