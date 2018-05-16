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
-- Module      : Network.AWS.GameLift.ValidateMatchmakingRuleSet
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Validates the syntax of a matchmaking rule or rule set. This operation checks that the rule set uses syntactically correct JSON and that it conforms to allowed property expressions. To validate syntax, provide a rule set string.
--
--
-- Operations related to match configurations and rule sets include:
--
--     * 'CreateMatchmakingConfiguration'
--
--     * 'DescribeMatchmakingConfigurations'
--
--     * 'UpdateMatchmakingConfiguration'
--
--     * 'DeleteMatchmakingConfiguration'
--
--     * 'CreateMatchmakingRuleSet'
--
--     * 'DescribeMatchmakingRuleSets'
--
--     * 'ValidateMatchmakingRuleSet'
--
--
--
module Network.AWS.GameLift.ValidateMatchmakingRuleSet
    (
    -- * Creating a Request
      validateMatchmakingRuleSet
    , ValidateMatchmakingRuleSet
    -- * Request Lenses
    , vmrsRuleSetBody

    -- * Destructuring the Response
    , validateMatchmakingRuleSetResponse
    , ValidateMatchmakingRuleSetResponse
    -- * Response Lenses
    , vmrsrsValid
    , vmrsrsResponseStatus
    ) where

import Network.AWS.GameLift.Types
import Network.AWS.GameLift.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input for a request action.
--
--
--
-- /See:/ 'validateMatchmakingRuleSet' smart constructor.
newtype ValidateMatchmakingRuleSet = ValidateMatchmakingRuleSet'
  { _vmrsRuleSetBody :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ValidateMatchmakingRuleSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vmrsRuleSetBody' - Collection of matchmaking rules to validate, formatted as a JSON string.
validateMatchmakingRuleSet
    :: Text -- ^ 'vmrsRuleSetBody'
    -> ValidateMatchmakingRuleSet
validateMatchmakingRuleSet pRuleSetBody_ =
  ValidateMatchmakingRuleSet' {_vmrsRuleSetBody = pRuleSetBody_}


-- | Collection of matchmaking rules to validate, formatted as a JSON string.
vmrsRuleSetBody :: Lens' ValidateMatchmakingRuleSet Text
vmrsRuleSetBody = lens _vmrsRuleSetBody (\ s a -> s{_vmrsRuleSetBody = a})

instance AWSRequest ValidateMatchmakingRuleSet where
        type Rs ValidateMatchmakingRuleSet =
             ValidateMatchmakingRuleSetResponse
        request = postJSON gameLift
        response
          = receiveJSON
              (\ s h x ->
                 ValidateMatchmakingRuleSetResponse' <$>
                   (x .?> "Valid") <*> (pure (fromEnum s)))

instance Hashable ValidateMatchmakingRuleSet where

instance NFData ValidateMatchmakingRuleSet where

instance ToHeaders ValidateMatchmakingRuleSet where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("GameLift.ValidateMatchmakingRuleSet" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ValidateMatchmakingRuleSet where
        toJSON ValidateMatchmakingRuleSet'{..}
          = object
              (catMaybes
                 [Just ("RuleSetBody" .= _vmrsRuleSetBody)])

instance ToPath ValidateMatchmakingRuleSet where
        toPath = const "/"

instance ToQuery ValidateMatchmakingRuleSet where
        toQuery = const mempty

-- | Represents the returned data in response to a request action.
--
--
--
-- /See:/ 'validateMatchmakingRuleSetResponse' smart constructor.
data ValidateMatchmakingRuleSetResponse = ValidateMatchmakingRuleSetResponse'
  { _vmrsrsValid          :: !(Maybe Bool)
  , _vmrsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ValidateMatchmakingRuleSetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vmrsrsValid' - Response indicating whether or not the rule set is valid.
--
-- * 'vmrsrsResponseStatus' - -- | The response status code.
validateMatchmakingRuleSetResponse
    :: Int -- ^ 'vmrsrsResponseStatus'
    -> ValidateMatchmakingRuleSetResponse
validateMatchmakingRuleSetResponse pResponseStatus_ =
  ValidateMatchmakingRuleSetResponse'
    {_vmrsrsValid = Nothing, _vmrsrsResponseStatus = pResponseStatus_}


-- | Response indicating whether or not the rule set is valid.
vmrsrsValid :: Lens' ValidateMatchmakingRuleSetResponse (Maybe Bool)
vmrsrsValid = lens _vmrsrsValid (\ s a -> s{_vmrsrsValid = a})

-- | -- | The response status code.
vmrsrsResponseStatus :: Lens' ValidateMatchmakingRuleSetResponse Int
vmrsrsResponseStatus = lens _vmrsrsResponseStatus (\ s a -> s{_vmrsrsResponseStatus = a})

instance NFData ValidateMatchmakingRuleSetResponse
         where
