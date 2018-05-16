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
-- Module      : Network.AWS.GameLift.CreateMatchmakingRuleSet
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new rule set for FlexMatch matchmaking. A rule set describes the type of match to create, such as the number and size of teams, and sets the parameters for acceptable player matches, such as minimum skill level or character type. Rule sets are used in matchmaking configurations, which define how matchmaking requests are handled. Each 'MatchmakingConfiguration' uses one rule set; you can set up multiple rule sets to handle the scenarios that suit your game (such as for different game modes), and create a separate matchmaking configuration for each rule set. See additional information on rule set content in the 'MatchmakingRuleSet' structure. For help creating rule sets, including useful examples, see the topic <http://docs.aws.amazon.com/gamelift/latest/developerguide/match-intro.html Adding FlexMatch to Your Game> .
--
--
-- Once created, matchmaking rule sets cannot be changed or deleted, so we recommend checking the rule set syntax using 'ValidateMatchmakingRuleSet' before creating the rule set.
--
-- To create a matchmaking rule set, provide the set of rules and a unique name. Rule sets must be defined in the same region as the matchmaking configuration they will be used with. Rule sets cannot be edited or deleted. If you need to change a rule set, create a new one with the necessary edits and then update matchmaking configurations to use the new rule set.
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
module Network.AWS.GameLift.CreateMatchmakingRuleSet
    (
    -- * Creating a Request
      createMatchmakingRuleSet
    , CreateMatchmakingRuleSet
    -- * Request Lenses
    , cmrsName
    , cmrsRuleSetBody

    -- * Destructuring the Response
    , createMatchmakingRuleSetResponse
    , CreateMatchmakingRuleSetResponse
    -- * Response Lenses
    , cmrsrsResponseStatus
    , cmrsrsRuleSet
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
-- /See:/ 'createMatchmakingRuleSet' smart constructor.
data CreateMatchmakingRuleSet = CreateMatchmakingRuleSet'
  { _cmrsName        :: !Text
  , _cmrsRuleSetBody :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateMatchmakingRuleSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmrsName' - Unique identifier for a matchmaking rule set. This name is used to identify the rule set associated with a matchmaking configuration.
--
-- * 'cmrsRuleSetBody' - Collection of matchmaking rules, formatted as a JSON string. (Note that comments are not allowed in JSON, but most elements support a description field.)
createMatchmakingRuleSet
    :: Text -- ^ 'cmrsName'
    -> Text -- ^ 'cmrsRuleSetBody'
    -> CreateMatchmakingRuleSet
createMatchmakingRuleSet pName_ pRuleSetBody_ =
  CreateMatchmakingRuleSet'
    {_cmrsName = pName_, _cmrsRuleSetBody = pRuleSetBody_}


-- | Unique identifier for a matchmaking rule set. This name is used to identify the rule set associated with a matchmaking configuration.
cmrsName :: Lens' CreateMatchmakingRuleSet Text
cmrsName = lens _cmrsName (\ s a -> s{_cmrsName = a})

-- | Collection of matchmaking rules, formatted as a JSON string. (Note that comments are not allowed in JSON, but most elements support a description field.)
cmrsRuleSetBody :: Lens' CreateMatchmakingRuleSet Text
cmrsRuleSetBody = lens _cmrsRuleSetBody (\ s a -> s{_cmrsRuleSetBody = a})

instance AWSRequest CreateMatchmakingRuleSet where
        type Rs CreateMatchmakingRuleSet =
             CreateMatchmakingRuleSetResponse
        request = postJSON gameLift
        response
          = receiveJSON
              (\ s h x ->
                 CreateMatchmakingRuleSetResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "RuleSet"))

instance Hashable CreateMatchmakingRuleSet where

instance NFData CreateMatchmakingRuleSet where

instance ToHeaders CreateMatchmakingRuleSet where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("GameLift.CreateMatchmakingRuleSet" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateMatchmakingRuleSet where
        toJSON CreateMatchmakingRuleSet'{..}
          = object
              (catMaybes
                 [Just ("Name" .= _cmrsName),
                  Just ("RuleSetBody" .= _cmrsRuleSetBody)])

instance ToPath CreateMatchmakingRuleSet where
        toPath = const "/"

instance ToQuery CreateMatchmakingRuleSet where
        toQuery = const mempty

-- | Represents the returned data in response to a request action.
--
--
--
-- /See:/ 'createMatchmakingRuleSetResponse' smart constructor.
data CreateMatchmakingRuleSetResponse = CreateMatchmakingRuleSetResponse'
  { _cmrsrsResponseStatus :: !Int
  , _cmrsrsRuleSet        :: !MatchmakingRuleSet
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateMatchmakingRuleSetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmrsrsResponseStatus' - -- | The response status code.
--
-- * 'cmrsrsRuleSet' - Object that describes the newly created matchmaking rule set.
createMatchmakingRuleSetResponse
    :: Int -- ^ 'cmrsrsResponseStatus'
    -> MatchmakingRuleSet -- ^ 'cmrsrsRuleSet'
    -> CreateMatchmakingRuleSetResponse
createMatchmakingRuleSetResponse pResponseStatus_ pRuleSet_ =
  CreateMatchmakingRuleSetResponse'
    {_cmrsrsResponseStatus = pResponseStatus_, _cmrsrsRuleSet = pRuleSet_}


-- | -- | The response status code.
cmrsrsResponseStatus :: Lens' CreateMatchmakingRuleSetResponse Int
cmrsrsResponseStatus = lens _cmrsrsResponseStatus (\ s a -> s{_cmrsrsResponseStatus = a})

-- | Object that describes the newly created matchmaking rule set.
cmrsrsRuleSet :: Lens' CreateMatchmakingRuleSetResponse MatchmakingRuleSet
cmrsrsRuleSet = lens _cmrsrsRuleSet (\ s a -> s{_cmrsrsRuleSet = a})

instance NFData CreateMatchmakingRuleSetResponse
         where
