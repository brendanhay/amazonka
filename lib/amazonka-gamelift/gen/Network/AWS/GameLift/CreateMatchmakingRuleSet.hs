{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.CreateMatchmakingRuleSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new rule set for FlexMatch matchmaking. A rule set describes the type of match to create, such as the number and size of teams. It also sets the parameters for acceptable player matches, such as minimum skill level or character type. A rule set is used by a 'MatchmakingConfiguration' .
--
--
-- To create a matchmaking rule set, provide unique rule set name and the rule set body in JSON format. Rule sets must be defined in the same Region as the matchmaking configuration they are used with.
--
-- Since matchmaking rule sets cannot be edited, it is a good idea to check the rule set syntax using 'ValidateMatchmakingRuleSet' before creating a new rule set.
--
-- __Learn more__
--
--     * <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-rulesets.html Build a Rule Set>
--
--     * <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-configuration.html Design a Matchmaker>
--
--     * <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-intro.html Matchmaking with FlexMatch>
--
--
--
-- __Related operations__
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
--     * 'DeleteMatchmakingRuleSet'
module Network.AWS.GameLift.CreateMatchmakingRuleSet
  ( -- * Creating a Request
    createMatchmakingRuleSet,
    CreateMatchmakingRuleSet,

    -- * Request Lenses
    cmrsTags,
    cmrsName,
    cmrsRuleSetBody,

    -- * Destructuring the Response
    createMatchmakingRuleSetResponse,
    CreateMatchmakingRuleSetResponse,

    -- * Response Lenses
    cmrsrsResponseStatus,
    cmrsrsRuleSet,
  )
where

import Network.AWS.GameLift.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input for a request operation.
--
--
--
-- /See:/ 'createMatchmakingRuleSet' smart constructor.
data CreateMatchmakingRuleSet = CreateMatchmakingRuleSet'
  { _cmrsTags ::
      !(Maybe [Tag]),
    _cmrsName :: !Text,
    _cmrsRuleSetBody :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateMatchmakingRuleSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmrsTags' - A list of labels to assign to the new matchmaking rule set resource. Tags are developer-defined key-value pairs. Tagging AWS resources are useful for resource management, access management and cost allocation. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in the /AWS General Reference/ . Once the resource is created, you can use 'TagResource' , 'UntagResource' , and 'ListTagsForResource' to add, remove, and view tags. The maximum tag limit may be lower than stated. See the AWS General Reference for actual tagging limits.
--
-- * 'cmrsName' - A unique identifier for a matchmaking rule set. A matchmaking configuration identifies the rule set it uses by this name value. Note that the rule set name is different from the optional @name@ field in the rule set body.
--
-- * 'cmrsRuleSetBody' - A collection of matchmaking rules, formatted as a JSON string. Comments are not allowed in JSON, but most elements support a description field.
createMatchmakingRuleSet ::
  -- | 'cmrsName'
  Text ->
  -- | 'cmrsRuleSetBody'
  Text ->
  CreateMatchmakingRuleSet
createMatchmakingRuleSet pName_ pRuleSetBody_ =
  CreateMatchmakingRuleSet'
    { _cmrsTags = Nothing,
      _cmrsName = pName_,
      _cmrsRuleSetBody = pRuleSetBody_
    }

-- | A list of labels to assign to the new matchmaking rule set resource. Tags are developer-defined key-value pairs. Tagging AWS resources are useful for resource management, access management and cost allocation. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in the /AWS General Reference/ . Once the resource is created, you can use 'TagResource' , 'UntagResource' , and 'ListTagsForResource' to add, remove, and view tags. The maximum tag limit may be lower than stated. See the AWS General Reference for actual tagging limits.
cmrsTags :: Lens' CreateMatchmakingRuleSet [Tag]
cmrsTags = lens _cmrsTags (\s a -> s {_cmrsTags = a}) . _Default . _Coerce

-- | A unique identifier for a matchmaking rule set. A matchmaking configuration identifies the rule set it uses by this name value. Note that the rule set name is different from the optional @name@ field in the rule set body.
cmrsName :: Lens' CreateMatchmakingRuleSet Text
cmrsName = lens _cmrsName (\s a -> s {_cmrsName = a})

-- | A collection of matchmaking rules, formatted as a JSON string. Comments are not allowed in JSON, but most elements support a description field.
cmrsRuleSetBody :: Lens' CreateMatchmakingRuleSet Text
cmrsRuleSetBody = lens _cmrsRuleSetBody (\s a -> s {_cmrsRuleSetBody = a})

instance AWSRequest CreateMatchmakingRuleSet where
  type Rs CreateMatchmakingRuleSet = CreateMatchmakingRuleSetResponse
  request = postJSON gameLift
  response =
    receiveJSON
      ( \s h x ->
          CreateMatchmakingRuleSetResponse'
            <$> (pure (fromEnum s)) <*> (x .:> "RuleSet")
      )

instance Hashable CreateMatchmakingRuleSet

instance NFData CreateMatchmakingRuleSet

instance ToHeaders CreateMatchmakingRuleSet where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("GameLift.CreateMatchmakingRuleSet" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateMatchmakingRuleSet where
  toJSON CreateMatchmakingRuleSet' {..} =
    object
      ( catMaybes
          [ ("Tags" .=) <$> _cmrsTags,
            Just ("Name" .= _cmrsName),
            Just ("RuleSetBody" .= _cmrsRuleSetBody)
          ]
      )

instance ToPath CreateMatchmakingRuleSet where
  toPath = const "/"

instance ToQuery CreateMatchmakingRuleSet where
  toQuery = const mempty

-- | Represents the returned data in response to a request operation.
--
--
--
-- /See:/ 'createMatchmakingRuleSetResponse' smart constructor.
data CreateMatchmakingRuleSetResponse = CreateMatchmakingRuleSetResponse'
  { _cmrsrsResponseStatus ::
      !Int,
    _cmrsrsRuleSet ::
      !MatchmakingRuleSet
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateMatchmakingRuleSetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmrsrsResponseStatus' - -- | The response status code.
--
-- * 'cmrsrsRuleSet' - The newly created matchmaking rule set.
createMatchmakingRuleSetResponse ::
  -- | 'cmrsrsResponseStatus'
  Int ->
  -- | 'cmrsrsRuleSet'
  MatchmakingRuleSet ->
  CreateMatchmakingRuleSetResponse
createMatchmakingRuleSetResponse pResponseStatus_ pRuleSet_ =
  CreateMatchmakingRuleSetResponse'
    { _cmrsrsResponseStatus =
        pResponseStatus_,
      _cmrsrsRuleSet = pRuleSet_
    }

-- | -- | The response status code.
cmrsrsResponseStatus :: Lens' CreateMatchmakingRuleSetResponse Int
cmrsrsResponseStatus = lens _cmrsrsResponseStatus (\s a -> s {_cmrsrsResponseStatus = a})

-- | The newly created matchmaking rule set.
cmrsrsRuleSet :: Lens' CreateMatchmakingRuleSetResponse MatchmakingRuleSet
cmrsrsRuleSet = lens _cmrsrsRuleSet (\s a -> s {_cmrsrsRuleSet = a})

instance NFData CreateMatchmakingRuleSetResponse
