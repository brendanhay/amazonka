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
-- Module      : Network.AWS.GameLift.DeleteMatchmakingRuleSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing matchmaking rule set. To delete the rule set, provide the rule set name. Rule sets cannot be deleted if they are currently being used by a matchmaking configuration.
--
--
-- __Learn more__
--
--     * <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-rulesets.html Build a Rule Set>
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
module Network.AWS.GameLift.DeleteMatchmakingRuleSet
  ( -- * Creating a Request
    deleteMatchmakingRuleSet,
    DeleteMatchmakingRuleSet,

    -- * Request Lenses
    dmrsName,

    -- * Destructuring the Response
    deleteMatchmakingRuleSetResponse,
    DeleteMatchmakingRuleSetResponse,

    -- * Response Lenses
    dmrsrsResponseStatus,
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
-- /See:/ 'deleteMatchmakingRuleSet' smart constructor.
newtype DeleteMatchmakingRuleSet = DeleteMatchmakingRuleSet'
  { _dmrsName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteMatchmakingRuleSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmrsName' - A unique identifier for a matchmaking rule set to be deleted. (Note: The rule set name is different from the optional "name" field in the rule set body.) You can use either the rule set name or ARN value.
deleteMatchmakingRuleSet ::
  -- | 'dmrsName'
  Text ->
  DeleteMatchmakingRuleSet
deleteMatchmakingRuleSet pName_ =
  DeleteMatchmakingRuleSet' {_dmrsName = pName_}

-- | A unique identifier for a matchmaking rule set to be deleted. (Note: The rule set name is different from the optional "name" field in the rule set body.) You can use either the rule set name or ARN value.
dmrsName :: Lens' DeleteMatchmakingRuleSet Text
dmrsName = lens _dmrsName (\s a -> s {_dmrsName = a})

instance AWSRequest DeleteMatchmakingRuleSet where
  type Rs DeleteMatchmakingRuleSet = DeleteMatchmakingRuleSetResponse
  request = postJSON gameLift
  response =
    receiveEmpty
      ( \s h x ->
          DeleteMatchmakingRuleSetResponse' <$> (pure (fromEnum s))
      )

instance Hashable DeleteMatchmakingRuleSet

instance NFData DeleteMatchmakingRuleSet

instance ToHeaders DeleteMatchmakingRuleSet where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("GameLift.DeleteMatchmakingRuleSet" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteMatchmakingRuleSet where
  toJSON DeleteMatchmakingRuleSet' {..} =
    object (catMaybes [Just ("Name" .= _dmrsName)])

instance ToPath DeleteMatchmakingRuleSet where
  toPath = const "/"

instance ToQuery DeleteMatchmakingRuleSet where
  toQuery = const mempty

-- | Represents the returned data in response to a request operation.
--
--
--
-- /See:/ 'deleteMatchmakingRuleSetResponse' smart constructor.
newtype DeleteMatchmakingRuleSetResponse = DeleteMatchmakingRuleSetResponse'
  { _dmrsrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteMatchmakingRuleSetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmrsrsResponseStatus' - -- | The response status code.
deleteMatchmakingRuleSetResponse ::
  -- | 'dmrsrsResponseStatus'
  Int ->
  DeleteMatchmakingRuleSetResponse
deleteMatchmakingRuleSetResponse pResponseStatus_ =
  DeleteMatchmakingRuleSetResponse'
    { _dmrsrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
dmrsrsResponseStatus :: Lens' DeleteMatchmakingRuleSetResponse Int
dmrsrsResponseStatus = lens _dmrsrsResponseStatus (\s a -> s {_dmrsrsResponseStatus = a})

instance NFData DeleteMatchmakingRuleSetResponse
