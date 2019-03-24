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
-- Module      : Network.AWS.SageMaker.CreateWorkteam
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new work team for labeling your data. A work team is defined by one or more Amazon Cognito user pools. You must first create the user pools before you can create a work team.
--
--
-- You cannot create more than 25 work teams in an account and region.
--
module Network.AWS.SageMaker.CreateWorkteam
    (
    -- * Creating a Request
      createWorkteam
    , CreateWorkteam
    -- * Request Lenses
    , cwTags
    , cwWorkteamName
    , cwMemberDefinitions
    , cwDescription

    -- * Destructuring the Response
    , createWorkteamResponse
    , CreateWorkteamResponse
    -- * Response Lenses
    , cwrsWorkteamARN
    , cwrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types
import Network.AWS.SageMaker.Types.Product

-- | /See:/ 'createWorkteam' smart constructor.
data CreateWorkteam = CreateWorkteam'
  { _cwTags              :: !(Maybe [Tag])
  , _cwWorkteamName      :: !Text
  , _cwMemberDefinitions :: !(List1 MemberDefinition)
  , _cwDescription       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateWorkteam' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cwTags' -
--
-- * 'cwWorkteamName' - The name of the work team. Use this name to identify the work team.
--
-- * 'cwMemberDefinitions' - A list of @MemberDefinition@ objects that contains objects that identify the Amazon Cognito user pool that makes up the work team. For more information, see <http://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools.html Amazon Cognito User Pools> . All of the @CognitoMemberDefinition@ objects that make up the member definition must have the same @ClientId@ and @UserPool@ values.
--
-- * 'cwDescription' - A description of the work team.
createWorkteam
    :: Text -- ^ 'cwWorkteamName'
    -> NonEmpty MemberDefinition -- ^ 'cwMemberDefinitions'
    -> Text -- ^ 'cwDescription'
    -> CreateWorkteam
createWorkteam pWorkteamName_ pMemberDefinitions_ pDescription_ =
  CreateWorkteam'
    { _cwTags = Nothing
    , _cwWorkteamName = pWorkteamName_
    , _cwMemberDefinitions = _List1 # pMemberDefinitions_
    , _cwDescription = pDescription_
    }


-- |
cwTags :: Lens' CreateWorkteam [Tag]
cwTags = lens _cwTags (\ s a -> s{_cwTags = a}) . _Default . _Coerce

-- | The name of the work team. Use this name to identify the work team.
cwWorkteamName :: Lens' CreateWorkteam Text
cwWorkteamName = lens _cwWorkteamName (\ s a -> s{_cwWorkteamName = a})

-- | A list of @MemberDefinition@ objects that contains objects that identify the Amazon Cognito user pool that makes up the work team. For more information, see <http://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools.html Amazon Cognito User Pools> . All of the @CognitoMemberDefinition@ objects that make up the member definition must have the same @ClientId@ and @UserPool@ values.
cwMemberDefinitions :: Lens' CreateWorkteam (NonEmpty MemberDefinition)
cwMemberDefinitions = lens _cwMemberDefinitions (\ s a -> s{_cwMemberDefinitions = a}) . _List1

-- | A description of the work team.
cwDescription :: Lens' CreateWorkteam Text
cwDescription = lens _cwDescription (\ s a -> s{_cwDescription = a})

instance AWSRequest CreateWorkteam where
        type Rs CreateWorkteam = CreateWorkteamResponse
        request = postJSON sageMaker
        response
          = receiveJSON
              (\ s h x ->
                 CreateWorkteamResponse' <$>
                   (x .?> "WorkteamArn") <*> (pure (fromEnum s)))

instance Hashable CreateWorkteam where

instance NFData CreateWorkteam where

instance ToHeaders CreateWorkteam where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SageMaker.CreateWorkteam" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateWorkteam where
        toJSON CreateWorkteam'{..}
          = object
              (catMaybes
                 [("Tags" .=) <$> _cwTags,
                  Just ("WorkteamName" .= _cwWorkteamName),
                  Just ("MemberDefinitions" .= _cwMemberDefinitions),
                  Just ("Description" .= _cwDescription)])

instance ToPath CreateWorkteam where
        toPath = const "/"

instance ToQuery CreateWorkteam where
        toQuery = const mempty

-- | /See:/ 'createWorkteamResponse' smart constructor.
data CreateWorkteamResponse = CreateWorkteamResponse'
  { _cwrsWorkteamARN    :: !(Maybe Text)
  , _cwrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateWorkteamResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cwrsWorkteamARN' - The Amazon Resource Name (ARN) of the work team. You can use this ARN to identify the work team.
--
-- * 'cwrsResponseStatus' - -- | The response status code.
createWorkteamResponse
    :: Int -- ^ 'cwrsResponseStatus'
    -> CreateWorkteamResponse
createWorkteamResponse pResponseStatus_ =
  CreateWorkteamResponse'
    {_cwrsWorkteamARN = Nothing, _cwrsResponseStatus = pResponseStatus_}


-- | The Amazon Resource Name (ARN) of the work team. You can use this ARN to identify the work team.
cwrsWorkteamARN :: Lens' CreateWorkteamResponse (Maybe Text)
cwrsWorkteamARN = lens _cwrsWorkteamARN (\ s a -> s{_cwrsWorkteamARN = a})

-- | -- | The response status code.
cwrsResponseStatus :: Lens' CreateWorkteamResponse Int
cwrsResponseStatus = lens _cwrsResponseStatus (\ s a -> s{_cwrsResponseStatus = a})

instance NFData CreateWorkteamResponse where
