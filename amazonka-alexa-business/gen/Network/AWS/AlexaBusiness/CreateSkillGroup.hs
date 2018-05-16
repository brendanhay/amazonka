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
-- Module      : Network.AWS.AlexaBusiness.CreateSkillGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a skill group with a specified name and description.
--
--
module Network.AWS.AlexaBusiness.CreateSkillGroup
    (
    -- * Creating a Request
      createSkillGroup
    , CreateSkillGroup
    -- * Request Lenses
    , csgClientRequestToken
    , csgDescription
    , csgSkillGroupName

    -- * Destructuring the Response
    , createSkillGroupResponse
    , CreateSkillGroupResponse
    -- * Response Lenses
    , csgrsSkillGroupARN
    , csgrsResponseStatus
    ) where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.AlexaBusiness.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createSkillGroup' smart constructor.
data CreateSkillGroup = CreateSkillGroup'
  { _csgClientRequestToken :: !(Maybe Text)
  , _csgDescription        :: !(Maybe Text)
  , _csgSkillGroupName     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateSkillGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csgClientRequestToken' - A unique, user-specified identifier for this request that ensures idempotency.
--
-- * 'csgDescription' - The description for the skill group.
--
-- * 'csgSkillGroupName' - The name for the skill group.
createSkillGroup
    :: Text -- ^ 'csgSkillGroupName'
    -> CreateSkillGroup
createSkillGroup pSkillGroupName_ =
  CreateSkillGroup'
    { _csgClientRequestToken = Nothing
    , _csgDescription = Nothing
    , _csgSkillGroupName = pSkillGroupName_
    }


-- | A unique, user-specified identifier for this request that ensures idempotency.
csgClientRequestToken :: Lens' CreateSkillGroup (Maybe Text)
csgClientRequestToken = lens _csgClientRequestToken (\ s a -> s{_csgClientRequestToken = a})

-- | The description for the skill group.
csgDescription :: Lens' CreateSkillGroup (Maybe Text)
csgDescription = lens _csgDescription (\ s a -> s{_csgDescription = a})

-- | The name for the skill group.
csgSkillGroupName :: Lens' CreateSkillGroup Text
csgSkillGroupName = lens _csgSkillGroupName (\ s a -> s{_csgSkillGroupName = a})

instance AWSRequest CreateSkillGroup where
        type Rs CreateSkillGroup = CreateSkillGroupResponse
        request = postJSON alexaBusiness
        response
          = receiveJSON
              (\ s h x ->
                 CreateSkillGroupResponse' <$>
                   (x .?> "SkillGroupArn") <*> (pure (fromEnum s)))

instance Hashable CreateSkillGroup where

instance NFData CreateSkillGroup where

instance ToHeaders CreateSkillGroup where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AlexaForBusiness.CreateSkillGroup" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateSkillGroup where
        toJSON CreateSkillGroup'{..}
          = object
              (catMaybes
                 [("ClientRequestToken" .=) <$>
                    _csgClientRequestToken,
                  ("Description" .=) <$> _csgDescription,
                  Just ("SkillGroupName" .= _csgSkillGroupName)])

instance ToPath CreateSkillGroup where
        toPath = const "/"

instance ToQuery CreateSkillGroup where
        toQuery = const mempty

-- | /See:/ 'createSkillGroupResponse' smart constructor.
data CreateSkillGroupResponse = CreateSkillGroupResponse'
  { _csgrsSkillGroupARN  :: !(Maybe Text)
  , _csgrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateSkillGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csgrsSkillGroupARN' - The ARN of the newly created skill group in the response.
--
-- * 'csgrsResponseStatus' - -- | The response status code.
createSkillGroupResponse
    :: Int -- ^ 'csgrsResponseStatus'
    -> CreateSkillGroupResponse
createSkillGroupResponse pResponseStatus_ =
  CreateSkillGroupResponse'
    {_csgrsSkillGroupARN = Nothing, _csgrsResponseStatus = pResponseStatus_}


-- | The ARN of the newly created skill group in the response.
csgrsSkillGroupARN :: Lens' CreateSkillGroupResponse (Maybe Text)
csgrsSkillGroupARN = lens _csgrsSkillGroupARN (\ s a -> s{_csgrsSkillGroupARN = a})

-- | -- | The response status code.
csgrsResponseStatus :: Lens' CreateSkillGroupResponse Int
csgrsResponseStatus = lens _csgrsResponseStatus (\ s a -> s{_csgrsResponseStatus = a})

instance NFData CreateSkillGroupResponse where
