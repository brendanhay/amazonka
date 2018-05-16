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
-- Module      : Network.AWS.AlexaBusiness.UpdateSkillGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates skill group details by skill group ARN.
--
--
module Network.AWS.AlexaBusiness.UpdateSkillGroup
    (
    -- * Creating a Request
      updateSkillGroup
    , UpdateSkillGroup
    -- * Request Lenses
    , usgSkillGroupARN
    , usgDescription
    , usgSkillGroupName

    -- * Destructuring the Response
    , updateSkillGroupResponse
    , UpdateSkillGroupResponse
    -- * Response Lenses
    , usgrsResponseStatus
    ) where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.AlexaBusiness.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateSkillGroup' smart constructor.
data UpdateSkillGroup = UpdateSkillGroup'
  { _usgSkillGroupARN  :: !(Maybe Text)
  , _usgDescription    :: !(Maybe Text)
  , _usgSkillGroupName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateSkillGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usgSkillGroupARN' - The ARN of the skill group to update.
--
-- * 'usgDescription' - The updated description for the skill group.
--
-- * 'usgSkillGroupName' - The updated name for the skill group.
updateSkillGroup
    :: UpdateSkillGroup
updateSkillGroup =
  UpdateSkillGroup'
    { _usgSkillGroupARN = Nothing
    , _usgDescription = Nothing
    , _usgSkillGroupName = Nothing
    }


-- | The ARN of the skill group to update.
usgSkillGroupARN :: Lens' UpdateSkillGroup (Maybe Text)
usgSkillGroupARN = lens _usgSkillGroupARN (\ s a -> s{_usgSkillGroupARN = a})

-- | The updated description for the skill group.
usgDescription :: Lens' UpdateSkillGroup (Maybe Text)
usgDescription = lens _usgDescription (\ s a -> s{_usgDescription = a})

-- | The updated name for the skill group.
usgSkillGroupName :: Lens' UpdateSkillGroup (Maybe Text)
usgSkillGroupName = lens _usgSkillGroupName (\ s a -> s{_usgSkillGroupName = a})

instance AWSRequest UpdateSkillGroup where
        type Rs UpdateSkillGroup = UpdateSkillGroupResponse
        request = postJSON alexaBusiness
        response
          = receiveEmpty
              (\ s h x ->
                 UpdateSkillGroupResponse' <$> (pure (fromEnum s)))

instance Hashable UpdateSkillGroup where

instance NFData UpdateSkillGroup where

instance ToHeaders UpdateSkillGroup where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AlexaForBusiness.UpdateSkillGroup" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateSkillGroup where
        toJSON UpdateSkillGroup'{..}
          = object
              (catMaybes
                 [("SkillGroupArn" .=) <$> _usgSkillGroupARN,
                  ("Description" .=) <$> _usgDescription,
                  ("SkillGroupName" .=) <$> _usgSkillGroupName])

instance ToPath UpdateSkillGroup where
        toPath = const "/"

instance ToQuery UpdateSkillGroup where
        toQuery = const mempty

-- | /See:/ 'updateSkillGroupResponse' smart constructor.
newtype UpdateSkillGroupResponse = UpdateSkillGroupResponse'
  { _usgrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateSkillGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usgrsResponseStatus' - -- | The response status code.
updateSkillGroupResponse
    :: Int -- ^ 'usgrsResponseStatus'
    -> UpdateSkillGroupResponse
updateSkillGroupResponse pResponseStatus_ =
  UpdateSkillGroupResponse' {_usgrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
usgrsResponseStatus :: Lens' UpdateSkillGroupResponse Int
usgrsResponseStatus = lens _usgrsResponseStatus (\ s a -> s{_usgrsResponseStatus = a})

instance NFData UpdateSkillGroupResponse where
