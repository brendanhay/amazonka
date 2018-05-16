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
-- Module      : Network.AWS.AlexaBusiness.DeleteSkillGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a skill group by skill group ARN.
--
--
module Network.AWS.AlexaBusiness.DeleteSkillGroup
    (
    -- * Creating a Request
      deleteSkillGroup
    , DeleteSkillGroup
    -- * Request Lenses
    , dsgSkillGroupARN

    -- * Destructuring the Response
    , deleteSkillGroupResponse
    , DeleteSkillGroupResponse
    -- * Response Lenses
    , dsgrsResponseStatus
    ) where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.AlexaBusiness.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteSkillGroup' smart constructor.
newtype DeleteSkillGroup = DeleteSkillGroup'
  { _dsgSkillGroupARN :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteSkillGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsgSkillGroupARN' - The ARN of the skill group to delete. Required.
deleteSkillGroup
    :: DeleteSkillGroup
deleteSkillGroup = DeleteSkillGroup' {_dsgSkillGroupARN = Nothing}


-- | The ARN of the skill group to delete. Required.
dsgSkillGroupARN :: Lens' DeleteSkillGroup (Maybe Text)
dsgSkillGroupARN = lens _dsgSkillGroupARN (\ s a -> s{_dsgSkillGroupARN = a})

instance AWSRequest DeleteSkillGroup where
        type Rs DeleteSkillGroup = DeleteSkillGroupResponse
        request = postJSON alexaBusiness
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteSkillGroupResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteSkillGroup where

instance NFData DeleteSkillGroup where

instance ToHeaders DeleteSkillGroup where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AlexaForBusiness.DeleteSkillGroup" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteSkillGroup where
        toJSON DeleteSkillGroup'{..}
          = object
              (catMaybes
                 [("SkillGroupArn" .=) <$> _dsgSkillGroupARN])

instance ToPath DeleteSkillGroup where
        toPath = const "/"

instance ToQuery DeleteSkillGroup where
        toQuery = const mempty

-- | /See:/ 'deleteSkillGroupResponse' smart constructor.
newtype DeleteSkillGroupResponse = DeleteSkillGroupResponse'
  { _dsgrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteSkillGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsgrsResponseStatus' - -- | The response status code.
deleteSkillGroupResponse
    :: Int -- ^ 'dsgrsResponseStatus'
    -> DeleteSkillGroupResponse
deleteSkillGroupResponse pResponseStatus_ =
  DeleteSkillGroupResponse' {_dsgrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dsgrsResponseStatus :: Lens' DeleteSkillGroupResponse Int
dsgrsResponseStatus = lens _dsgrsResponseStatus (\ s a -> s{_dsgrsResponseStatus = a})

instance NFData DeleteSkillGroupResponse where
