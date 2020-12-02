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
-- Module      : Network.AWS.AlexaBusiness.GetSkillGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets skill group details by skill group ARN.
--
--
module Network.AWS.AlexaBusiness.GetSkillGroup
    (
    -- * Creating a Request
      getSkillGroup
    , GetSkillGroup
    -- * Request Lenses
    , gsgSkillGroupARN

    -- * Destructuring the Response
    , getSkillGroupResponse
    , GetSkillGroupResponse
    -- * Response Lenses
    , gsgrsSkillGroup
    , gsgrsResponseStatus
    ) where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.AlexaBusiness.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getSkillGroup' smart constructor.
newtype GetSkillGroup = GetSkillGroup'
  { _gsgSkillGroupARN :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetSkillGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsgSkillGroupARN' - The ARN of the skill group for which to get details. Required.
getSkillGroup
    :: GetSkillGroup
getSkillGroup = GetSkillGroup' {_gsgSkillGroupARN = Nothing}


-- | The ARN of the skill group for which to get details. Required.
gsgSkillGroupARN :: Lens' GetSkillGroup (Maybe Text)
gsgSkillGroupARN = lens _gsgSkillGroupARN (\ s a -> s{_gsgSkillGroupARN = a})

instance AWSRequest GetSkillGroup where
        type Rs GetSkillGroup = GetSkillGroupResponse
        request = postJSON alexaBusiness
        response
          = receiveJSON
              (\ s h x ->
                 GetSkillGroupResponse' <$>
                   (x .?> "SkillGroup") <*> (pure (fromEnum s)))

instance Hashable GetSkillGroup where

instance NFData GetSkillGroup where

instance ToHeaders GetSkillGroup where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AlexaForBusiness.GetSkillGroup" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetSkillGroup where
        toJSON GetSkillGroup'{..}
          = object
              (catMaybes
                 [("SkillGroupArn" .=) <$> _gsgSkillGroupARN])

instance ToPath GetSkillGroup where
        toPath = const "/"

instance ToQuery GetSkillGroup where
        toQuery = const mempty

-- | /See:/ 'getSkillGroupResponse' smart constructor.
data GetSkillGroupResponse = GetSkillGroupResponse'
  { _gsgrsSkillGroup     :: !(Maybe SkillGroup)
  , _gsgrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetSkillGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsgrsSkillGroup' - The details of the skill group requested. Required.
--
-- * 'gsgrsResponseStatus' - -- | The response status code.
getSkillGroupResponse
    :: Int -- ^ 'gsgrsResponseStatus'
    -> GetSkillGroupResponse
getSkillGroupResponse pResponseStatus_ =
  GetSkillGroupResponse'
    {_gsgrsSkillGroup = Nothing, _gsgrsResponseStatus = pResponseStatus_}


-- | The details of the skill group requested. Required.
gsgrsSkillGroup :: Lens' GetSkillGroupResponse (Maybe SkillGroup)
gsgrsSkillGroup = lens _gsgrsSkillGroup (\ s a -> s{_gsgrsSkillGroup = a})

-- | -- | The response status code.
gsgrsResponseStatus :: Lens' GetSkillGroupResponse Int
gsgrsResponseStatus = lens _gsgrsResponseStatus (\ s a -> s{_gsgrsResponseStatus = a})

instance NFData GetSkillGroupResponse where
