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
-- Module      : Network.AWS.AlexaBusiness.AssociateSkillWithSkillGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a skill with a skill group.
--
--
module Network.AWS.AlexaBusiness.AssociateSkillWithSkillGroup
    (
    -- * Creating a Request
      associateSkillWithSkillGroup
    , AssociateSkillWithSkillGroup
    -- * Request Lenses
    , aswsgSkillGroupARN
    , aswsgSkillId

    -- * Destructuring the Response
    , associateSkillWithSkillGroupResponse
    , AssociateSkillWithSkillGroupResponse
    -- * Response Lenses
    , aswsgrsResponseStatus
    ) where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.AlexaBusiness.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'associateSkillWithSkillGroup' smart constructor.
data AssociateSkillWithSkillGroup = AssociateSkillWithSkillGroup'
  { _aswsgSkillGroupARN :: !(Maybe Text)
  , _aswsgSkillId       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssociateSkillWithSkillGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aswsgSkillGroupARN' - The ARN of the skill group to associate the skill to. Required.
--
-- * 'aswsgSkillId' - The unique identifier of the skill.
associateSkillWithSkillGroup
    :: Text -- ^ 'aswsgSkillId'
    -> AssociateSkillWithSkillGroup
associateSkillWithSkillGroup pSkillId_ =
  AssociateSkillWithSkillGroup'
    {_aswsgSkillGroupARN = Nothing, _aswsgSkillId = pSkillId_}


-- | The ARN of the skill group to associate the skill to. Required.
aswsgSkillGroupARN :: Lens' AssociateSkillWithSkillGroup (Maybe Text)
aswsgSkillGroupARN = lens _aswsgSkillGroupARN (\ s a -> s{_aswsgSkillGroupARN = a})

-- | The unique identifier of the skill.
aswsgSkillId :: Lens' AssociateSkillWithSkillGroup Text
aswsgSkillId = lens _aswsgSkillId (\ s a -> s{_aswsgSkillId = a})

instance AWSRequest AssociateSkillWithSkillGroup
         where
        type Rs AssociateSkillWithSkillGroup =
             AssociateSkillWithSkillGroupResponse
        request = postJSON alexaBusiness
        response
          = receiveEmpty
              (\ s h x ->
                 AssociateSkillWithSkillGroupResponse' <$>
                   (pure (fromEnum s)))

instance Hashable AssociateSkillWithSkillGroup where

instance NFData AssociateSkillWithSkillGroup where

instance ToHeaders AssociateSkillWithSkillGroup where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AlexaForBusiness.AssociateSkillWithSkillGroup" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AssociateSkillWithSkillGroup where
        toJSON AssociateSkillWithSkillGroup'{..}
          = object
              (catMaybes
                 [("SkillGroupArn" .=) <$> _aswsgSkillGroupARN,
                  Just ("SkillId" .= _aswsgSkillId)])

instance ToPath AssociateSkillWithSkillGroup where
        toPath = const "/"

instance ToQuery AssociateSkillWithSkillGroup where
        toQuery = const mempty

-- | /See:/ 'associateSkillWithSkillGroupResponse' smart constructor.
newtype AssociateSkillWithSkillGroupResponse = AssociateSkillWithSkillGroupResponse'
  { _aswsgrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssociateSkillWithSkillGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aswsgrsResponseStatus' - -- | The response status code.
associateSkillWithSkillGroupResponse
    :: Int -- ^ 'aswsgrsResponseStatus'
    -> AssociateSkillWithSkillGroupResponse
associateSkillWithSkillGroupResponse pResponseStatus_ =
  AssociateSkillWithSkillGroupResponse'
    {_aswsgrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
aswsgrsResponseStatus :: Lens' AssociateSkillWithSkillGroupResponse Int
aswsgrsResponseStatus = lens _aswsgrsResponseStatus (\ s a -> s{_aswsgrsResponseStatus = a})

instance NFData AssociateSkillWithSkillGroupResponse
         where
