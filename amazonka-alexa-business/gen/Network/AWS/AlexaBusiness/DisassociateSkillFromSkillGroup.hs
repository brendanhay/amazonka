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
-- Module      : Network.AWS.AlexaBusiness.DisassociateSkillFromSkillGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a skill from a skill group.
--
--
module Network.AWS.AlexaBusiness.DisassociateSkillFromSkillGroup
    (
    -- * Creating a Request
      disassociateSkillFromSkillGroup
    , DisassociateSkillFromSkillGroup
    -- * Request Lenses
    , dsfsgSkillGroupARN
    , dsfsgSkillId

    -- * Destructuring the Response
    , disassociateSkillFromSkillGroupResponse
    , DisassociateSkillFromSkillGroupResponse
    -- * Response Lenses
    , dsfsgrsResponseStatus
    ) where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.AlexaBusiness.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'disassociateSkillFromSkillGroup' smart constructor.
data DisassociateSkillFromSkillGroup = DisassociateSkillFromSkillGroup'
  { _dsfsgSkillGroupARN :: !(Maybe Text)
  , _dsfsgSkillId       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisassociateSkillFromSkillGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsfsgSkillGroupARN' - The unique identifier of a skill. Required.
--
-- * 'dsfsgSkillId' - The ARN of a skill group to associate to a skill.
disassociateSkillFromSkillGroup
    :: Text -- ^ 'dsfsgSkillId'
    -> DisassociateSkillFromSkillGroup
disassociateSkillFromSkillGroup pSkillId_ =
  DisassociateSkillFromSkillGroup'
    {_dsfsgSkillGroupARN = Nothing, _dsfsgSkillId = pSkillId_}


-- | The unique identifier of a skill. Required.
dsfsgSkillGroupARN :: Lens' DisassociateSkillFromSkillGroup (Maybe Text)
dsfsgSkillGroupARN = lens _dsfsgSkillGroupARN (\ s a -> s{_dsfsgSkillGroupARN = a})

-- | The ARN of a skill group to associate to a skill.
dsfsgSkillId :: Lens' DisassociateSkillFromSkillGroup Text
dsfsgSkillId = lens _dsfsgSkillId (\ s a -> s{_dsfsgSkillId = a})

instance AWSRequest DisassociateSkillFromSkillGroup
         where
        type Rs DisassociateSkillFromSkillGroup =
             DisassociateSkillFromSkillGroupResponse
        request = postJSON alexaBusiness
        response
          = receiveEmpty
              (\ s h x ->
                 DisassociateSkillFromSkillGroupResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DisassociateSkillFromSkillGroup
         where

instance NFData DisassociateSkillFromSkillGroup where

instance ToHeaders DisassociateSkillFromSkillGroup
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AlexaForBusiness.DisassociateSkillFromSkillGroup"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DisassociateSkillFromSkillGroup where
        toJSON DisassociateSkillFromSkillGroup'{..}
          = object
              (catMaybes
                 [("SkillGroupArn" .=) <$> _dsfsgSkillGroupARN,
                  Just ("SkillId" .= _dsfsgSkillId)])

instance ToPath DisassociateSkillFromSkillGroup where
        toPath = const "/"

instance ToQuery DisassociateSkillFromSkillGroup
         where
        toQuery = const mempty

-- | /See:/ 'disassociateSkillFromSkillGroupResponse' smart constructor.
newtype DisassociateSkillFromSkillGroupResponse = DisassociateSkillFromSkillGroupResponse'
  { _dsfsgrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisassociateSkillFromSkillGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsfsgrsResponseStatus' - -- | The response status code.
disassociateSkillFromSkillGroupResponse
    :: Int -- ^ 'dsfsgrsResponseStatus'
    -> DisassociateSkillFromSkillGroupResponse
disassociateSkillFromSkillGroupResponse pResponseStatus_ =
  DisassociateSkillFromSkillGroupResponse'
    {_dsfsgrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dsfsgrsResponseStatus :: Lens' DisassociateSkillFromSkillGroupResponse Int
dsfsgrsResponseStatus = lens _dsfsgrsResponseStatus (\ s a -> s{_dsfsgrsResponseStatus = a})

instance NFData
           DisassociateSkillFromSkillGroupResponse
         where
