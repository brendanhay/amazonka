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
-- Module      : Network.AWS.AlexaBusiness.AssociateSkillWithUsers
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Makes a private skill available for enrolled users to enable on their devices.
--
--
module Network.AWS.AlexaBusiness.AssociateSkillWithUsers
    (
    -- * Creating a Request
      associateSkillWithUsers
    , AssociateSkillWithUsers
    -- * Request Lenses
    , aswuSkillId

    -- * Destructuring the Response
    , associateSkillWithUsersResponse
    , AssociateSkillWithUsersResponse
    -- * Response Lenses
    , aswursResponseStatus
    ) where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.AlexaBusiness.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'associateSkillWithUsers' smart constructor.
newtype AssociateSkillWithUsers = AssociateSkillWithUsers'
  { _aswuSkillId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssociateSkillWithUsers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aswuSkillId' - The private skill ID you want to make available to enrolled users.
associateSkillWithUsers
    :: Text -- ^ 'aswuSkillId'
    -> AssociateSkillWithUsers
associateSkillWithUsers pSkillId_ =
  AssociateSkillWithUsers' {_aswuSkillId = pSkillId_}


-- | The private skill ID you want to make available to enrolled users.
aswuSkillId :: Lens' AssociateSkillWithUsers Text
aswuSkillId = lens _aswuSkillId (\ s a -> s{_aswuSkillId = a})

instance AWSRequest AssociateSkillWithUsers where
        type Rs AssociateSkillWithUsers =
             AssociateSkillWithUsersResponse
        request = postJSON alexaBusiness
        response
          = receiveEmpty
              (\ s h x ->
                 AssociateSkillWithUsersResponse' <$>
                   (pure (fromEnum s)))

instance Hashable AssociateSkillWithUsers where

instance NFData AssociateSkillWithUsers where

instance ToHeaders AssociateSkillWithUsers where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AlexaForBusiness.AssociateSkillWithUsers" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AssociateSkillWithUsers where
        toJSON AssociateSkillWithUsers'{..}
          = object
              (catMaybes [Just ("SkillId" .= _aswuSkillId)])

instance ToPath AssociateSkillWithUsers where
        toPath = const "/"

instance ToQuery AssociateSkillWithUsers where
        toQuery = const mempty

-- | /See:/ 'associateSkillWithUsersResponse' smart constructor.
newtype AssociateSkillWithUsersResponse = AssociateSkillWithUsersResponse'
  { _aswursResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssociateSkillWithUsersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aswursResponseStatus' - -- | The response status code.
associateSkillWithUsersResponse
    :: Int -- ^ 'aswursResponseStatus'
    -> AssociateSkillWithUsersResponse
associateSkillWithUsersResponse pResponseStatus_ =
  AssociateSkillWithUsersResponse' {_aswursResponseStatus = pResponseStatus_}


-- | -- | The response status code.
aswursResponseStatus :: Lens' AssociateSkillWithUsersResponse Int
aswursResponseStatus = lens _aswursResponseStatus (\ s a -> s{_aswursResponseStatus = a})

instance NFData AssociateSkillWithUsersResponse where
