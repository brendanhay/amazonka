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
-- Module      : Network.AWS.WorkMail.RegisterToWorkMail
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers an existing and disabled user, group, or resource/entity for Amazon WorkMail use by associating a mailbox and calendaring capabilities. It performs no change if the entity is enabled and fails if the entity is deleted. This operation results in the accumulation of costs. For more information, see <http://aws.amazon.com/workmail/pricing Pricing> . The equivalent console functionality for this operation is /Enable/ . Users can either be created by calling the CreateUser API or they can be synchronized from your directory. For more information, see DeregisterFromWorkMail.
--
--
module Network.AWS.WorkMail.RegisterToWorkMail
    (
    -- * Creating a Request
      registerToWorkMail
    , RegisterToWorkMail
    -- * Request Lenses
    , rtwmOrganizationId
    , rtwmEntityId
    , rtwmEmail

    -- * Destructuring the Response
    , registerToWorkMailResponse
    , RegisterToWorkMailResponse
    -- * Response Lenses
    , rtwmrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkMail.Types
import Network.AWS.WorkMail.Types.Product

-- | /See:/ 'registerToWorkMail' smart constructor.
data RegisterToWorkMail = RegisterToWorkMail'
  { _rtwmOrganizationId :: !Text
  , _rtwmEntityId       :: !Text
  , _rtwmEmail          :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RegisterToWorkMail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtwmOrganizationId' - The identifier for the organization under which the Amazon WorkMail entity exists.
--
-- * 'rtwmEntityId' - The identifier for the entity to be updated.
--
-- * 'rtwmEmail' - The email for the entity to be updated.
registerToWorkMail
    :: Text -- ^ 'rtwmOrganizationId'
    -> Text -- ^ 'rtwmEntityId'
    -> Text -- ^ 'rtwmEmail'
    -> RegisterToWorkMail
registerToWorkMail pOrganizationId_ pEntityId_ pEmail_ =
  RegisterToWorkMail'
    { _rtwmOrganizationId = pOrganizationId_
    , _rtwmEntityId = pEntityId_
    , _rtwmEmail = pEmail_
    }


-- | The identifier for the organization under which the Amazon WorkMail entity exists.
rtwmOrganizationId :: Lens' RegisterToWorkMail Text
rtwmOrganizationId = lens _rtwmOrganizationId (\ s a -> s{_rtwmOrganizationId = a})

-- | The identifier for the entity to be updated.
rtwmEntityId :: Lens' RegisterToWorkMail Text
rtwmEntityId = lens _rtwmEntityId (\ s a -> s{_rtwmEntityId = a})

-- | The email for the entity to be updated.
rtwmEmail :: Lens' RegisterToWorkMail Text
rtwmEmail = lens _rtwmEmail (\ s a -> s{_rtwmEmail = a})

instance AWSRequest RegisterToWorkMail where
        type Rs RegisterToWorkMail =
             RegisterToWorkMailResponse
        request = postJSON workMail
        response
          = receiveEmpty
              (\ s h x ->
                 RegisterToWorkMailResponse' <$> (pure (fromEnum s)))

instance Hashable RegisterToWorkMail where

instance NFData RegisterToWorkMail where

instance ToHeaders RegisterToWorkMail where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("WorkMailService.RegisterToWorkMail" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RegisterToWorkMail where
        toJSON RegisterToWorkMail'{..}
          = object
              (catMaybes
                 [Just ("OrganizationId" .= _rtwmOrganizationId),
                  Just ("EntityId" .= _rtwmEntityId),
                  Just ("Email" .= _rtwmEmail)])

instance ToPath RegisterToWorkMail where
        toPath = const "/"

instance ToQuery RegisterToWorkMail where
        toQuery = const mempty

-- | /See:/ 'registerToWorkMailResponse' smart constructor.
newtype RegisterToWorkMailResponse = RegisterToWorkMailResponse'
  { _rtwmrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RegisterToWorkMailResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtwmrsResponseStatus' - -- | The response status code.
registerToWorkMailResponse
    :: Int -- ^ 'rtwmrsResponseStatus'
    -> RegisterToWorkMailResponse
registerToWorkMailResponse pResponseStatus_ =
  RegisterToWorkMailResponse' {_rtwmrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
rtwmrsResponseStatus :: Lens' RegisterToWorkMailResponse Int
rtwmrsResponseStatus = lens _rtwmrsResponseStatus (\ s a -> s{_rtwmrsResponseStatus = a})

instance NFData RegisterToWorkMailResponse where
