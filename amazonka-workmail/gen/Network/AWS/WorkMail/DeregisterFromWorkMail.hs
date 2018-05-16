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
-- Module      : Network.AWS.WorkMail.DeregisterFromWorkMail
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Mark a user, group, or resource as no longer used in Amazon WorkMail. This action disassociates the mailbox and schedules it for clean-up. Amazon WorkMail keeps mailboxes for 30 days before they are permanently removed. The functionality in the console is /Disable/ .
--
--
module Network.AWS.WorkMail.DeregisterFromWorkMail
    (
    -- * Creating a Request
      deregisterFromWorkMail
    , DeregisterFromWorkMail
    -- * Request Lenses
    , dfwmOrganizationId
    , dfwmEntityId

    -- * Destructuring the Response
    , deregisterFromWorkMailResponse
    , DeregisterFromWorkMailResponse
    -- * Response Lenses
    , dfwmrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkMail.Types
import Network.AWS.WorkMail.Types.Product

-- | /See:/ 'deregisterFromWorkMail' smart constructor.
data DeregisterFromWorkMail = DeregisterFromWorkMail'
  { _dfwmOrganizationId :: !Text
  , _dfwmEntityId       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeregisterFromWorkMail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfwmOrganizationId' - The identifier for the organization under which the Amazon WorkMail entity exists.
--
-- * 'dfwmEntityId' - The identifier for the entity to be updated.
deregisterFromWorkMail
    :: Text -- ^ 'dfwmOrganizationId'
    -> Text -- ^ 'dfwmEntityId'
    -> DeregisterFromWorkMail
deregisterFromWorkMail pOrganizationId_ pEntityId_ =
  DeregisterFromWorkMail'
    {_dfwmOrganizationId = pOrganizationId_, _dfwmEntityId = pEntityId_}


-- | The identifier for the organization under which the Amazon WorkMail entity exists.
dfwmOrganizationId :: Lens' DeregisterFromWorkMail Text
dfwmOrganizationId = lens _dfwmOrganizationId (\ s a -> s{_dfwmOrganizationId = a})

-- | The identifier for the entity to be updated.
dfwmEntityId :: Lens' DeregisterFromWorkMail Text
dfwmEntityId = lens _dfwmEntityId (\ s a -> s{_dfwmEntityId = a})

instance AWSRequest DeregisterFromWorkMail where
        type Rs DeregisterFromWorkMail =
             DeregisterFromWorkMailResponse
        request = postJSON workMail
        response
          = receiveEmpty
              (\ s h x ->
                 DeregisterFromWorkMailResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DeregisterFromWorkMail where

instance NFData DeregisterFromWorkMail where

instance ToHeaders DeregisterFromWorkMail where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("WorkMailService.DeregisterFromWorkMail" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeregisterFromWorkMail where
        toJSON DeregisterFromWorkMail'{..}
          = object
              (catMaybes
                 [Just ("OrganizationId" .= _dfwmOrganizationId),
                  Just ("EntityId" .= _dfwmEntityId)])

instance ToPath DeregisterFromWorkMail where
        toPath = const "/"

instance ToQuery DeregisterFromWorkMail where
        toQuery = const mempty

-- | /See:/ 'deregisterFromWorkMailResponse' smart constructor.
newtype DeregisterFromWorkMailResponse = DeregisterFromWorkMailResponse'
  { _dfwmrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeregisterFromWorkMailResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfwmrsResponseStatus' - -- | The response status code.
deregisterFromWorkMailResponse
    :: Int -- ^ 'dfwmrsResponseStatus'
    -> DeregisterFromWorkMailResponse
deregisterFromWorkMailResponse pResponseStatus_ =
  DeregisterFromWorkMailResponse' {_dfwmrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dfwmrsResponseStatus :: Lens' DeregisterFromWorkMailResponse Int
dfwmrsResponseStatus = lens _dfwmrsResponseStatus (\ s a -> s{_dfwmrsResponseStatus = a})

instance NFData DeregisterFromWorkMailResponse where
