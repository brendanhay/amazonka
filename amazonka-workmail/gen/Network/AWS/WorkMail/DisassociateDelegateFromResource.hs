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
-- Module      : Network.AWS.WorkMail.DisassociateDelegateFromResource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a member from the resource's set of delegates.
--
--
module Network.AWS.WorkMail.DisassociateDelegateFromResource
    (
    -- * Creating a Request
      disassociateDelegateFromResource
    , DisassociateDelegateFromResource
    -- * Request Lenses
    , ddfrOrganizationId
    , ddfrResourceId
    , ddfrEntityId

    -- * Destructuring the Response
    , disassociateDelegateFromResourceResponse
    , DisassociateDelegateFromResourceResponse
    -- * Response Lenses
    , ddfrrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkMail.Types
import Network.AWS.WorkMail.Types.Product

-- | /See:/ 'disassociateDelegateFromResource' smart constructor.
data DisassociateDelegateFromResource = DisassociateDelegateFromResource'
  { _ddfrOrganizationId :: !Text
  , _ddfrResourceId     :: !Text
  , _ddfrEntityId       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisassociateDelegateFromResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddfrOrganizationId' - The identifier for the organization under which the resource exists.
--
-- * 'ddfrResourceId' - The identifier of the resource from which delegates' set members are removed.
--
-- * 'ddfrEntityId' - The identifier for the member (user, group) to be removed from the resource's delegates.
disassociateDelegateFromResource
    :: Text -- ^ 'ddfrOrganizationId'
    -> Text -- ^ 'ddfrResourceId'
    -> Text -- ^ 'ddfrEntityId'
    -> DisassociateDelegateFromResource
disassociateDelegateFromResource pOrganizationId_ pResourceId_ pEntityId_ =
  DisassociateDelegateFromResource'
    { _ddfrOrganizationId = pOrganizationId_
    , _ddfrResourceId = pResourceId_
    , _ddfrEntityId = pEntityId_
    }


-- | The identifier for the organization under which the resource exists.
ddfrOrganizationId :: Lens' DisassociateDelegateFromResource Text
ddfrOrganizationId = lens _ddfrOrganizationId (\ s a -> s{_ddfrOrganizationId = a})

-- | The identifier of the resource from which delegates' set members are removed.
ddfrResourceId :: Lens' DisassociateDelegateFromResource Text
ddfrResourceId = lens _ddfrResourceId (\ s a -> s{_ddfrResourceId = a})

-- | The identifier for the member (user, group) to be removed from the resource's delegates.
ddfrEntityId :: Lens' DisassociateDelegateFromResource Text
ddfrEntityId = lens _ddfrEntityId (\ s a -> s{_ddfrEntityId = a})

instance AWSRequest DisassociateDelegateFromResource
         where
        type Rs DisassociateDelegateFromResource =
             DisassociateDelegateFromResourceResponse
        request = postJSON workMail
        response
          = receiveEmpty
              (\ s h x ->
                 DisassociateDelegateFromResourceResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DisassociateDelegateFromResource
         where

instance NFData DisassociateDelegateFromResource
         where

instance ToHeaders DisassociateDelegateFromResource
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("WorkMailService.DisassociateDelegateFromResource"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DisassociateDelegateFromResource
         where
        toJSON DisassociateDelegateFromResource'{..}
          = object
              (catMaybes
                 [Just ("OrganizationId" .= _ddfrOrganizationId),
                  Just ("ResourceId" .= _ddfrResourceId),
                  Just ("EntityId" .= _ddfrEntityId)])

instance ToPath DisassociateDelegateFromResource
         where
        toPath = const "/"

instance ToQuery DisassociateDelegateFromResource
         where
        toQuery = const mempty

-- | /See:/ 'disassociateDelegateFromResourceResponse' smart constructor.
newtype DisassociateDelegateFromResourceResponse = DisassociateDelegateFromResourceResponse'
  { _ddfrrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisassociateDelegateFromResourceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddfrrsResponseStatus' - -- | The response status code.
disassociateDelegateFromResourceResponse
    :: Int -- ^ 'ddfrrsResponseStatus'
    -> DisassociateDelegateFromResourceResponse
disassociateDelegateFromResourceResponse pResponseStatus_ =
  DisassociateDelegateFromResourceResponse'
    {_ddfrrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
ddfrrsResponseStatus :: Lens' DisassociateDelegateFromResourceResponse Int
ddfrrsResponseStatus = lens _ddfrrsResponseStatus (\ s a -> s{_ddfrrsResponseStatus = a})

instance NFData
           DisassociateDelegateFromResourceResponse
         where
