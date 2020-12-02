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
-- Module      : Network.AWS.WorkMail.AssociateDelegateToResource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a member to the resource's set of delegates.
--
--
module Network.AWS.WorkMail.AssociateDelegateToResource
    (
    -- * Creating a Request
      associateDelegateToResource
    , AssociateDelegateToResource
    -- * Request Lenses
    , adtrOrganizationId
    , adtrResourceId
    , adtrEntityId

    -- * Destructuring the Response
    , associateDelegateToResourceResponse
    , AssociateDelegateToResourceResponse
    -- * Response Lenses
    , adtrrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkMail.Types
import Network.AWS.WorkMail.Types.Product

-- | /See:/ 'associateDelegateToResource' smart constructor.
data AssociateDelegateToResource = AssociateDelegateToResource'
  { _adtrOrganizationId :: !Text
  , _adtrResourceId     :: !Text
  , _adtrEntityId       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssociateDelegateToResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'adtrOrganizationId' - The organization under which the resource exists.
--
-- * 'adtrResourceId' - The resource for which members are associated.
--
-- * 'adtrEntityId' - The member (user or group) to associate to the resource.
associateDelegateToResource
    :: Text -- ^ 'adtrOrganizationId'
    -> Text -- ^ 'adtrResourceId'
    -> Text -- ^ 'adtrEntityId'
    -> AssociateDelegateToResource
associateDelegateToResource pOrganizationId_ pResourceId_ pEntityId_ =
  AssociateDelegateToResource'
    { _adtrOrganizationId = pOrganizationId_
    , _adtrResourceId = pResourceId_
    , _adtrEntityId = pEntityId_
    }


-- | The organization under which the resource exists.
adtrOrganizationId :: Lens' AssociateDelegateToResource Text
adtrOrganizationId = lens _adtrOrganizationId (\ s a -> s{_adtrOrganizationId = a})

-- | The resource for which members are associated.
adtrResourceId :: Lens' AssociateDelegateToResource Text
adtrResourceId = lens _adtrResourceId (\ s a -> s{_adtrResourceId = a})

-- | The member (user or group) to associate to the resource.
adtrEntityId :: Lens' AssociateDelegateToResource Text
adtrEntityId = lens _adtrEntityId (\ s a -> s{_adtrEntityId = a})

instance AWSRequest AssociateDelegateToResource where
        type Rs AssociateDelegateToResource =
             AssociateDelegateToResourceResponse
        request = postJSON workMail
        response
          = receiveEmpty
              (\ s h x ->
                 AssociateDelegateToResourceResponse' <$>
                   (pure (fromEnum s)))

instance Hashable AssociateDelegateToResource where

instance NFData AssociateDelegateToResource where

instance ToHeaders AssociateDelegateToResource where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("WorkMailService.AssociateDelegateToResource" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AssociateDelegateToResource where
        toJSON AssociateDelegateToResource'{..}
          = object
              (catMaybes
                 [Just ("OrganizationId" .= _adtrOrganizationId),
                  Just ("ResourceId" .= _adtrResourceId),
                  Just ("EntityId" .= _adtrEntityId)])

instance ToPath AssociateDelegateToResource where
        toPath = const "/"

instance ToQuery AssociateDelegateToResource where
        toQuery = const mempty

-- | /See:/ 'associateDelegateToResourceResponse' smart constructor.
newtype AssociateDelegateToResourceResponse = AssociateDelegateToResourceResponse'
  { _adtrrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssociateDelegateToResourceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'adtrrsResponseStatus' - -- | The response status code.
associateDelegateToResourceResponse
    :: Int -- ^ 'adtrrsResponseStatus'
    -> AssociateDelegateToResourceResponse
associateDelegateToResourceResponse pResponseStatus_ =
  AssociateDelegateToResourceResponse'
    {_adtrrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
adtrrsResponseStatus :: Lens' AssociateDelegateToResourceResponse Int
adtrrsResponseStatus = lens _adtrrsResponseStatus (\ s a -> s{_adtrrsResponseStatus = a})

instance NFData AssociateDelegateToResourceResponse
         where
