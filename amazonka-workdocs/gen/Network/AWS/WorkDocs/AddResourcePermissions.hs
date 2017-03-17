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
-- Module      : Network.AWS.WorkDocs.AddResourcePermissions
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a set of permissions for the specified folder or document. The resource permissions are overwritten if the principals already have different permissions.
--
--
module Network.AWS.WorkDocs.AddResourcePermissions
    (
    -- * Creating a Request
      addResourcePermissions
    , AddResourcePermissions
    -- * Request Lenses
    , arpResourceId
    , arpPrincipals

    -- * Destructuring the Response
    , addResourcePermissionsResponse
    , AddResourcePermissionsResponse
    -- * Response Lenses
    , arprsShareResults
    , arprsResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.WorkDocs.Types
import           Network.AWS.WorkDocs.Types.Product

-- | /See:/ 'addResourcePermissions' smart constructor.
data AddResourcePermissions = AddResourcePermissions'
    { _arpResourceId :: !Text
    , _arpPrincipals :: ![SharePrincipal]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AddResourcePermissions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'arpResourceId' - The ID of the resource.
--
-- * 'arpPrincipals' - The users, groups, or organization being granted permission.
addResourcePermissions
    :: Text -- ^ 'arpResourceId'
    -> AddResourcePermissions
addResourcePermissions pResourceId_ =
    AddResourcePermissions'
    { _arpResourceId = pResourceId_
    , _arpPrincipals = mempty
    }

-- | The ID of the resource.
arpResourceId :: Lens' AddResourcePermissions Text
arpResourceId = lens _arpResourceId (\ s a -> s{_arpResourceId = a});

-- | The users, groups, or organization being granted permission.
arpPrincipals :: Lens' AddResourcePermissions [SharePrincipal]
arpPrincipals = lens _arpPrincipals (\ s a -> s{_arpPrincipals = a}) . _Coerce;

instance AWSRequest AddResourcePermissions where
        type Rs AddResourcePermissions =
             AddResourcePermissionsResponse
        request = postJSON workDocs
        response
          = receiveJSON
              (\ s h x ->
                 AddResourcePermissionsResponse' <$>
                   (x .?> "ShareResults" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable AddResourcePermissions

instance NFData AddResourcePermissions

instance ToHeaders AddResourcePermissions where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AddResourcePermissions where
        toJSON AddResourcePermissions'{..}
          = object
              (catMaybes [Just ("Principals" .= _arpPrincipals)])

instance ToPath AddResourcePermissions where
        toPath AddResourcePermissions'{..}
          = mconcat
              ["/api/v1/resources/", toBS _arpResourceId,
               "/permissions"]

instance ToQuery AddResourcePermissions where
        toQuery = const mempty

-- | /See:/ 'addResourcePermissionsResponse' smart constructor.
data AddResourcePermissionsResponse = AddResourcePermissionsResponse'
    { _arprsShareResults   :: !(Maybe [ShareResult])
    , _arprsResponseStatus :: !Int
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AddResourcePermissionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'arprsShareResults' - The share results.
--
-- * 'arprsResponseStatus' - -- | The response status code.
addResourcePermissionsResponse
    :: Int -- ^ 'arprsResponseStatus'
    -> AddResourcePermissionsResponse
addResourcePermissionsResponse pResponseStatus_ =
    AddResourcePermissionsResponse'
    { _arprsShareResults = Nothing
    , _arprsResponseStatus = pResponseStatus_
    }

-- | The share results.
arprsShareResults :: Lens' AddResourcePermissionsResponse [ShareResult]
arprsShareResults = lens _arprsShareResults (\ s a -> s{_arprsShareResults = a}) . _Default . _Coerce;

-- | -- | The response status code.
arprsResponseStatus :: Lens' AddResourcePermissionsResponse Int
arprsResponseStatus = lens _arprsResponseStatus (\ s a -> s{_arprsResponseStatus = a});

instance NFData AddResourcePermissionsResponse
