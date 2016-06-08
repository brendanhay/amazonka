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
-- Module      : Network.AWS.SES.DeleteIdentity
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified identity (email address or domain) from the list of verified identities.
--
-- This action is throttled at one request per second.
module Network.AWS.SES.DeleteIdentity
    (
    -- * Creating a Request
      deleteIdentity
    , DeleteIdentity
    -- * Request Lenses
    , diIdentity

    -- * Destructuring the Response
    , deleteIdentityResponse
    , DeleteIdentityResponse
    -- * Response Lenses
    , dirsResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SES.Types
import           Network.AWS.SES.Types.Product

-- | /See:/ 'deleteIdentity' smart constructor.
newtype DeleteIdentity = DeleteIdentity'
    { _diIdentity :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteIdentity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diIdentity'
deleteIdentity
    :: Text -- ^ 'diIdentity'
    -> DeleteIdentity
deleteIdentity pIdentity_ =
    DeleteIdentity'
    { _diIdentity = pIdentity_
    }

-- | The identity to be removed from the list of identities for the AWS Account.
diIdentity :: Lens' DeleteIdentity Text
diIdentity = lens _diIdentity (\ s a -> s{_diIdentity = a});

instance AWSRequest DeleteIdentity where
        type Rs DeleteIdentity = DeleteIdentityResponse
        request = postQuery ses
        response
          = receiveXMLWrapper "DeleteIdentityResult"
              (\ s h x ->
                 DeleteIdentityResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteIdentity

instance NFData DeleteIdentity

instance ToHeaders DeleteIdentity where
        toHeaders = const mempty

instance ToPath DeleteIdentity where
        toPath = const "/"

instance ToQuery DeleteIdentity where
        toQuery DeleteIdentity'{..}
          = mconcat
              ["Action" =: ("DeleteIdentity" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "Identity" =: _diIdentity]

-- | /See:/ 'deleteIdentityResponse' smart constructor.
newtype DeleteIdentityResponse = DeleteIdentityResponse'
    { _dirsResponseStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteIdentityResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dirsResponseStatus'
deleteIdentityResponse
    :: Int -- ^ 'dirsResponseStatus'
    -> DeleteIdentityResponse
deleteIdentityResponse pResponseStatus_ =
    DeleteIdentityResponse'
    { _dirsResponseStatus = pResponseStatus_
    }

-- | The response status code.
dirsResponseStatus :: Lens' DeleteIdentityResponse Int
dirsResponseStatus = lens _dirsResponseStatus (\ s a -> s{_dirsResponseStatus = a});

instance NFData DeleteIdentityResponse
