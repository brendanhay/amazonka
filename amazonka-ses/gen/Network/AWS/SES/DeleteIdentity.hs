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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified identity (email address or domain) from the list
-- of verified identities.
--
-- This action is throttled at one request per second.
--
-- /See:/ <http://docs.aws.amazon.com/ses/latest/APIReference/API_DeleteIdentity.html AWS API Reference> for DeleteIdentity.
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
    , dirsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SES.Types
import           Network.AWS.SES.Types.Product

-- | Represents a request instructing the service to delete an identity from
-- the list of identities for the AWS Account.
--
-- /See:/ 'deleteIdentity' smart constructor.
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

-- | The identity to be removed from the list of identities for the AWS
-- Account.
diIdentity :: Lens' DeleteIdentity Text
diIdentity = lens _diIdentity (\ s a -> s{_diIdentity = a});

instance AWSRequest DeleteIdentity where
        type Rs DeleteIdentity = DeleteIdentityResponse
        request = postQuery sES
        response
          = receiveXMLWrapper "DeleteIdentityResult"
              (\ s h x ->
                 DeleteIdentityResponse' <$> (pure (fromEnum s)))

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

-- | An empty element. Receiving this element indicates that the request
-- completed successfully.
--
-- /See:/ 'deleteIdentityResponse' smart constructor.
newtype DeleteIdentityResponse = DeleteIdentityResponse'
    { _dirsStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteIdentityResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dirsStatus'
deleteIdentityResponse
    :: Int -- ^ 'dirsStatus'
    -> DeleteIdentityResponse
deleteIdentityResponse pStatus_ =
    DeleteIdentityResponse'
    { _dirsStatus = pStatus_
    }

-- | The response status code.
dirsStatus :: Lens' DeleteIdentityResponse Int
dirsStatus = lens _dirsStatus (\ s a -> s{_dirsStatus = a});
