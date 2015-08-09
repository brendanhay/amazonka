{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

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
      DeleteIdentity
    , deleteIdentity
    -- * Request Lenses
    , diIdentity

    -- * Destructuring the Response
    , DeleteIdentityResponse
    , deleteIdentityResponse
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
--
-- The fields accessible through corresponding lenses are:
--
-- * 'diIdentity'
newtype DeleteIdentity = DeleteIdentity'
    { _diIdentity :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteIdentity' smart constructor.
deleteIdentity :: Text -> DeleteIdentity
deleteIdentity pIdentity_ =
    DeleteIdentity'
    { _diIdentity = pIdentity_
    }

-- | The identity to be removed from the list of identities for the AWS
-- Account.
diIdentity :: Lens' DeleteIdentity Text
diIdentity = lens _diIdentity (\ s a -> s{_diIdentity = a});

instance AWSRequest DeleteIdentity where
        type Sv DeleteIdentity = SES
        type Rs DeleteIdentity = DeleteIdentityResponse
        request = postQuery
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
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dirsStatus'
newtype DeleteIdentityResponse = DeleteIdentityResponse'
    { _dirsStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteIdentityResponse' smart constructor.
deleteIdentityResponse :: Int -> DeleteIdentityResponse
deleteIdentityResponse pStatus_ =
    DeleteIdentityResponse'
    { _dirsStatus = pStatus_
    }

-- | Undocumented member.
dirsStatus :: Lens' DeleteIdentityResponse Int
dirsStatus = lens _dirsStatus (\ s a -> s{_dirsStatus = a});
