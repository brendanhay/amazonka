{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.DeleteIdentities
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes identities from an identity pool. You can specify a list of 1-60
-- identities that you want to delete.
--
-- You must use AWS Developer credentials to call this API.
--
-- <http://docs.aws.amazon.com/cognitoidentity/latest/APIReference/API_DeleteIdentities.html>
module Network.AWS.CognitoIdentity.DeleteIdentities
    (
    -- * Request
      DeleteIdentities
    -- ** Request constructor
    , deleteIdentities
    -- ** Request lenses
    , diIdentityIdsToDelete

    -- * Response
    , DeleteIdentitiesResponse
    -- ** Response constructor
    , deleteIdentitiesResponse
    -- ** Response lenses
    , dirsUnprocessedIdentityIds
    , dirsStatus
    ) where

import           Network.AWS.CognitoIdentity.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Input to the @DeleteIdentities@ action.
--
-- /See:/ 'deleteIdentities' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'diIdentityIdsToDelete'
newtype DeleteIdentities = DeleteIdentities'
    { _diIdentityIdsToDelete :: List1 Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteIdentities' smart constructor.
deleteIdentities :: NonEmpty Text -> DeleteIdentities
deleteIdentities pIdentityIdsToDelete_ =
    DeleteIdentities'
    { _diIdentityIdsToDelete = _List1 # pIdentityIdsToDelete_
    }

-- | A list of 1-60 identities that you want to delete.
diIdentityIdsToDelete :: Lens' DeleteIdentities (NonEmpty Text)
diIdentityIdsToDelete = lens _diIdentityIdsToDelete (\ s a -> s{_diIdentityIdsToDelete = a}) . _List1;

instance AWSRequest DeleteIdentities where
        type Sv DeleteIdentities = CognitoIdentity
        type Rs DeleteIdentities = DeleteIdentitiesResponse
        request = postJSON "DeleteIdentities"
        response
          = receiveJSON
              (\ s h x ->
                 DeleteIdentitiesResponse' <$>
                   (x .?> "UnprocessedIdentityIds" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance ToHeaders DeleteIdentities where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityService.DeleteIdentities" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteIdentities where
        toJSON DeleteIdentities'{..}
          = object
              ["IdentityIdsToDelete" .= _diIdentityIdsToDelete]

instance ToPath DeleteIdentities where
        toPath = const "/"

instance ToQuery DeleteIdentities where
        toQuery = const mempty

-- | Returned in response to a successful @DeleteIdentities@ operation.
--
-- /See:/ 'deleteIdentitiesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dirsUnprocessedIdentityIds'
--
-- * 'dirsStatus'
data DeleteIdentitiesResponse = DeleteIdentitiesResponse'
    { _dirsUnprocessedIdentityIds :: !(Maybe [UnprocessedIdentityId])
    , _dirsStatus                 :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteIdentitiesResponse' smart constructor.
deleteIdentitiesResponse :: Int -> DeleteIdentitiesResponse
deleteIdentitiesResponse pStatus_ =
    DeleteIdentitiesResponse'
    { _dirsUnprocessedIdentityIds = Nothing
    , _dirsStatus = pStatus_
    }

-- | An array of UnprocessedIdentityId objects, each of which contains an
-- ErrorCode and IdentityId.
dirsUnprocessedIdentityIds :: Lens' DeleteIdentitiesResponse [UnprocessedIdentityId]
dirsUnprocessedIdentityIds = lens _dirsUnprocessedIdentityIds (\ s a -> s{_dirsUnprocessedIdentityIds = a}) . _Default;

-- | FIXME: Undocumented member.
dirsStatus :: Lens' DeleteIdentitiesResponse Int
dirsStatus = lens _dirsStatus (\ s a -> s{_dirsStatus = a});
