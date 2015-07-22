{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.DeleteAccountAlias
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified AWS account alias. For information about using an
-- AWS account alias, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/AccountAlias.html Using an Alias for Your AWS Account ID>
-- in the /Using IAM/ guide.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_DeleteAccountAlias.html>
module Network.AWS.IAM.DeleteAccountAlias
    (
    -- * Request
      DeleteAccountAlias
    -- ** Request constructor
    , deleteAccountAlias
    -- ** Request lenses
    , daarqAccountAlias

    -- * Response
    , DeleteAccountAliasResponse
    -- ** Response constructor
    , deleteAccountAliasResponse
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteAccountAlias' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'daarqAccountAlias'
newtype DeleteAccountAlias = DeleteAccountAlias'
    { _daarqAccountAlias :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteAccountAlias' smart constructor.
deleteAccountAlias :: Text -> DeleteAccountAlias
deleteAccountAlias pAccountAlias_ =
    DeleteAccountAlias'
    { _daarqAccountAlias = pAccountAlias_
    }

-- | The name of the account alias to delete.
daarqAccountAlias :: Lens' DeleteAccountAlias Text
daarqAccountAlias = lens _daarqAccountAlias (\ s a -> s{_daarqAccountAlias = a});

instance AWSRequest DeleteAccountAlias where
        type Sv DeleteAccountAlias = IAM
        type Rs DeleteAccountAlias =
             DeleteAccountAliasResponse
        request = post
        response = receiveNull DeleteAccountAliasResponse'

instance ToHeaders DeleteAccountAlias where
        toHeaders = const mempty

instance ToPath DeleteAccountAlias where
        toPath = const "/"

instance ToQuery DeleteAccountAlias where
        toQuery DeleteAccountAlias'{..}
          = mconcat
              ["Action" =: ("DeleteAccountAlias" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "AccountAlias" =: _daarqAccountAlias]

-- | /See:/ 'deleteAccountAliasResponse' smart constructor.
data DeleteAccountAliasResponse =
    DeleteAccountAliasResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteAccountAliasResponse' smart constructor.
deleteAccountAliasResponse :: DeleteAccountAliasResponse
deleteAccountAliasResponse = DeleteAccountAliasResponse'
