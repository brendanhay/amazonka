{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.IAM.DeleteAccountAlias
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Deletes the specified AWS account alias. For information about using an
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
    , daaAccountAlias

    -- * Response
    , DeleteAccountAliasResponse
    -- ** Response constructor
    , deleteAccountAliasResponse
    ) where

import Network.AWS.IAM.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteAccountAlias' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'daaAccountAlias'
newtype DeleteAccountAlias = DeleteAccountAlias'{_daaAccountAlias :: Text} deriving (Eq, Read, Show)

-- | 'DeleteAccountAlias' smart constructor.
deleteAccountAlias :: Text -> DeleteAccountAlias
deleteAccountAlias pAccountAlias = DeleteAccountAlias'{_daaAccountAlias = pAccountAlias};

-- | The name of the account alias to delete.
daaAccountAlias :: Lens' DeleteAccountAlias Text
daaAccountAlias = lens _daaAccountAlias (\ s a -> s{_daaAccountAlias = a});

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
               "AccountAlias" =: _daaAccountAlias]

-- | /See:/ 'deleteAccountAliasResponse' smart constructor.
data DeleteAccountAliasResponse = DeleteAccountAliasResponse' deriving (Eq, Read, Show)

-- | 'DeleteAccountAliasResponse' smart constructor.
deleteAccountAliasResponse :: DeleteAccountAliasResponse
deleteAccountAliasResponse = DeleteAccountAliasResponse';
