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
-- Module      : Network.AWS.IAM.DeleteAccountAlias
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified AWS account alias. For information about using an AWS account alias, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/AccountAlias.html Using an Alias for Your AWS Account ID> in the /IAM User Guide/ .
--
--
module Network.AWS.IAM.DeleteAccountAlias
    (
    -- * Creating a Request
      deleteAccountAlias
    , DeleteAccountAlias
    -- * Request Lenses
    , daaAccountAlias

    -- * Destructuring the Response
    , deleteAccountAliasResponse
    , DeleteAccountAliasResponse
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteAccountAlias' smart constructor.
newtype DeleteAccountAlias = DeleteAccountAlias'
  { _daaAccountAlias :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteAccountAlias' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daaAccountAlias' - The name of the account alias to delete. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of lowercase letters, digits, and dashes. You cannot start or finish with a dash, nor can you have two dashes in a row.
deleteAccountAlias
    :: Text -- ^ 'daaAccountAlias'
    -> DeleteAccountAlias
deleteAccountAlias pAccountAlias_ =
  DeleteAccountAlias' {_daaAccountAlias = pAccountAlias_}


-- | The name of the account alias to delete. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of lowercase letters, digits, and dashes. You cannot start or finish with a dash, nor can you have two dashes in a row.
daaAccountAlias :: Lens' DeleteAccountAlias Text
daaAccountAlias = lens _daaAccountAlias (\ s a -> s{_daaAccountAlias = a})

instance AWSRequest DeleteAccountAlias where
        type Rs DeleteAccountAlias =
             DeleteAccountAliasResponse
        request = postQuery iam
        response = receiveNull DeleteAccountAliasResponse'

instance Hashable DeleteAccountAlias where

instance NFData DeleteAccountAlias where

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
data DeleteAccountAliasResponse =
  DeleteAccountAliasResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteAccountAliasResponse' with the minimum fields required to make a request.
--
deleteAccountAliasResponse
    :: DeleteAccountAliasResponse
deleteAccountAliasResponse = DeleteAccountAliasResponse'


instance NFData DeleteAccountAliasResponse where
