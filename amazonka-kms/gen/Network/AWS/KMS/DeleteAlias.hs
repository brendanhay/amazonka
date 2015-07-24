{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.DeleteAlias
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified alias. To associate an alias with a different key,
-- call UpdateAlias.
--
-- <http://docs.aws.amazon.com/kms/latest/APIReference/API_DeleteAlias.html>
module Network.AWS.KMS.DeleteAlias
    (
    -- * Request
      DeleteAlias
    -- ** Request constructor
    , deleteAlias
    -- ** Request lenses
    , daAliasName

    -- * Response
    , DeleteAliasResponse
    -- ** Response constructor
    , deleteAliasResponse
    ) where

import           Network.AWS.KMS.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteAlias' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'daAliasName'
newtype DeleteAlias = DeleteAlias'
    { _daAliasName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteAlias' smart constructor.
deleteAlias :: Text -> DeleteAlias
deleteAlias pAliasName_ =
    DeleteAlias'
    { _daAliasName = pAliasName_
    }

-- | The alias to be deleted. The name must start with the word \"alias\"
-- followed by a forward slash (alias\/). Aliases that begin with
-- \"alias\/AWS\" are reserved.
daAliasName :: Lens' DeleteAlias Text
daAliasName = lens _daAliasName (\ s a -> s{_daAliasName = a});

instance AWSRequest DeleteAlias where
        type Sv DeleteAlias = KMS
        type Rs DeleteAlias = DeleteAliasResponse
        request = postJSON "DeleteAlias"
        response = receiveNull DeleteAliasResponse'

instance ToHeaders DeleteAlias where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("TrentService.DeleteAlias" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteAlias where
        toJSON DeleteAlias'{..}
          = object ["AliasName" .= _daAliasName]

instance ToPath DeleteAlias where
        toPath = const "/"

instance ToQuery DeleteAlias where
        toQuery = const mempty

-- | /See:/ 'deleteAliasResponse' smart constructor.
data DeleteAliasResponse =
    DeleteAliasResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteAliasResponse' smart constructor.
deleteAliasResponse :: DeleteAliasResponse
deleteAliasResponse = DeleteAliasResponse'
