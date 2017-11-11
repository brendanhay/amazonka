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
-- Module      : Network.AWS.KMS.DeleteAlias
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified alias. To map an alias to a different key, call 'UpdateAlias' .
--
--
module Network.AWS.KMS.DeleteAlias
    (
    -- * Creating a Request
      deleteAlias
    , DeleteAlias
    -- * Request Lenses
    , daAliasName

    -- * Destructuring the Response
    , deleteAliasResponse
    , DeleteAliasResponse
    ) where

import Network.AWS.KMS.Types
import Network.AWS.KMS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteAlias' smart constructor.
newtype DeleteAlias = DeleteAlias'
  { _daAliasName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteAlias' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daAliasName' - The alias to be deleted. The name must start with the word "alias" followed by a forward slash (alias/). Aliases that begin with "alias/AWS" are reserved.
deleteAlias
    :: Text -- ^ 'daAliasName'
    -> DeleteAlias
deleteAlias pAliasName_ = DeleteAlias' {_daAliasName = pAliasName_}


-- | The alias to be deleted. The name must start with the word "alias" followed by a forward slash (alias/). Aliases that begin with "alias/AWS" are reserved.
daAliasName :: Lens' DeleteAlias Text
daAliasName = lens _daAliasName (\ s a -> s{_daAliasName = a});

instance AWSRequest DeleteAlias where
        type Rs DeleteAlias = DeleteAliasResponse
        request = postJSON kms
        response = receiveNull DeleteAliasResponse'

instance Hashable DeleteAlias where

instance NFData DeleteAlias where

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
          = object
              (catMaybes [Just ("AliasName" .= _daAliasName)])

instance ToPath DeleteAlias where
        toPath = const "/"

instance ToQuery DeleteAlias where
        toQuery = const mempty

-- | /See:/ 'deleteAliasResponse' smart constructor.
data DeleteAliasResponse =
  DeleteAliasResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteAliasResponse' with the minimum fields required to make a request.
--
deleteAliasResponse
    :: DeleteAliasResponse
deleteAliasResponse = DeleteAliasResponse'


instance NFData DeleteAliasResponse where
