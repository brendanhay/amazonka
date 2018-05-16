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
-- Module      : Network.AWS.IAM.CreateAccountAlias
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an alias for your AWS account. For information about using an AWS account alias, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/AccountAlias.html Using an Alias for Your AWS Account ID> in the /IAM User Guide/ .
--
--
module Network.AWS.IAM.CreateAccountAlias
    (
    -- * Creating a Request
      createAccountAlias
    , CreateAccountAlias
    -- * Request Lenses
    , caaAccountAlias

    -- * Destructuring the Response
    , createAccountAliasResponse
    , CreateAccountAliasResponse
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createAccountAlias' smart constructor.
newtype CreateAccountAlias = CreateAccountAlias'
  { _caaAccountAlias :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateAccountAlias' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'caaAccountAlias' - The account alias to create. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of lowercase letters, digits, and dashes. You cannot start or finish with a dash, nor can you have two dashes in a row.
createAccountAlias
    :: Text -- ^ 'caaAccountAlias'
    -> CreateAccountAlias
createAccountAlias pAccountAlias_ =
  CreateAccountAlias' {_caaAccountAlias = pAccountAlias_}


-- | The account alias to create. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of lowercase letters, digits, and dashes. You cannot start or finish with a dash, nor can you have two dashes in a row.
caaAccountAlias :: Lens' CreateAccountAlias Text
caaAccountAlias = lens _caaAccountAlias (\ s a -> s{_caaAccountAlias = a})

instance AWSRequest CreateAccountAlias where
        type Rs CreateAccountAlias =
             CreateAccountAliasResponse
        request = postQuery iam
        response = receiveNull CreateAccountAliasResponse'

instance Hashable CreateAccountAlias where

instance NFData CreateAccountAlias where

instance ToHeaders CreateAccountAlias where
        toHeaders = const mempty

instance ToPath CreateAccountAlias where
        toPath = const "/"

instance ToQuery CreateAccountAlias where
        toQuery CreateAccountAlias'{..}
          = mconcat
              ["Action" =: ("CreateAccountAlias" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "AccountAlias" =: _caaAccountAlias]

-- | /See:/ 'createAccountAliasResponse' smart constructor.
data CreateAccountAliasResponse =
  CreateAccountAliasResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateAccountAliasResponse' with the minimum fields required to make a request.
--
createAccountAliasResponse
    :: CreateAccountAliasResponse
createAccountAliasResponse = CreateAccountAliasResponse'


instance NFData CreateAccountAliasResponse where
