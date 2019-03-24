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
-- Module      : Network.AWS.EC2.DeleteKeyPair
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified key pair, by removing the public key from Amazon EC2.
--
--
module Network.AWS.EC2.DeleteKeyPair
    (
    -- * Creating a Request
      deleteKeyPair
    , DeleteKeyPair
    -- * Request Lenses
    , dkpDryRun
    , dkpKeyName

    -- * Destructuring the Response
    , deleteKeyPairResponse
    , DeleteKeyPairResponse
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteKeyPair' smart constructor.
data DeleteKeyPair = DeleteKeyPair'
  { _dkpDryRun  :: !(Maybe Bool)
  , _dkpKeyName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteKeyPair' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dkpDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dkpKeyName' - The name of the key pair.
deleteKeyPair
    :: Text -- ^ 'dkpKeyName'
    -> DeleteKeyPair
deleteKeyPair pKeyName_ =
  DeleteKeyPair' {_dkpDryRun = Nothing, _dkpKeyName = pKeyName_}


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dkpDryRun :: Lens' DeleteKeyPair (Maybe Bool)
dkpDryRun = lens _dkpDryRun (\ s a -> s{_dkpDryRun = a})

-- | The name of the key pair.
dkpKeyName :: Lens' DeleteKeyPair Text
dkpKeyName = lens _dkpKeyName (\ s a -> s{_dkpKeyName = a})

instance AWSRequest DeleteKeyPair where
        type Rs DeleteKeyPair = DeleteKeyPairResponse
        request = postQuery ec2
        response = receiveNull DeleteKeyPairResponse'

instance Hashable DeleteKeyPair where

instance NFData DeleteKeyPair where

instance ToHeaders DeleteKeyPair where
        toHeaders = const mempty

instance ToPath DeleteKeyPair where
        toPath = const "/"

instance ToQuery DeleteKeyPair where
        toQuery DeleteKeyPair'{..}
          = mconcat
              ["Action" =: ("DeleteKeyPair" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _dkpDryRun, "KeyName" =: _dkpKeyName]

-- | /See:/ 'deleteKeyPairResponse' smart constructor.
data DeleteKeyPairResponse =
  DeleteKeyPairResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteKeyPairResponse' with the minimum fields required to make a request.
--
deleteKeyPairResponse
    :: DeleteKeyPairResponse
deleteKeyPairResponse = DeleteKeyPairResponse'


instance NFData DeleteKeyPairResponse where
