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
-- Module      : Network.AWS.IAM.UpdateServiceSpecificCredential
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the status of a service-specific credential to @Active@ or @Inactive@ . Service-specific credentials that are inactive cannot be used for authentication to the service. This operation can be used to disable a user’s service-specific credential as part of a credential rotation work flow.
--
--
module Network.AWS.IAM.UpdateServiceSpecificCredential
    (
    -- * Creating a Request
      updateServiceSpecificCredential
    , UpdateServiceSpecificCredential
    -- * Request Lenses
    , usscUserName
    , usscServiceSpecificCredentialId
    , usscStatus

    -- * Destructuring the Response
    , updateServiceSpecificCredentialResponse
    , UpdateServiceSpecificCredentialResponse
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateServiceSpecificCredential' smart constructor.
data UpdateServiceSpecificCredential = UpdateServiceSpecificCredential'
  { _usscUserName                    :: !(Maybe Text)
  , _usscServiceSpecificCredentialId :: !Text
  , _usscStatus                      :: !StatusType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateServiceSpecificCredential' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usscUserName' - The name of the IAM user associated with the service-specific credential. If you do not specify this value, then the operation assumes the user whose credentials are used to call the operation. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- * 'usscServiceSpecificCredentialId' - The unique identifier of the service-specific credential. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that can consist of any upper or lowercased letter or digit.
--
-- * 'usscStatus' - The status to be assigned to the service-specific credential.
updateServiceSpecificCredential
    :: Text -- ^ 'usscServiceSpecificCredentialId'
    -> StatusType -- ^ 'usscStatus'
    -> UpdateServiceSpecificCredential
updateServiceSpecificCredential pServiceSpecificCredentialId_ pStatus_ =
  UpdateServiceSpecificCredential'
    { _usscUserName = Nothing
    , _usscServiceSpecificCredentialId = pServiceSpecificCredentialId_
    , _usscStatus = pStatus_
    }


-- | The name of the IAM user associated with the service-specific credential. If you do not specify this value, then the operation assumes the user whose credentials are used to call the operation. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
usscUserName :: Lens' UpdateServiceSpecificCredential (Maybe Text)
usscUserName = lens _usscUserName (\ s a -> s{_usscUserName = a})

-- | The unique identifier of the service-specific credential. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that can consist of any upper or lowercased letter or digit.
usscServiceSpecificCredentialId :: Lens' UpdateServiceSpecificCredential Text
usscServiceSpecificCredentialId = lens _usscServiceSpecificCredentialId (\ s a -> s{_usscServiceSpecificCredentialId = a})

-- | The status to be assigned to the service-specific credential.
usscStatus :: Lens' UpdateServiceSpecificCredential StatusType
usscStatus = lens _usscStatus (\ s a -> s{_usscStatus = a})

instance AWSRequest UpdateServiceSpecificCredential
         where
        type Rs UpdateServiceSpecificCredential =
             UpdateServiceSpecificCredentialResponse
        request = postQuery iam
        response
          = receiveNull
              UpdateServiceSpecificCredentialResponse'

instance Hashable UpdateServiceSpecificCredential
         where

instance NFData UpdateServiceSpecificCredential where

instance ToHeaders UpdateServiceSpecificCredential
         where
        toHeaders = const mempty

instance ToPath UpdateServiceSpecificCredential where
        toPath = const "/"

instance ToQuery UpdateServiceSpecificCredential
         where
        toQuery UpdateServiceSpecificCredential'{..}
          = mconcat
              ["Action" =:
                 ("UpdateServiceSpecificCredential" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "UserName" =: _usscUserName,
               "ServiceSpecificCredentialId" =:
                 _usscServiceSpecificCredentialId,
               "Status" =: _usscStatus]

-- | /See:/ 'updateServiceSpecificCredentialResponse' smart constructor.
data UpdateServiceSpecificCredentialResponse =
  UpdateServiceSpecificCredentialResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateServiceSpecificCredentialResponse' with the minimum fields required to make a request.
--
updateServiceSpecificCredentialResponse
    :: UpdateServiceSpecificCredentialResponse
updateServiceSpecificCredentialResponse =
  UpdateServiceSpecificCredentialResponse'


instance NFData
           UpdateServiceSpecificCredentialResponse
         where
