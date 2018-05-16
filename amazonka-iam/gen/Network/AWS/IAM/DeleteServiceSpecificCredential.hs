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
-- Module      : Network.AWS.IAM.DeleteServiceSpecificCredential
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified service-specific credential.
--
--
module Network.AWS.IAM.DeleteServiceSpecificCredential
    (
    -- * Creating a Request
      deleteServiceSpecificCredential
    , DeleteServiceSpecificCredential
    -- * Request Lenses
    , dsscUserName
    , dsscServiceSpecificCredentialId

    -- * Destructuring the Response
    , deleteServiceSpecificCredentialResponse
    , DeleteServiceSpecificCredentialResponse
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteServiceSpecificCredential' smart constructor.
data DeleteServiceSpecificCredential = DeleteServiceSpecificCredential'
  { _dsscUserName                    :: !(Maybe Text)
  , _dsscServiceSpecificCredentialId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteServiceSpecificCredential' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsscUserName' - The name of the IAM user associated with the service-specific credential. If this value is not specified, then the operation assumes the user whose credentials are used to call the operation. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- * 'dsscServiceSpecificCredentialId' - The unique identifier of the service-specific credential. You can get this value by calling 'ListServiceSpecificCredentials' . This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that can consist of any upper or lowercased letter or digit.
deleteServiceSpecificCredential
    :: Text -- ^ 'dsscServiceSpecificCredentialId'
    -> DeleteServiceSpecificCredential
deleteServiceSpecificCredential pServiceSpecificCredentialId_ =
  DeleteServiceSpecificCredential'
    { _dsscUserName = Nothing
    , _dsscServiceSpecificCredentialId = pServiceSpecificCredentialId_
    }


-- | The name of the IAM user associated with the service-specific credential. If this value is not specified, then the operation assumes the user whose credentials are used to call the operation. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
dsscUserName :: Lens' DeleteServiceSpecificCredential (Maybe Text)
dsscUserName = lens _dsscUserName (\ s a -> s{_dsscUserName = a})

-- | The unique identifier of the service-specific credential. You can get this value by calling 'ListServiceSpecificCredentials' . This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that can consist of any upper or lowercased letter or digit.
dsscServiceSpecificCredentialId :: Lens' DeleteServiceSpecificCredential Text
dsscServiceSpecificCredentialId = lens _dsscServiceSpecificCredentialId (\ s a -> s{_dsscServiceSpecificCredentialId = a})

instance AWSRequest DeleteServiceSpecificCredential
         where
        type Rs DeleteServiceSpecificCredential =
             DeleteServiceSpecificCredentialResponse
        request = postQuery iam
        response
          = receiveNull
              DeleteServiceSpecificCredentialResponse'

instance Hashable DeleteServiceSpecificCredential
         where

instance NFData DeleteServiceSpecificCredential where

instance ToHeaders DeleteServiceSpecificCredential
         where
        toHeaders = const mempty

instance ToPath DeleteServiceSpecificCredential where
        toPath = const "/"

instance ToQuery DeleteServiceSpecificCredential
         where
        toQuery DeleteServiceSpecificCredential'{..}
          = mconcat
              ["Action" =:
                 ("DeleteServiceSpecificCredential" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "UserName" =: _dsscUserName,
               "ServiceSpecificCredentialId" =:
                 _dsscServiceSpecificCredentialId]

-- | /See:/ 'deleteServiceSpecificCredentialResponse' smart constructor.
data DeleteServiceSpecificCredentialResponse =
  DeleteServiceSpecificCredentialResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteServiceSpecificCredentialResponse' with the minimum fields required to make a request.
--
deleteServiceSpecificCredentialResponse
    :: DeleteServiceSpecificCredentialResponse
deleteServiceSpecificCredentialResponse =
  DeleteServiceSpecificCredentialResponse'


instance NFData
           DeleteServiceSpecificCredentialResponse
         where
