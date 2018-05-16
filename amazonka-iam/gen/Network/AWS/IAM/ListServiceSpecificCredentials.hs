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
-- Module      : Network.AWS.IAM.ListServiceSpecificCredentials
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the service-specific credentials associated with the specified IAM user. If there are none, the operation returns an empty list. The service-specific credentials returned by this operation are used only for authenticating the IAM user to a specific service. For more information about using service-specific credentials to authenticate to an AWS service, see <http://docs.aws.amazon.com/codecommit/latest/userguide/setting-up-gc.html Set Up service-specific credentials> in the AWS CodeCommit User Guide.
--
--
module Network.AWS.IAM.ListServiceSpecificCredentials
    (
    -- * Creating a Request
      listServiceSpecificCredentials
    , ListServiceSpecificCredentials
    -- * Request Lenses
    , lsscUserName
    , lsscServiceName

    -- * Destructuring the Response
    , listServiceSpecificCredentialsResponse
    , ListServiceSpecificCredentialsResponse
    -- * Response Lenses
    , lsscrsServiceSpecificCredentials
    , lsscrsResponseStatus
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listServiceSpecificCredentials' smart constructor.
data ListServiceSpecificCredentials = ListServiceSpecificCredentials'
  { _lsscUserName    :: !(Maybe Text)
  , _lsscServiceName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListServiceSpecificCredentials' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsscUserName' - The name of the user whose service-specific credentials you want information about. If this value is not specified, then the operation assumes the user whose credentials are used to call the operation. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- * 'lsscServiceName' - Filters the returned results to only those for the specified AWS service. If not specified, then AWS returns service-specific credentials for all services.
listServiceSpecificCredentials
    :: ListServiceSpecificCredentials
listServiceSpecificCredentials =
  ListServiceSpecificCredentials'
    {_lsscUserName = Nothing, _lsscServiceName = Nothing}


-- | The name of the user whose service-specific credentials you want information about. If this value is not specified, then the operation assumes the user whose credentials are used to call the operation. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
lsscUserName :: Lens' ListServiceSpecificCredentials (Maybe Text)
lsscUserName = lens _lsscUserName (\ s a -> s{_lsscUserName = a})

-- | Filters the returned results to only those for the specified AWS service. If not specified, then AWS returns service-specific credentials for all services.
lsscServiceName :: Lens' ListServiceSpecificCredentials (Maybe Text)
lsscServiceName = lens _lsscServiceName (\ s a -> s{_lsscServiceName = a})

instance AWSRequest ListServiceSpecificCredentials
         where
        type Rs ListServiceSpecificCredentials =
             ListServiceSpecificCredentialsResponse
        request = postQuery iam
        response
          = receiveXMLWrapper
              "ListServiceSpecificCredentialsResult"
              (\ s h x ->
                 ListServiceSpecificCredentialsResponse' <$>
                   (x .@? "ServiceSpecificCredentials" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance Hashable ListServiceSpecificCredentials
         where

instance NFData ListServiceSpecificCredentials where

instance ToHeaders ListServiceSpecificCredentials
         where
        toHeaders = const mempty

instance ToPath ListServiceSpecificCredentials where
        toPath = const "/"

instance ToQuery ListServiceSpecificCredentials where
        toQuery ListServiceSpecificCredentials'{..}
          = mconcat
              ["Action" =:
                 ("ListServiceSpecificCredentials" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "UserName" =: _lsscUserName,
               "ServiceName" =: _lsscServiceName]

-- | /See:/ 'listServiceSpecificCredentialsResponse' smart constructor.
data ListServiceSpecificCredentialsResponse = ListServiceSpecificCredentialsResponse'
  { _lsscrsServiceSpecificCredentials :: !(Maybe [ServiceSpecificCredentialMetadata])
  , _lsscrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListServiceSpecificCredentialsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsscrsServiceSpecificCredentials' - A list of structures that each contain details about a service-specific credential.
--
-- * 'lsscrsResponseStatus' - -- | The response status code.
listServiceSpecificCredentialsResponse
    :: Int -- ^ 'lsscrsResponseStatus'
    -> ListServiceSpecificCredentialsResponse
listServiceSpecificCredentialsResponse pResponseStatus_ =
  ListServiceSpecificCredentialsResponse'
    { _lsscrsServiceSpecificCredentials = Nothing
    , _lsscrsResponseStatus = pResponseStatus_
    }


-- | A list of structures that each contain details about a service-specific credential.
lsscrsServiceSpecificCredentials :: Lens' ListServiceSpecificCredentialsResponse [ServiceSpecificCredentialMetadata]
lsscrsServiceSpecificCredentials = lens _lsscrsServiceSpecificCredentials (\ s a -> s{_lsscrsServiceSpecificCredentials = a}) . _Default . _Coerce

-- | -- | The response status code.
lsscrsResponseStatus :: Lens' ListServiceSpecificCredentialsResponse Int
lsscrsResponseStatus = lens _lsscrsResponseStatus (\ s a -> s{_lsscrsResponseStatus = a})

instance NFData
           ListServiceSpecificCredentialsResponse
         where
