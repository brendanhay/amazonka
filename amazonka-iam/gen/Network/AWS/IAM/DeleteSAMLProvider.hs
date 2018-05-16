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
-- Module      : Network.AWS.IAM.DeleteSAMLProvider
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a SAML provider resource in IAM.
--
--
-- Deleting the provider resource from IAM does not update any roles that reference the SAML provider resource's ARN as a principal in their trust policies. Any attempt to assume a role that references a non-existent provider resource ARN fails.
--
module Network.AWS.IAM.DeleteSAMLProvider
    (
    -- * Creating a Request
      deleteSAMLProvider
    , DeleteSAMLProvider
    -- * Request Lenses
    , dsamlpSAMLProviderARN

    -- * Destructuring the Response
    , deleteSAMLProviderResponse
    , DeleteSAMLProviderResponse
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteSAMLProvider' smart constructor.
newtype DeleteSAMLProvider = DeleteSAMLProvider'
  { _dsamlpSAMLProviderARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteSAMLProvider' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsamlpSAMLProviderARN' - The Amazon Resource Name (ARN) of the SAML provider to delete.
deleteSAMLProvider
    :: Text -- ^ 'dsamlpSAMLProviderARN'
    -> DeleteSAMLProvider
deleteSAMLProvider pSAMLProviderARN_ =
  DeleteSAMLProvider' {_dsamlpSAMLProviderARN = pSAMLProviderARN_}


-- | The Amazon Resource Name (ARN) of the SAML provider to delete.
dsamlpSAMLProviderARN :: Lens' DeleteSAMLProvider Text
dsamlpSAMLProviderARN = lens _dsamlpSAMLProviderARN (\ s a -> s{_dsamlpSAMLProviderARN = a})

instance AWSRequest DeleteSAMLProvider where
        type Rs DeleteSAMLProvider =
             DeleteSAMLProviderResponse
        request = postQuery iam
        response = receiveNull DeleteSAMLProviderResponse'

instance Hashable DeleteSAMLProvider where

instance NFData DeleteSAMLProvider where

instance ToHeaders DeleteSAMLProvider where
        toHeaders = const mempty

instance ToPath DeleteSAMLProvider where
        toPath = const "/"

instance ToQuery DeleteSAMLProvider where
        toQuery DeleteSAMLProvider'{..}
          = mconcat
              ["Action" =: ("DeleteSAMLProvider" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "SAMLProviderArn" =: _dsamlpSAMLProviderARN]

-- | /See:/ 'deleteSAMLProviderResponse' smart constructor.
data DeleteSAMLProviderResponse =
  DeleteSAMLProviderResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteSAMLProviderResponse' with the minimum fields required to make a request.
--
deleteSAMLProviderResponse
    :: DeleteSAMLProviderResponse
deleteSAMLProviderResponse = DeleteSAMLProviderResponse'


instance NFData DeleteSAMLProviderResponse where
