{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.DeleteSAMLProvider
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes a SAML provider.
--
-- Deleting the provider does not update any roles that reference the SAML
-- provider as a principal in their trust policies. Any attempt to assume a
-- role that references a SAML provider that has been deleted will fail.
--
-- This operation requires
-- <http://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4>.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_DeleteSAMLProvider.html>
module Network.AWS.IAM.DeleteSAMLProvider
    (
    -- * Request
      DeleteSAMLProvider
    -- ** Request constructor
    , deleteSAMLProvider
    -- ** Request lenses
    , dsamlpSAMLProviderARN

    -- * Response
    , DeleteSAMLProviderResponse
    -- ** Response constructor
    , deleteSAMLProviderResponse
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteSAMLProvider' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsamlpSAMLProviderARN'
newtype DeleteSAMLProvider = DeleteSAMLProvider'
    { _dsamlpSAMLProviderARN :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteSAMLProvider' smart constructor.
deleteSAMLProvider :: Text -> DeleteSAMLProvider
deleteSAMLProvider pSAMLProviderARN =
    DeleteSAMLProvider'
    { _dsamlpSAMLProviderARN = pSAMLProviderARN
    }

-- | The Amazon Resource Name (ARN) of the SAML provider to delete.
dsamlpSAMLProviderARN :: Lens' DeleteSAMLProvider Text
dsamlpSAMLProviderARN = lens _dsamlpSAMLProviderARN (\ s a -> s{_dsamlpSAMLProviderARN = a});

instance AWSRequest DeleteSAMLProvider where
        type Sv DeleteSAMLProvider = IAM
        type Rs DeleteSAMLProvider =
             DeleteSAMLProviderResponse
        request = post
        response = receiveNull DeleteSAMLProviderResponse'

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
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteSAMLProviderResponse' smart constructor.
deleteSAMLProviderResponse :: DeleteSAMLProviderResponse
deleteSAMLProviderResponse = DeleteSAMLProviderResponse'
