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
-- Module      : Network.AWS.Redshift.CreateHSMClientCertificate
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an HSM client certificate that an Amazon Redshift cluster will use to connect to the client's HSM in order to store and retrieve the keys used to encrypt the cluster databases.
--
--
-- The command returns a public key, which you must store in the HSM. In addition to creating the HSM certificate, you must create an Amazon Redshift HSM configuration that provides a cluster the information needed to store and use encryption keys in the HSM. For more information, go to <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-HSM.html Hardware Security Modules> in the Amazon Redshift Cluster Management Guide.
--
module Network.AWS.Redshift.CreateHSMClientCertificate
    (
    -- * Creating a Request
      createHSMClientCertificate
    , CreateHSMClientCertificate
    -- * Request Lenses
    , chccTags
    , chccHSMClientCertificateIdentifier

    -- * Destructuring the Response
    , createHSMClientCertificateResponse
    , CreateHSMClientCertificateResponse
    -- * Response Lenses
    , chccrsHSMClientCertificate
    , chccrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Types
import Network.AWS.Redshift.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'createHSMClientCertificate' smart constructor.
data CreateHSMClientCertificate = CreateHSMClientCertificate'
  { _chccTags                           :: !(Maybe [Tag])
  , _chccHSMClientCertificateIdentifier :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateHSMClientCertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'chccTags' - A list of tag instances.
--
-- * 'chccHSMClientCertificateIdentifier' - The identifier to be assigned to the new HSM client certificate that the cluster will use to connect to the HSM to use the database encryption keys.
createHSMClientCertificate
    :: Text -- ^ 'chccHSMClientCertificateIdentifier'
    -> CreateHSMClientCertificate
createHSMClientCertificate pHSMClientCertificateIdentifier_ =
  CreateHSMClientCertificate'
    { _chccTags = Nothing
    , _chccHSMClientCertificateIdentifier = pHSMClientCertificateIdentifier_
    }


-- | A list of tag instances.
chccTags :: Lens' CreateHSMClientCertificate [Tag]
chccTags = lens _chccTags (\ s a -> s{_chccTags = a}) . _Default . _Coerce

-- | The identifier to be assigned to the new HSM client certificate that the cluster will use to connect to the HSM to use the database encryption keys.
chccHSMClientCertificateIdentifier :: Lens' CreateHSMClientCertificate Text
chccHSMClientCertificateIdentifier = lens _chccHSMClientCertificateIdentifier (\ s a -> s{_chccHSMClientCertificateIdentifier = a})

instance AWSRequest CreateHSMClientCertificate where
        type Rs CreateHSMClientCertificate =
             CreateHSMClientCertificateResponse
        request = postQuery redshift
        response
          = receiveXMLWrapper
              "CreateHsmClientCertificateResult"
              (\ s h x ->
                 CreateHSMClientCertificateResponse' <$>
                   (x .@? "HsmClientCertificate") <*>
                     (pure (fromEnum s)))

instance Hashable CreateHSMClientCertificate where

instance NFData CreateHSMClientCertificate where

instance ToHeaders CreateHSMClientCertificate where
        toHeaders = const mempty

instance ToPath CreateHSMClientCertificate where
        toPath = const "/"

instance ToQuery CreateHSMClientCertificate where
        toQuery CreateHSMClientCertificate'{..}
          = mconcat
              ["Action" =:
                 ("CreateHsmClientCertificate" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "Tags" =: toQuery (toQueryList "Tag" <$> _chccTags),
               "HsmClientCertificateIdentifier" =:
                 _chccHSMClientCertificateIdentifier]

-- | /See:/ 'createHSMClientCertificateResponse' smart constructor.
data CreateHSMClientCertificateResponse = CreateHSMClientCertificateResponse'
  { _chccrsHSMClientCertificate :: !(Maybe HSMClientCertificate)
  , _chccrsResponseStatus       :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateHSMClientCertificateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'chccrsHSMClientCertificate' - Undocumented member.
--
-- * 'chccrsResponseStatus' - -- | The response status code.
createHSMClientCertificateResponse
    :: Int -- ^ 'chccrsResponseStatus'
    -> CreateHSMClientCertificateResponse
createHSMClientCertificateResponse pResponseStatus_ =
  CreateHSMClientCertificateResponse'
    { _chccrsHSMClientCertificate = Nothing
    , _chccrsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
chccrsHSMClientCertificate :: Lens' CreateHSMClientCertificateResponse (Maybe HSMClientCertificate)
chccrsHSMClientCertificate = lens _chccrsHSMClientCertificate (\ s a -> s{_chccrsHSMClientCertificate = a})

-- | -- | The response status code.
chccrsResponseStatus :: Lens' CreateHSMClientCertificateResponse Int
chccrsResponseStatus = lens _chccrsResponseStatus (\ s a -> s{_chccrsResponseStatus = a})

instance NFData CreateHSMClientCertificateResponse
         where
