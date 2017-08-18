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
-- Module      : Network.AWS.ServiceCatalog.DeleteProvisioningArtifact
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified provisioning artifact. This operation does not work on a provisioning artifact associated with a product that has been shared with you, or on the last provisioning artifact associated with a product (a product must have at least one provisioning artifact).
--
--
module Network.AWS.ServiceCatalog.DeleteProvisioningArtifact
    (
    -- * Creating a Request
      deleteProvisioningArtifact
    , DeleteProvisioningArtifact
    -- * Request Lenses
    , dpapAcceptLanguage
    , dpapProductId
    , dpapProvisioningArtifactId

    -- * Destructuring the Response
    , deleteProvisioningArtifactResponse
    , DeleteProvisioningArtifactResponse
    -- * Response Lenses
    , dparsResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.ServiceCatalog.Types
import           Network.AWS.ServiceCatalog.Types.Product

-- | /See:/ 'deleteProvisioningArtifact' smart constructor.
data DeleteProvisioningArtifact = DeleteProvisioningArtifact'
    { _dpapAcceptLanguage         :: !(Maybe Text)
    , _dpapProductId              :: !Text
    , _dpapProvisioningArtifactId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteProvisioningArtifact' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpapAcceptLanguage' - The language code to use for this operation. Supported language codes are as follows: "en" (English) "jp" (Japanese) "zh" (Chinese) If no code is specified, "en" is used as the default.
--
-- * 'dpapProductId' - The product identifier.
--
-- * 'dpapProvisioningArtifactId' - The identifier of the provisioning artifact for the delete request. This is sometimes referred to as the product version.
deleteProvisioningArtifact
    :: Text -- ^ 'dpapProductId'
    -> Text -- ^ 'dpapProvisioningArtifactId'
    -> DeleteProvisioningArtifact
deleteProvisioningArtifact pProductId_ pProvisioningArtifactId_ =
    DeleteProvisioningArtifact'
    { _dpapAcceptLanguage = Nothing
    , _dpapProductId = pProductId_
    , _dpapProvisioningArtifactId = pProvisioningArtifactId_
    }

-- | The language code to use for this operation. Supported language codes are as follows: "en" (English) "jp" (Japanese) "zh" (Chinese) If no code is specified, "en" is used as the default.
dpapAcceptLanguage :: Lens' DeleteProvisioningArtifact (Maybe Text)
dpapAcceptLanguage = lens _dpapAcceptLanguage (\ s a -> s{_dpapAcceptLanguage = a});

-- | The product identifier.
dpapProductId :: Lens' DeleteProvisioningArtifact Text
dpapProductId = lens _dpapProductId (\ s a -> s{_dpapProductId = a});

-- | The identifier of the provisioning artifact for the delete request. This is sometimes referred to as the product version.
dpapProvisioningArtifactId :: Lens' DeleteProvisioningArtifact Text
dpapProvisioningArtifactId = lens _dpapProvisioningArtifactId (\ s a -> s{_dpapProvisioningArtifactId = a});

instance AWSRequest DeleteProvisioningArtifact where
        type Rs DeleteProvisioningArtifact =
             DeleteProvisioningArtifactResponse
        request = postJSON serviceCatalog
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteProvisioningArtifactResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DeleteProvisioningArtifact

instance NFData DeleteProvisioningArtifact

instance ToHeaders DeleteProvisioningArtifact where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWS242ServiceCatalogService.DeleteProvisioningArtifact"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteProvisioningArtifact where
        toJSON DeleteProvisioningArtifact'{..}
          = object
              (catMaybes
                 [("AcceptLanguage" .=) <$> _dpapAcceptLanguage,
                  Just ("ProductId" .= _dpapProductId),
                  Just
                    ("ProvisioningArtifactId" .=
                       _dpapProvisioningArtifactId)])

instance ToPath DeleteProvisioningArtifact where
        toPath = const "/"

instance ToQuery DeleteProvisioningArtifact where
        toQuery = const mempty

-- | /See:/ 'deleteProvisioningArtifactResponse' smart constructor.
newtype DeleteProvisioningArtifactResponse = DeleteProvisioningArtifactResponse'
    { _dparsResponseStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteProvisioningArtifactResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dparsResponseStatus' - -- | The response status code.
deleteProvisioningArtifactResponse
    :: Int -- ^ 'dparsResponseStatus'
    -> DeleteProvisioningArtifactResponse
deleteProvisioningArtifactResponse pResponseStatus_ =
    DeleteProvisioningArtifactResponse'
    { _dparsResponseStatus = pResponseStatus_
    }

-- | -- | The response status code.
dparsResponseStatus :: Lens' DeleteProvisioningArtifactResponse Int
dparsResponseStatus = lens _dparsResponseStatus (\ s a -> s{_dparsResponseStatus = a});

instance NFData DeleteProvisioningArtifactResponse
