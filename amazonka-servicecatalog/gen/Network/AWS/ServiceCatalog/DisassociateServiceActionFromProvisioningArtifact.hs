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
-- Module      : Network.AWS.ServiceCatalog.DisassociateServiceActionFromProvisioningArtifact
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the specified self-service action association from the specified provisioning artifact.
--
--
module Network.AWS.ServiceCatalog.DisassociateServiceActionFromProvisioningArtifact
    (
    -- * Creating a Request
      disassociateServiceActionFromProvisioningArtifact
    , DisassociateServiceActionFromProvisioningArtifact
    -- * Request Lenses
    , dsafpaAcceptLanguage
    , dsafpaProductId
    , dsafpaProvisioningArtifactId
    , dsafpaServiceActionId

    -- * Destructuring the Response
    , disassociateServiceActionFromProvisioningArtifactResponse
    , DisassociateServiceActionFromProvisioningArtifactResponse
    -- * Response Lenses
    , dsafparsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types
import Network.AWS.ServiceCatalog.Types.Product

-- | /See:/ 'disassociateServiceActionFromProvisioningArtifact' smart constructor.
data DisassociateServiceActionFromProvisioningArtifact = DisassociateServiceActionFromProvisioningArtifact'
  { _dsafpaAcceptLanguage         :: !(Maybe Text)
  , _dsafpaProductId              :: !Text
  , _dsafpaProvisioningArtifactId :: !Text
  , _dsafpaServiceActionId        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisassociateServiceActionFromProvisioningArtifact' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsafpaAcceptLanguage' - The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
--
-- * 'dsafpaProductId' - The product identifier. For example, @prod-abcdzk7xy33qa@ .
--
-- * 'dsafpaProvisioningArtifactId' - The identifier of the provisioning artifact. For example, @pa-4abcdjnxjj6ne@ .
--
-- * 'dsafpaServiceActionId' - The self-service action identifier. For example, @act-fs7abcd89wxyz@ .
disassociateServiceActionFromProvisioningArtifact
    :: Text -- ^ 'dsafpaProductId'
    -> Text -- ^ 'dsafpaProvisioningArtifactId'
    -> Text -- ^ 'dsafpaServiceActionId'
    -> DisassociateServiceActionFromProvisioningArtifact
disassociateServiceActionFromProvisioningArtifact pProductId_ pProvisioningArtifactId_ pServiceActionId_ =
  DisassociateServiceActionFromProvisioningArtifact'
    { _dsafpaAcceptLanguage = Nothing
    , _dsafpaProductId = pProductId_
    , _dsafpaProvisioningArtifactId = pProvisioningArtifactId_
    , _dsafpaServiceActionId = pServiceActionId_
    }


-- | The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
dsafpaAcceptLanguage :: Lens' DisassociateServiceActionFromProvisioningArtifact (Maybe Text)
dsafpaAcceptLanguage = lens _dsafpaAcceptLanguage (\ s a -> s{_dsafpaAcceptLanguage = a})

-- | The product identifier. For example, @prod-abcdzk7xy33qa@ .
dsafpaProductId :: Lens' DisassociateServiceActionFromProvisioningArtifact Text
dsafpaProductId = lens _dsafpaProductId (\ s a -> s{_dsafpaProductId = a})

-- | The identifier of the provisioning artifact. For example, @pa-4abcdjnxjj6ne@ .
dsafpaProvisioningArtifactId :: Lens' DisassociateServiceActionFromProvisioningArtifact Text
dsafpaProvisioningArtifactId = lens _dsafpaProvisioningArtifactId (\ s a -> s{_dsafpaProvisioningArtifactId = a})

-- | The self-service action identifier. For example, @act-fs7abcd89wxyz@ .
dsafpaServiceActionId :: Lens' DisassociateServiceActionFromProvisioningArtifact Text
dsafpaServiceActionId = lens _dsafpaServiceActionId (\ s a -> s{_dsafpaServiceActionId = a})

instance AWSRequest
           DisassociateServiceActionFromProvisioningArtifact
         where
        type Rs
               DisassociateServiceActionFromProvisioningArtifact
             =
             DisassociateServiceActionFromProvisioningArtifactResponse
        request = postJSON serviceCatalog
        response
          = receiveEmpty
              (\ s h x ->
                 DisassociateServiceActionFromProvisioningArtifactResponse'
                   <$> (pure (fromEnum s)))

instance Hashable
           DisassociateServiceActionFromProvisioningArtifact
         where

instance NFData
           DisassociateServiceActionFromProvisioningArtifact
         where

instance ToHeaders
           DisassociateServiceActionFromProvisioningArtifact
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWS242ServiceCatalogService.DisassociateServiceActionFromProvisioningArtifact"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON
           DisassociateServiceActionFromProvisioningArtifact
         where
        toJSON
          DisassociateServiceActionFromProvisioningArtifact'{..}
          = object
              (catMaybes
                 [("AcceptLanguage" .=) <$> _dsafpaAcceptLanguage,
                  Just ("ProductId" .= _dsafpaProductId),
                  Just
                    ("ProvisioningArtifactId" .=
                       _dsafpaProvisioningArtifactId),
                  Just ("ServiceActionId" .= _dsafpaServiceActionId)])

instance ToPath
           DisassociateServiceActionFromProvisioningArtifact
         where
        toPath = const "/"

instance ToQuery
           DisassociateServiceActionFromProvisioningArtifact
         where
        toQuery = const mempty

-- | /See:/ 'disassociateServiceActionFromProvisioningArtifactResponse' smart constructor.
newtype DisassociateServiceActionFromProvisioningArtifactResponse = DisassociateServiceActionFromProvisioningArtifactResponse'
  { _dsafparsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisassociateServiceActionFromProvisioningArtifactResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsafparsResponseStatus' - -- | The response status code.
disassociateServiceActionFromProvisioningArtifactResponse
    :: Int -- ^ 'dsafparsResponseStatus'
    -> DisassociateServiceActionFromProvisioningArtifactResponse
disassociateServiceActionFromProvisioningArtifactResponse pResponseStatus_ =
  DisassociateServiceActionFromProvisioningArtifactResponse'
    {_dsafparsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dsafparsResponseStatus :: Lens' DisassociateServiceActionFromProvisioningArtifactResponse Int
dsafparsResponseStatus = lens _dsafparsResponseStatus (\ s a -> s{_dsafparsResponseStatus = a})

instance NFData
           DisassociateServiceActionFromProvisioningArtifactResponse
         where
