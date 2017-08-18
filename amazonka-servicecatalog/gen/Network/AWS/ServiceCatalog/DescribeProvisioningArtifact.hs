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
-- Module      : Network.AWS.ServiceCatalog.DescribeProvisioningArtifact
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves detailed information about the specified provisioning artifact.
--
--
module Network.AWS.ServiceCatalog.DescribeProvisioningArtifact
    (
    -- * Creating a Request
      describeProvisioningArtifact
    , DescribeProvisioningArtifact
    -- * Request Lenses
    , dpaVerbose
    , dpaAcceptLanguage
    , dpaProvisioningArtifactId
    , dpaProductId

    -- * Destructuring the Response
    , describeProvisioningArtifactResponse
    , DescribeProvisioningArtifactResponse
    -- * Response Lenses
    , dpaprsStatus
    , dpaprsInfo
    , dpaprsProvisioningArtifactDetail
    , dpaprsResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.ServiceCatalog.Types
import           Network.AWS.ServiceCatalog.Types.Product

-- | /See:/ 'describeProvisioningArtifact' smart constructor.
data DescribeProvisioningArtifact = DescribeProvisioningArtifact'
    { _dpaVerbose                :: !(Maybe Bool)
    , _dpaAcceptLanguage         :: !(Maybe Text)
    , _dpaProvisioningArtifactId :: !Text
    , _dpaProductId              :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeProvisioningArtifact' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpaVerbose' - Enable a verbose level of details for the provisioning artifact.
--
-- * 'dpaAcceptLanguage' - The language code to use for this operation. Supported language codes are as follows: "en" (English) "jp" (Japanese) "zh" (Chinese) If no code is specified, "en" is used as the default.
--
-- * 'dpaProvisioningArtifactId' - The identifier of the provisioning artifact. This is sometimes referred to as the product version.
--
-- * 'dpaProductId' - The product identifier.
describeProvisioningArtifact
    :: Text -- ^ 'dpaProvisioningArtifactId'
    -> Text -- ^ 'dpaProductId'
    -> DescribeProvisioningArtifact
describeProvisioningArtifact pProvisioningArtifactId_ pProductId_ =
    DescribeProvisioningArtifact'
    { _dpaVerbose = Nothing
    , _dpaAcceptLanguage = Nothing
    , _dpaProvisioningArtifactId = pProvisioningArtifactId_
    , _dpaProductId = pProductId_
    }

-- | Enable a verbose level of details for the provisioning artifact.
dpaVerbose :: Lens' DescribeProvisioningArtifact (Maybe Bool)
dpaVerbose = lens _dpaVerbose (\ s a -> s{_dpaVerbose = a});

-- | The language code to use for this operation. Supported language codes are as follows: "en" (English) "jp" (Japanese) "zh" (Chinese) If no code is specified, "en" is used as the default.
dpaAcceptLanguage :: Lens' DescribeProvisioningArtifact (Maybe Text)
dpaAcceptLanguage = lens _dpaAcceptLanguage (\ s a -> s{_dpaAcceptLanguage = a});

-- | The identifier of the provisioning artifact. This is sometimes referred to as the product version.
dpaProvisioningArtifactId :: Lens' DescribeProvisioningArtifact Text
dpaProvisioningArtifactId = lens _dpaProvisioningArtifactId (\ s a -> s{_dpaProvisioningArtifactId = a});

-- | The product identifier.
dpaProductId :: Lens' DescribeProvisioningArtifact Text
dpaProductId = lens _dpaProductId (\ s a -> s{_dpaProductId = a});

instance AWSRequest DescribeProvisioningArtifact
         where
        type Rs DescribeProvisioningArtifact =
             DescribeProvisioningArtifactResponse
        request = postJSON serviceCatalog
        response
          = receiveJSON
              (\ s h x ->
                 DescribeProvisioningArtifactResponse' <$>
                   (x .?> "Status") <*> (x .?> "Info" .!@ mempty) <*>
                     (x .?> "ProvisioningArtifactDetail")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeProvisioningArtifact

instance NFData DescribeProvisioningArtifact

instance ToHeaders DescribeProvisioningArtifact where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWS242ServiceCatalogService.DescribeProvisioningArtifact"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeProvisioningArtifact where
        toJSON DescribeProvisioningArtifact'{..}
          = object
              (catMaybes
                 [("Verbose" .=) <$> _dpaVerbose,
                  ("AcceptLanguage" .=) <$> _dpaAcceptLanguage,
                  Just
                    ("ProvisioningArtifactId" .=
                       _dpaProvisioningArtifactId),
                  Just ("ProductId" .= _dpaProductId)])

instance ToPath DescribeProvisioningArtifact where
        toPath = const "/"

instance ToQuery DescribeProvisioningArtifact where
        toQuery = const mempty

-- | /See:/ 'describeProvisioningArtifactResponse' smart constructor.
data DescribeProvisioningArtifactResponse = DescribeProvisioningArtifactResponse'
    { _dpaprsStatus                     :: !(Maybe RequestStatus)
    , _dpaprsInfo                       :: !(Maybe (Map Text Text))
    , _dpaprsProvisioningArtifactDetail :: !(Maybe ProvisioningArtifactDetail)
    , _dpaprsResponseStatus             :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeProvisioningArtifactResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpaprsStatus' - The status of the current request.
--
-- * 'dpaprsInfo' - Additional information about the provisioning artifact.
--
-- * 'dpaprsProvisioningArtifactDetail' - Detailed provisioning artifact information.
--
-- * 'dpaprsResponseStatus' - -- | The response status code.
describeProvisioningArtifactResponse
    :: Int -- ^ 'dpaprsResponseStatus'
    -> DescribeProvisioningArtifactResponse
describeProvisioningArtifactResponse pResponseStatus_ =
    DescribeProvisioningArtifactResponse'
    { _dpaprsStatus = Nothing
    , _dpaprsInfo = Nothing
    , _dpaprsProvisioningArtifactDetail = Nothing
    , _dpaprsResponseStatus = pResponseStatus_
    }

-- | The status of the current request.
dpaprsStatus :: Lens' DescribeProvisioningArtifactResponse (Maybe RequestStatus)
dpaprsStatus = lens _dpaprsStatus (\ s a -> s{_dpaprsStatus = a});

-- | Additional information about the provisioning artifact.
dpaprsInfo :: Lens' DescribeProvisioningArtifactResponse (HashMap Text Text)
dpaprsInfo = lens _dpaprsInfo (\ s a -> s{_dpaprsInfo = a}) . _Default . _Map;

-- | Detailed provisioning artifact information.
dpaprsProvisioningArtifactDetail :: Lens' DescribeProvisioningArtifactResponse (Maybe ProvisioningArtifactDetail)
dpaprsProvisioningArtifactDetail = lens _dpaprsProvisioningArtifactDetail (\ s a -> s{_dpaprsProvisioningArtifactDetail = a});

-- | -- | The response status code.
dpaprsResponseStatus :: Lens' DescribeProvisioningArtifactResponse Int
dpaprsResponseStatus = lens _dpaprsResponseStatus (\ s a -> s{_dpaprsResponseStatus = a});

instance NFData DescribeProvisioningArtifactResponse
