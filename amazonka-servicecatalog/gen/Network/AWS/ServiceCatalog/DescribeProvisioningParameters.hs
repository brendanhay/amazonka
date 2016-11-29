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
-- Module      : Network.AWS.ServiceCatalog.DescribeProvisioningParameters
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information about parameters required to provision a specified product in a specified manner. Use this operation to obtain the list of @ProvisioningArtifactParameters@ parameters available to call the 'ProvisionProduct' operation for the specified product.
--
--
module Network.AWS.ServiceCatalog.DescribeProvisioningParameters
    (
    -- * Creating a Request
      describeProvisioningParameters
    , DescribeProvisioningParameters
    -- * Request Lenses
    , dppsAcceptLanguage
    , dppsPathId
    , dppsProductId
    , dppsProvisioningArtifactId

    -- * Destructuring the Response
    , describeProvisioningParametersResponse
    , DescribeProvisioningParametersResponse
    -- * Response Lenses
    , dpprsProvisioningArtifactParameters
    , dpprsUsageInstructions
    , dpprsConstraintSummaries
    , dpprsResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.ServiceCatalog.Types
import           Network.AWS.ServiceCatalog.Types.Product

-- | /See:/ 'describeProvisioningParameters' smart constructor.
data DescribeProvisioningParameters = DescribeProvisioningParameters'
    { _dppsAcceptLanguage         :: !(Maybe Text)
    , _dppsPathId                 :: !(Maybe Text)
    , _dppsProductId              :: !Text
    , _dppsProvisioningArtifactId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeProvisioningParameters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dppsAcceptLanguage' - The language code to use for this operation. Supported language codes are as follows: "en" (English) "jp" (Japanese) "zh" (Chinese) If no code is specified, "en" is used as the default.
--
-- * 'dppsPathId' - The identifier of the path for this product's provisioning. This value is optional if the product has a default path, and is required if there is more than one path for the specified product.
--
-- * 'dppsProductId' - The product identifier.
--
-- * 'dppsProvisioningArtifactId' - The provisioning artifact identifier for this product.
describeProvisioningParameters
    :: Text -- ^ 'dppsProductId'
    -> Text -- ^ 'dppsProvisioningArtifactId'
    -> DescribeProvisioningParameters
describeProvisioningParameters pProductId_ pProvisioningArtifactId_ =
    DescribeProvisioningParameters'
    { _dppsAcceptLanguage = Nothing
    , _dppsPathId = Nothing
    , _dppsProductId = pProductId_
    , _dppsProvisioningArtifactId = pProvisioningArtifactId_
    }

-- | The language code to use for this operation. Supported language codes are as follows: "en" (English) "jp" (Japanese) "zh" (Chinese) If no code is specified, "en" is used as the default.
dppsAcceptLanguage :: Lens' DescribeProvisioningParameters (Maybe Text)
dppsAcceptLanguage = lens _dppsAcceptLanguage (\ s a -> s{_dppsAcceptLanguage = a});

-- | The identifier of the path for this product's provisioning. This value is optional if the product has a default path, and is required if there is more than one path for the specified product.
dppsPathId :: Lens' DescribeProvisioningParameters (Maybe Text)
dppsPathId = lens _dppsPathId (\ s a -> s{_dppsPathId = a});

-- | The product identifier.
dppsProductId :: Lens' DescribeProvisioningParameters Text
dppsProductId = lens _dppsProductId (\ s a -> s{_dppsProductId = a});

-- | The provisioning artifact identifier for this product.
dppsProvisioningArtifactId :: Lens' DescribeProvisioningParameters Text
dppsProvisioningArtifactId = lens _dppsProvisioningArtifactId (\ s a -> s{_dppsProvisioningArtifactId = a});

instance AWSRequest DescribeProvisioningParameters
         where
        type Rs DescribeProvisioningParameters =
             DescribeProvisioningParametersResponse
        request = postJSON serviceCatalog
        response
          = receiveJSON
              (\ s h x ->
                 DescribeProvisioningParametersResponse' <$>
                   (x .?> "ProvisioningArtifactParameters" .!@ mempty)
                     <*> (x .?> "UsageInstructions" .!@ mempty)
                     <*> (x .?> "ConstraintSummaries" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeProvisioningParameters

instance NFData DescribeProvisioningParameters

instance ToHeaders DescribeProvisioningParameters
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWS242ServiceCatalogService.DescribeProvisioningParameters"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeProvisioningParameters where
        toJSON DescribeProvisioningParameters'{..}
          = object
              (catMaybes
                 [("AcceptLanguage" .=) <$> _dppsAcceptLanguage,
                  ("PathId" .=) <$> _dppsPathId,
                  Just ("ProductId" .= _dppsProductId),
                  Just
                    ("ProvisioningArtifactId" .=
                       _dppsProvisioningArtifactId)])

instance ToPath DescribeProvisioningParameters where
        toPath = const "/"

instance ToQuery DescribeProvisioningParameters where
        toQuery = const mempty

-- | /See:/ 'describeProvisioningParametersResponse' smart constructor.
data DescribeProvisioningParametersResponse = DescribeProvisioningParametersResponse'
    { _dpprsProvisioningArtifactParameters :: !(Maybe [ProvisioningArtifactParameter])
    , _dpprsUsageInstructions              :: !(Maybe [UsageInstruction])
    , _dpprsConstraintSummaries            :: !(Maybe [ConstraintSummary])
    , _dpprsResponseStatus                 :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeProvisioningParametersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpprsProvisioningArtifactParameters' - The list of parameters used to successfully provision the product. Each parameter includes a list of allowable values and additional metadata about each parameter.
--
-- * 'dpprsUsageInstructions' - Any additional metadata specifically related to the provisioning of the product. For example, see the @Version@ field of the CloudFormation template.
--
-- * 'dpprsConstraintSummaries' - The list of constraint summaries that apply to provisioning this product.
--
-- * 'dpprsResponseStatus' - -- | The response status code.
describeProvisioningParametersResponse
    :: Int -- ^ 'dpprsResponseStatus'
    -> DescribeProvisioningParametersResponse
describeProvisioningParametersResponse pResponseStatus_ =
    DescribeProvisioningParametersResponse'
    { _dpprsProvisioningArtifactParameters = Nothing
    , _dpprsUsageInstructions = Nothing
    , _dpprsConstraintSummaries = Nothing
    , _dpprsResponseStatus = pResponseStatus_
    }

-- | The list of parameters used to successfully provision the product. Each parameter includes a list of allowable values and additional metadata about each parameter.
dpprsProvisioningArtifactParameters :: Lens' DescribeProvisioningParametersResponse [ProvisioningArtifactParameter]
dpprsProvisioningArtifactParameters = lens _dpprsProvisioningArtifactParameters (\ s a -> s{_dpprsProvisioningArtifactParameters = a}) . _Default . _Coerce;

-- | Any additional metadata specifically related to the provisioning of the product. For example, see the @Version@ field of the CloudFormation template.
dpprsUsageInstructions :: Lens' DescribeProvisioningParametersResponse [UsageInstruction]
dpprsUsageInstructions = lens _dpprsUsageInstructions (\ s a -> s{_dpprsUsageInstructions = a}) . _Default . _Coerce;

-- | The list of constraint summaries that apply to provisioning this product.
dpprsConstraintSummaries :: Lens' DescribeProvisioningParametersResponse [ConstraintSummary]
dpprsConstraintSummaries = lens _dpprsConstraintSummaries (\ s a -> s{_dpprsConstraintSummaries = a}) . _Default . _Coerce;

-- | -- | The response status code.
dpprsResponseStatus :: Lens' DescribeProvisioningParametersResponse Int
dpprsResponseStatus = lens _dpprsResponseStatus (\ s a -> s{_dpprsResponseStatus = a});

instance NFData
         DescribeProvisioningParametersResponse
