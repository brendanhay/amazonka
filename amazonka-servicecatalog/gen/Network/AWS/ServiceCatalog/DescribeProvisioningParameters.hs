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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the configuration required to provision the specified product using the specified provisioning artifact.
--
--
-- If the output contains a TagOption key with an empty list of values, there is a TagOption conflict for that key. The end user cannot take action to fix the conflict, and launch is not blocked. In subsequent calls to 'ProvisionProduct' , do not include conflicted TagOption keys as tags, or this causes the error "Parameter validation failed: Missing required parameter in Tags[/N/ ]:/Value/ ". Tag the provisioned product with the value @sc-tagoption-conflict-portfolioId-productId@ .
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
    , dpprsTagOptions
    , dpprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types
import Network.AWS.ServiceCatalog.Types.Product

-- | /See:/ 'describeProvisioningParameters' smart constructor.
data DescribeProvisioningParameters = DescribeProvisioningParameters'
  { _dppsAcceptLanguage         :: !(Maybe Text)
  , _dppsPathId                 :: !(Maybe Text)
  , _dppsProductId              :: !Text
  , _dppsProvisioningArtifactId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeProvisioningParameters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dppsAcceptLanguage' - The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
--
-- * 'dppsPathId' - The path identifier of the product. This value is optional if the product has a default path, and required if the product has more than one path. To list the paths for a product, use 'ListLaunchPaths' .
--
-- * 'dppsProductId' - The product identifier.
--
-- * 'dppsProvisioningArtifactId' - The identifier of the provisioning artifact.
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


-- | The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
dppsAcceptLanguage :: Lens' DescribeProvisioningParameters (Maybe Text)
dppsAcceptLanguage = lens _dppsAcceptLanguage (\ s a -> s{_dppsAcceptLanguage = a})

-- | The path identifier of the product. This value is optional if the product has a default path, and required if the product has more than one path. To list the paths for a product, use 'ListLaunchPaths' .
dppsPathId :: Lens' DescribeProvisioningParameters (Maybe Text)
dppsPathId = lens _dppsPathId (\ s a -> s{_dppsPathId = a})

-- | The product identifier.
dppsProductId :: Lens' DescribeProvisioningParameters Text
dppsProductId = lens _dppsProductId (\ s a -> s{_dppsProductId = a})

-- | The identifier of the provisioning artifact.
dppsProvisioningArtifactId :: Lens' DescribeProvisioningParameters Text
dppsProvisioningArtifactId = lens _dppsProvisioningArtifactId (\ s a -> s{_dppsProvisioningArtifactId = a})

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
                     <*> (x .?> "TagOptions" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeProvisioningParameters
         where

instance NFData DescribeProvisioningParameters where

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
  , _dpprsUsageInstructions :: !(Maybe [UsageInstruction])
  , _dpprsConstraintSummaries :: !(Maybe [ConstraintSummary])
  , _dpprsTagOptions :: !(Maybe [TagOptionSummary])
  , _dpprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeProvisioningParametersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpprsProvisioningArtifactParameters' - Information about the parameters used to provision the product.
--
-- * 'dpprsUsageInstructions' - Any additional metadata specifically related to the provisioning of the product. For example, see the @Version@ field of the CloudFormation template.
--
-- * 'dpprsConstraintSummaries' - Information about the constraints used to provision the product.
--
-- * 'dpprsTagOptions' - Information about the TagOptions associated with the resource.
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
    , _dpprsTagOptions = Nothing
    , _dpprsResponseStatus = pResponseStatus_
    }


-- | Information about the parameters used to provision the product.
dpprsProvisioningArtifactParameters :: Lens' DescribeProvisioningParametersResponse [ProvisioningArtifactParameter]
dpprsProvisioningArtifactParameters = lens _dpprsProvisioningArtifactParameters (\ s a -> s{_dpprsProvisioningArtifactParameters = a}) . _Default . _Coerce

-- | Any additional metadata specifically related to the provisioning of the product. For example, see the @Version@ field of the CloudFormation template.
dpprsUsageInstructions :: Lens' DescribeProvisioningParametersResponse [UsageInstruction]
dpprsUsageInstructions = lens _dpprsUsageInstructions (\ s a -> s{_dpprsUsageInstructions = a}) . _Default . _Coerce

-- | Information about the constraints used to provision the product.
dpprsConstraintSummaries :: Lens' DescribeProvisioningParametersResponse [ConstraintSummary]
dpprsConstraintSummaries = lens _dpprsConstraintSummaries (\ s a -> s{_dpprsConstraintSummaries = a}) . _Default . _Coerce

-- | Information about the TagOptions associated with the resource.
dpprsTagOptions :: Lens' DescribeProvisioningParametersResponse [TagOptionSummary]
dpprsTagOptions = lens _dpprsTagOptions (\ s a -> s{_dpprsTagOptions = a}) . _Default . _Coerce

-- | -- | The response status code.
dpprsResponseStatus :: Lens' DescribeProvisioningParametersResponse Int
dpprsResponseStatus = lens _dpprsResponseStatus (\ s a -> s{_dpprsResponseStatus = a})

instance NFData
           DescribeProvisioningParametersResponse
         where
