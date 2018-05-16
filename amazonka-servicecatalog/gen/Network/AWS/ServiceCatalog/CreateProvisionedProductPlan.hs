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
-- Module      : Network.AWS.ServiceCatalog.CreateProvisionedProductPlan
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a plan. A plan includes the list of resources to be created (when provisioning a new product) or modified (when updating a provisioned product) when the plan is executed.
--
--
-- You can create one plan per provisioned product. To create a plan for an existing provisioned product, the product status must be AVAILBLE or TAINTED.
--
-- To view the resource changes in the change set, use 'DescribeProvisionedProductPlan' . To create or modify the provisioned product, use 'ExecuteProvisionedProductPlan' .
--
module Network.AWS.ServiceCatalog.CreateProvisionedProductPlan
    (
    -- * Creating a Request
      createProvisionedProductPlan
    , CreateProvisionedProductPlan
    -- * Request Lenses
    , cpppNotificationARNs
    , cpppAcceptLanguage
    , cpppPathId
    , cpppProvisioningParameters
    , cpppTags
    , cpppPlanName
    , cpppPlanType
    , cpppProductId
    , cpppProvisionedProductName
    , cpppProvisioningArtifactId
    , cpppIdempotencyToken

    -- * Destructuring the Response
    , createProvisionedProductPlanResponse
    , CreateProvisionedProductPlanResponse
    -- * Response Lenses
    , cppprsProvisionedProductName
    , cppprsProvisionProductId
    , cppprsProvisioningArtifactId
    , cppprsPlanId
    , cppprsPlanName
    , cppprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types
import Network.AWS.ServiceCatalog.Types.Product

-- | /See:/ 'createProvisionedProductPlan' smart constructor.
data CreateProvisionedProductPlan = CreateProvisionedProductPlan'
  { _cpppNotificationARNs       :: !(Maybe [Text])
  , _cpppAcceptLanguage         :: !(Maybe Text)
  , _cpppPathId                 :: !(Maybe Text)
  , _cpppProvisioningParameters :: !(Maybe [UpdateProvisioningParameter])
  , _cpppTags                   :: !(Maybe [Tag])
  , _cpppPlanName               :: !Text
  , _cpppPlanType               :: !ProvisionedProductPlanType
  , _cpppProductId              :: !Text
  , _cpppProvisionedProductName :: !Text
  , _cpppProvisioningArtifactId :: !Text
  , _cpppIdempotencyToken       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateProvisionedProductPlan' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpppNotificationARNs' - Passed to CloudFormation. The SNS topic ARNs to which to publish stack-related events.
--
-- * 'cpppAcceptLanguage' - The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
--
-- * 'cpppPathId' - The path identifier of the product. This value is optional if the product has a default path, and required if the product has more than one path. To list the paths for a product, use 'ListLaunchPaths' .
--
-- * 'cpppProvisioningParameters' - Parameters specified by the administrator that are required for provisioning the product.
--
-- * 'cpppTags' - One or more tags.
--
-- * 'cpppPlanName' - The name of the plan.
--
-- * 'cpppPlanType' - The plan type.
--
-- * 'cpppProductId' - The product identifier.
--
-- * 'cpppProvisionedProductName' - A user-friendly name for the provisioned product. This value must be unique for the AWS account and cannot be updated after the product is provisioned.
--
-- * 'cpppProvisioningArtifactId' - The identifier of the provisioning artifact.
--
-- * 'cpppIdempotencyToken' - A unique identifier that you provide to ensure idempotency. If multiple requests differ only by the idempotency token, the same response is returned for each repeated request.
createProvisionedProductPlan
    :: Text -- ^ 'cpppPlanName'
    -> ProvisionedProductPlanType -- ^ 'cpppPlanType'
    -> Text -- ^ 'cpppProductId'
    -> Text -- ^ 'cpppProvisionedProductName'
    -> Text -- ^ 'cpppProvisioningArtifactId'
    -> Text -- ^ 'cpppIdempotencyToken'
    -> CreateProvisionedProductPlan
createProvisionedProductPlan pPlanName_ pPlanType_ pProductId_ pProvisionedProductName_ pProvisioningArtifactId_ pIdempotencyToken_ =
  CreateProvisionedProductPlan'
    { _cpppNotificationARNs = Nothing
    , _cpppAcceptLanguage = Nothing
    , _cpppPathId = Nothing
    , _cpppProvisioningParameters = Nothing
    , _cpppTags = Nothing
    , _cpppPlanName = pPlanName_
    , _cpppPlanType = pPlanType_
    , _cpppProductId = pProductId_
    , _cpppProvisionedProductName = pProvisionedProductName_
    , _cpppProvisioningArtifactId = pProvisioningArtifactId_
    , _cpppIdempotencyToken = pIdempotencyToken_
    }


-- | Passed to CloudFormation. The SNS topic ARNs to which to publish stack-related events.
cpppNotificationARNs :: Lens' CreateProvisionedProductPlan [Text]
cpppNotificationARNs = lens _cpppNotificationARNs (\ s a -> s{_cpppNotificationARNs = a}) . _Default . _Coerce

-- | The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
cpppAcceptLanguage :: Lens' CreateProvisionedProductPlan (Maybe Text)
cpppAcceptLanguage = lens _cpppAcceptLanguage (\ s a -> s{_cpppAcceptLanguage = a})

-- | The path identifier of the product. This value is optional if the product has a default path, and required if the product has more than one path. To list the paths for a product, use 'ListLaunchPaths' .
cpppPathId :: Lens' CreateProvisionedProductPlan (Maybe Text)
cpppPathId = lens _cpppPathId (\ s a -> s{_cpppPathId = a})

-- | Parameters specified by the administrator that are required for provisioning the product.
cpppProvisioningParameters :: Lens' CreateProvisionedProductPlan [UpdateProvisioningParameter]
cpppProvisioningParameters = lens _cpppProvisioningParameters (\ s a -> s{_cpppProvisioningParameters = a}) . _Default . _Coerce

-- | One or more tags.
cpppTags :: Lens' CreateProvisionedProductPlan [Tag]
cpppTags = lens _cpppTags (\ s a -> s{_cpppTags = a}) . _Default . _Coerce

-- | The name of the plan.
cpppPlanName :: Lens' CreateProvisionedProductPlan Text
cpppPlanName = lens _cpppPlanName (\ s a -> s{_cpppPlanName = a})

-- | The plan type.
cpppPlanType :: Lens' CreateProvisionedProductPlan ProvisionedProductPlanType
cpppPlanType = lens _cpppPlanType (\ s a -> s{_cpppPlanType = a})

-- | The product identifier.
cpppProductId :: Lens' CreateProvisionedProductPlan Text
cpppProductId = lens _cpppProductId (\ s a -> s{_cpppProductId = a})

-- | A user-friendly name for the provisioned product. This value must be unique for the AWS account and cannot be updated after the product is provisioned.
cpppProvisionedProductName :: Lens' CreateProvisionedProductPlan Text
cpppProvisionedProductName = lens _cpppProvisionedProductName (\ s a -> s{_cpppProvisionedProductName = a})

-- | The identifier of the provisioning artifact.
cpppProvisioningArtifactId :: Lens' CreateProvisionedProductPlan Text
cpppProvisioningArtifactId = lens _cpppProvisioningArtifactId (\ s a -> s{_cpppProvisioningArtifactId = a})

-- | A unique identifier that you provide to ensure idempotency. If multiple requests differ only by the idempotency token, the same response is returned for each repeated request.
cpppIdempotencyToken :: Lens' CreateProvisionedProductPlan Text
cpppIdempotencyToken = lens _cpppIdempotencyToken (\ s a -> s{_cpppIdempotencyToken = a})

instance AWSRequest CreateProvisionedProductPlan
         where
        type Rs CreateProvisionedProductPlan =
             CreateProvisionedProductPlanResponse
        request = postJSON serviceCatalog
        response
          = receiveJSON
              (\ s h x ->
                 CreateProvisionedProductPlanResponse' <$>
                   (x .?> "ProvisionedProductName") <*>
                     (x .?> "ProvisionProductId")
                     <*> (x .?> "ProvisioningArtifactId")
                     <*> (x .?> "PlanId")
                     <*> (x .?> "PlanName")
                     <*> (pure (fromEnum s)))

instance Hashable CreateProvisionedProductPlan where

instance NFData CreateProvisionedProductPlan where

instance ToHeaders CreateProvisionedProductPlan where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWS242ServiceCatalogService.CreateProvisionedProductPlan"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateProvisionedProductPlan where
        toJSON CreateProvisionedProductPlan'{..}
          = object
              (catMaybes
                 [("NotificationArns" .=) <$> _cpppNotificationARNs,
                  ("AcceptLanguage" .=) <$> _cpppAcceptLanguage,
                  ("PathId" .=) <$> _cpppPathId,
                  ("ProvisioningParameters" .=) <$>
                    _cpppProvisioningParameters,
                  ("Tags" .=) <$> _cpppTags,
                  Just ("PlanName" .= _cpppPlanName),
                  Just ("PlanType" .= _cpppPlanType),
                  Just ("ProductId" .= _cpppProductId),
                  Just
                    ("ProvisionedProductName" .=
                       _cpppProvisionedProductName),
                  Just
                    ("ProvisioningArtifactId" .=
                       _cpppProvisioningArtifactId),
                  Just ("IdempotencyToken" .= _cpppIdempotencyToken)])

instance ToPath CreateProvisionedProductPlan where
        toPath = const "/"

instance ToQuery CreateProvisionedProductPlan where
        toQuery = const mempty

-- | /See:/ 'createProvisionedProductPlanResponse' smart constructor.
data CreateProvisionedProductPlanResponse = CreateProvisionedProductPlanResponse'
  { _cppprsProvisionedProductName :: !(Maybe Text)
  , _cppprsProvisionProductId     :: !(Maybe Text)
  , _cppprsProvisioningArtifactId :: !(Maybe Text)
  , _cppprsPlanId                 :: !(Maybe Text)
  , _cppprsPlanName               :: !(Maybe Text)
  , _cppprsResponseStatus         :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateProvisionedProductPlanResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cppprsProvisionedProductName' - The user-friendly name of the provisioned product.
--
-- * 'cppprsProvisionProductId' - The product identifier.
--
-- * 'cppprsProvisioningArtifactId' - The identifier of the provisioning artifact.
--
-- * 'cppprsPlanId' - The plan identifier.
--
-- * 'cppprsPlanName' - The name of the plan.
--
-- * 'cppprsResponseStatus' - -- | The response status code.
createProvisionedProductPlanResponse
    :: Int -- ^ 'cppprsResponseStatus'
    -> CreateProvisionedProductPlanResponse
createProvisionedProductPlanResponse pResponseStatus_ =
  CreateProvisionedProductPlanResponse'
    { _cppprsProvisionedProductName = Nothing
    , _cppprsProvisionProductId = Nothing
    , _cppprsProvisioningArtifactId = Nothing
    , _cppprsPlanId = Nothing
    , _cppprsPlanName = Nothing
    , _cppprsResponseStatus = pResponseStatus_
    }


-- | The user-friendly name of the provisioned product.
cppprsProvisionedProductName :: Lens' CreateProvisionedProductPlanResponse (Maybe Text)
cppprsProvisionedProductName = lens _cppprsProvisionedProductName (\ s a -> s{_cppprsProvisionedProductName = a})

-- | The product identifier.
cppprsProvisionProductId :: Lens' CreateProvisionedProductPlanResponse (Maybe Text)
cppprsProvisionProductId = lens _cppprsProvisionProductId (\ s a -> s{_cppprsProvisionProductId = a})

-- | The identifier of the provisioning artifact.
cppprsProvisioningArtifactId :: Lens' CreateProvisionedProductPlanResponse (Maybe Text)
cppprsProvisioningArtifactId = lens _cppprsProvisioningArtifactId (\ s a -> s{_cppprsProvisioningArtifactId = a})

-- | The plan identifier.
cppprsPlanId :: Lens' CreateProvisionedProductPlanResponse (Maybe Text)
cppprsPlanId = lens _cppprsPlanId (\ s a -> s{_cppprsPlanId = a})

-- | The name of the plan.
cppprsPlanName :: Lens' CreateProvisionedProductPlanResponse (Maybe Text)
cppprsPlanName = lens _cppprsPlanName (\ s a -> s{_cppprsPlanName = a})

-- | -- | The response status code.
cppprsResponseStatus :: Lens' CreateProvisionedProductPlanResponse Int
cppprsResponseStatus = lens _cppprsResponseStatus (\ s a -> s{_cppprsResponseStatus = a})

instance NFData CreateProvisionedProductPlanResponse
         where
