{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.FMS.Types.Product where

import Network.AWS.FMS.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Details of the resource that is not protected by the policy.
--
--
--
-- /See:/ 'complianceViolator' smart constructor.
data ComplianceViolator = ComplianceViolator'
  { _cvResourceId      :: !(Maybe Text)
  , _cvResourceType    :: !(Maybe Text)
  , _cvViolationReason :: !(Maybe ViolationReason)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ComplianceViolator' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cvResourceId' - The resource ID.
--
-- * 'cvResourceType' - The resource type. This is in the format shown in <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference> . Valid values are @AWS::ElasticLoadBalancingV2::LoadBalancer@ or @AWS::CloudFront::Distribution@ .
--
-- * 'cvViolationReason' - The reason that the resource is not protected by the policy.
complianceViolator
    :: ComplianceViolator
complianceViolator =
  ComplianceViolator'
    { _cvResourceId = Nothing
    , _cvResourceType = Nothing
    , _cvViolationReason = Nothing
    }


-- | The resource ID.
cvResourceId :: Lens' ComplianceViolator (Maybe Text)
cvResourceId = lens _cvResourceId (\ s a -> s{_cvResourceId = a})

-- | The resource type. This is in the format shown in <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference> . Valid values are @AWS::ElasticLoadBalancingV2::LoadBalancer@ or @AWS::CloudFront::Distribution@ .
cvResourceType :: Lens' ComplianceViolator (Maybe Text)
cvResourceType = lens _cvResourceType (\ s a -> s{_cvResourceType = a})

-- | The reason that the resource is not protected by the policy.
cvViolationReason :: Lens' ComplianceViolator (Maybe ViolationReason)
cvViolationReason = lens _cvViolationReason (\ s a -> s{_cvViolationReason = a})

instance FromJSON ComplianceViolator where
        parseJSON
          = withObject "ComplianceViolator"
              (\ x ->
                 ComplianceViolator' <$>
                   (x .:? "ResourceId") <*> (x .:? "ResourceType") <*>
                     (x .:? "ViolationReason"))

instance Hashable ComplianceViolator where

instance NFData ComplianceViolator where

-- | Describes the compliance status for the account. An account is considered non-compliant if it includes resources that are not protected by the specified policy.
--
--
--
-- /See:/ 'evaluationResult' smart constructor.
data EvaluationResult = EvaluationResult'
  { _erViolatorCount           :: !(Maybe Nat)
  , _erComplianceStatus        :: !(Maybe PolicyComplianceStatusType)
  , _erEvaluationLimitExceeded :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EvaluationResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'erViolatorCount' - Number of resources that are non-compliant with the specified policy. A resource is considered non-compliant if it is not associated with the specified policy.
--
-- * 'erComplianceStatus' - Describes an AWS account's compliance with the AWS Firewall Manager policy.
--
-- * 'erEvaluationLimitExceeded' - Indicates that over 100 resources are non-compliant with the AWS Firewall Manager policy.
evaluationResult
    :: EvaluationResult
evaluationResult =
  EvaluationResult'
    { _erViolatorCount = Nothing
    , _erComplianceStatus = Nothing
    , _erEvaluationLimitExceeded = Nothing
    }


-- | Number of resources that are non-compliant with the specified policy. A resource is considered non-compliant if it is not associated with the specified policy.
erViolatorCount :: Lens' EvaluationResult (Maybe Natural)
erViolatorCount = lens _erViolatorCount (\ s a -> s{_erViolatorCount = a}) . mapping _Nat

-- | Describes an AWS account's compliance with the AWS Firewall Manager policy.
erComplianceStatus :: Lens' EvaluationResult (Maybe PolicyComplianceStatusType)
erComplianceStatus = lens _erComplianceStatus (\ s a -> s{_erComplianceStatus = a})

-- | Indicates that over 100 resources are non-compliant with the AWS Firewall Manager policy.
erEvaluationLimitExceeded :: Lens' EvaluationResult (Maybe Bool)
erEvaluationLimitExceeded = lens _erEvaluationLimitExceeded (\ s a -> s{_erEvaluationLimitExceeded = a})

instance FromJSON EvaluationResult where
        parseJSON
          = withObject "EvaluationResult"
              (\ x ->
                 EvaluationResult' <$>
                   (x .:? "ViolatorCount") <*>
                     (x .:? "ComplianceStatus")
                     <*> (x .:? "EvaluationLimitExceeded"))

instance Hashable EvaluationResult where

instance NFData EvaluationResult where

-- | An AWS Firewall Manager policy.
--
--
--
-- /See:/ 'policy' smart constructor.
data Policy = Policy'
  { _pPolicyId :: !(Maybe Text)
  , _pResourceTags :: !(Maybe [ResourceTag])
  , _pPolicyUpdateToken :: !(Maybe Text)
  , _pExcludeMap :: !(Maybe (Map CustomerPolicyScopeIdType [Text]))
  , _pIncludeMap :: !(Maybe (Map CustomerPolicyScopeIdType [Text]))
  , _pPolicyName :: !Text
  , _pSecurityServicePolicyData :: !SecurityServicePolicyData
  , _pResourceType :: !Text
  , _pExcludeResourceTags :: !Bool
  , _pRemediationEnabled :: !Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Policy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pPolicyId' - The ID of the AWS Firewall Manager policy.
--
-- * 'pResourceTags' - An array of @ResourceTag@ objects.
--
-- * 'pPolicyUpdateToken' - A unique identifier for each update to the policy. When issuing a @PutPolicy@ request, the @PolicyUpdateToken@ in the request must match the @PolicyUpdateToken@ of the current policy version. To get the @PolicyUpdateToken@ of the current policy version, use a @GetPolicy@ request.
--
-- * 'pExcludeMap' - Specifies the AWS account IDs to exclude from the policy. The @IncludeMap@ values are evaluated first, with all the appropriate account IDs added to the policy. Then the accounts listed in @ExcludeMap@ are removed, resulting in the final list of accounts to add to the policy. The key to the map is @ACCOUNT@ . For example, a valid @ExcludeMap@ would be @{
