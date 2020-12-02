{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.ComplianceViolator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.ComplianceViolator where

import Network.AWS.FMS.Types.ViolationReason
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Details of the resource that is not protected by the policy.
--
--
--
-- /See:/ 'complianceViolator' smart constructor.
data ComplianceViolator = ComplianceViolator'
  { _cvResourceId ::
      !(Maybe Text),
    _cvResourceType :: !(Maybe Text),
    _cvViolationReason :: !(Maybe ViolationReason)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ComplianceViolator' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cvResourceId' - The resource ID.
--
-- * 'cvResourceType' - The resource type. This is in the format shown in the <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference> . For example: @AWS::ElasticLoadBalancingV2::LoadBalancer@ , @AWS::CloudFront::Distribution@ , or @AWS::NetworkFirewall::FirewallPolicy@ .
--
-- * 'cvViolationReason' - The reason that the resource is not protected by the policy.
complianceViolator ::
  ComplianceViolator
complianceViolator =
  ComplianceViolator'
    { _cvResourceId = Nothing,
      _cvResourceType = Nothing,
      _cvViolationReason = Nothing
    }

-- | The resource ID.
cvResourceId :: Lens' ComplianceViolator (Maybe Text)
cvResourceId = lens _cvResourceId (\s a -> s {_cvResourceId = a})

-- | The resource type. This is in the format shown in the <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference> . For example: @AWS::ElasticLoadBalancingV2::LoadBalancer@ , @AWS::CloudFront::Distribution@ , or @AWS::NetworkFirewall::FirewallPolicy@ .
cvResourceType :: Lens' ComplianceViolator (Maybe Text)
cvResourceType = lens _cvResourceType (\s a -> s {_cvResourceType = a})

-- | The reason that the resource is not protected by the policy.
cvViolationReason :: Lens' ComplianceViolator (Maybe ViolationReason)
cvViolationReason = lens _cvViolationReason (\s a -> s {_cvViolationReason = a})

instance FromJSON ComplianceViolator where
  parseJSON =
    withObject
      "ComplianceViolator"
      ( \x ->
          ComplianceViolator'
            <$> (x .:? "ResourceId")
            <*> (x .:? "ResourceType")
            <*> (x .:? "ViolationReason")
      )

instance Hashable ComplianceViolator

instance NFData ComplianceViolator
