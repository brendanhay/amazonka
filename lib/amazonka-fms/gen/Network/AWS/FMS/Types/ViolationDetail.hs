{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.ViolationDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.ViolationDetail where

import Network.AWS.FMS.Types.ResourceViolation
import Network.AWS.FMS.Types.Tag
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Violations for a resource based on the specified AWS Firewall Manager policy and AWS account.
--
--
--
-- /See:/ 'violationDetail' smart constructor.
data ViolationDetail = ViolationDetail'
  { _vdResourceTags ::
      !(Maybe [Tag]),
    _vdResourceDescription :: !(Maybe Text),
    _vdPolicyId :: !Text,
    _vdMemberAccount :: !Text,
    _vdResourceId :: !Text,
    _vdResourceType :: !Text,
    _vdResourceViolations :: ![ResourceViolation]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ViolationDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vdResourceTags' - The @ResourceTag@ objects associated with the resource.
--
-- * 'vdResourceDescription' - Brief description for the requested resource.
--
-- * 'vdPolicyId' - The ID of the AWS Firewall Manager policy that the violation details were requested for.
--
-- * 'vdMemberAccount' - The AWS account that the violation details were requested for.
--
-- * 'vdResourceId' - The resource ID that the violation details were requested for.
--
-- * 'vdResourceType' - The resource type that the violation details were requested for.
--
-- * 'vdResourceViolations' - List of violations for the requested resource.
violationDetail ::
  -- | 'vdPolicyId'
  Text ->
  -- | 'vdMemberAccount'
  Text ->
  -- | 'vdResourceId'
  Text ->
  -- | 'vdResourceType'
  Text ->
  ViolationDetail
violationDetail
  pPolicyId_
  pMemberAccount_
  pResourceId_
  pResourceType_ =
    ViolationDetail'
      { _vdResourceTags = Nothing,
        _vdResourceDescription = Nothing,
        _vdPolicyId = pPolicyId_,
        _vdMemberAccount = pMemberAccount_,
        _vdResourceId = pResourceId_,
        _vdResourceType = pResourceType_,
        _vdResourceViolations = mempty
      }

-- | The @ResourceTag@ objects associated with the resource.
vdResourceTags :: Lens' ViolationDetail [Tag]
vdResourceTags = lens _vdResourceTags (\s a -> s {_vdResourceTags = a}) . _Default . _Coerce

-- | Brief description for the requested resource.
vdResourceDescription :: Lens' ViolationDetail (Maybe Text)
vdResourceDescription = lens _vdResourceDescription (\s a -> s {_vdResourceDescription = a})

-- | The ID of the AWS Firewall Manager policy that the violation details were requested for.
vdPolicyId :: Lens' ViolationDetail Text
vdPolicyId = lens _vdPolicyId (\s a -> s {_vdPolicyId = a})

-- | The AWS account that the violation details were requested for.
vdMemberAccount :: Lens' ViolationDetail Text
vdMemberAccount = lens _vdMemberAccount (\s a -> s {_vdMemberAccount = a})

-- | The resource ID that the violation details were requested for.
vdResourceId :: Lens' ViolationDetail Text
vdResourceId = lens _vdResourceId (\s a -> s {_vdResourceId = a})

-- | The resource type that the violation details were requested for.
vdResourceType :: Lens' ViolationDetail Text
vdResourceType = lens _vdResourceType (\s a -> s {_vdResourceType = a})

-- | List of violations for the requested resource.
vdResourceViolations :: Lens' ViolationDetail [ResourceViolation]
vdResourceViolations = lens _vdResourceViolations (\s a -> s {_vdResourceViolations = a}) . _Coerce

instance FromJSON ViolationDetail where
  parseJSON =
    withObject
      "ViolationDetail"
      ( \x ->
          ViolationDetail'
            <$> (x .:? "ResourceTags" .!= mempty)
            <*> (x .:? "ResourceDescription")
            <*> (x .: "PolicyId")
            <*> (x .: "MemberAccount")
            <*> (x .: "ResourceId")
            <*> (x .: "ResourceType")
            <*> (x .:? "ResourceViolations" .!= mempty)
      )

instance Hashable ViolationDetail

instance NFData ViolationDetail
