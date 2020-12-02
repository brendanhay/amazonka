{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.CreateProtectionGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a grouping of protected resources so they can be handled as a collective. This resource grouping improves the accuracy of detection and reduces false positives.
module Network.AWS.Shield.CreateProtectionGroup
  ( -- * Creating a Request
    createProtectionGroup,
    CreateProtectionGroup,

    -- * Request Lenses
    cpgResourceType,
    cpgMembers,
    cpgProtectionGroupId,
    cpgAggregation,
    cpgPattern,

    -- * Destructuring the Response
    createProtectionGroupResponse,
    CreateProtectionGroupResponse,

    -- * Response Lenses
    cpgrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Shield.Types

-- | /See:/ 'createProtectionGroup' smart constructor.
data CreateProtectionGroup = CreateProtectionGroup'
  { _cpgResourceType ::
      !(Maybe ProtectedResourceType),
    _cpgMembers :: !(Maybe [Text]),
    _cpgProtectionGroupId :: !Text,
    _cpgAggregation :: !ProtectionGroupAggregation,
    _cpgPattern :: !ProtectionGroupPattern
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateProtectionGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpgResourceType' - The resource type to include in the protection group. All protected resources of this type are included in the protection group. Newly protected resources of this type are automatically added to the group. You must set this when you set @Pattern@ to @BY_RESOURCE_TYPE@ and you must not set it for any other @Pattern@ setting.
--
-- * 'cpgMembers' - The Amazon Resource Names (ARNs) of the resources to include in the protection group. You must set this when you set @Pattern@ to @ARBITRARY@ and you must not set it for any other @Pattern@ setting.
--
-- * 'cpgProtectionGroupId' - The name of the protection group. You use this to identify the protection group in lists and to manage the protection group, for example to update, delete, or describe it.
--
-- * 'cpgAggregation' - Defines how AWS Shield combines resource data for the group in order to detect, mitigate, and report events.     * Sum - Use the total traffic across the group. This is a good choice for most cases. Examples include Elastic IP addresses for EC2 instances that scale manually or automatically.     * Mean - Use the average of the traffic across the group. This is a good choice for resources that share traffic uniformly. Examples include accelerators and load balancers.     * Max - Use the highest traffic from each resource. This is useful for resources that don't share traffic and for resources that share that traffic in a non-uniform way. Examples include CloudFront distributions and origin resources for CloudFront distributions.
--
-- * 'cpgPattern' - The criteria to use to choose the protected resources for inclusion in the group. You can include all resources that have protections, provide a list of resource Amazon Resource Names (ARNs), or include all resources of a specified resource type.
createProtectionGroup ::
  -- | 'cpgProtectionGroupId'
  Text ->
  -- | 'cpgAggregation'
  ProtectionGroupAggregation ->
  -- | 'cpgPattern'
  ProtectionGroupPattern ->
  CreateProtectionGroup
createProtectionGroup pProtectionGroupId_ pAggregation_ pPattern_ =
  CreateProtectionGroup'
    { _cpgResourceType = Nothing,
      _cpgMembers = Nothing,
      _cpgProtectionGroupId = pProtectionGroupId_,
      _cpgAggregation = pAggregation_,
      _cpgPattern = pPattern_
    }

-- | The resource type to include in the protection group. All protected resources of this type are included in the protection group. Newly protected resources of this type are automatically added to the group. You must set this when you set @Pattern@ to @BY_RESOURCE_TYPE@ and you must not set it for any other @Pattern@ setting.
cpgResourceType :: Lens' CreateProtectionGroup (Maybe ProtectedResourceType)
cpgResourceType = lens _cpgResourceType (\s a -> s {_cpgResourceType = a})

-- | The Amazon Resource Names (ARNs) of the resources to include in the protection group. You must set this when you set @Pattern@ to @ARBITRARY@ and you must not set it for any other @Pattern@ setting.
cpgMembers :: Lens' CreateProtectionGroup [Text]
cpgMembers = lens _cpgMembers (\s a -> s {_cpgMembers = a}) . _Default . _Coerce

-- | The name of the protection group. You use this to identify the protection group in lists and to manage the protection group, for example to update, delete, or describe it.
cpgProtectionGroupId :: Lens' CreateProtectionGroup Text
cpgProtectionGroupId = lens _cpgProtectionGroupId (\s a -> s {_cpgProtectionGroupId = a})

-- | Defines how AWS Shield combines resource data for the group in order to detect, mitigate, and report events.     * Sum - Use the total traffic across the group. This is a good choice for most cases. Examples include Elastic IP addresses for EC2 instances that scale manually or automatically.     * Mean - Use the average of the traffic across the group. This is a good choice for resources that share traffic uniformly. Examples include accelerators and load balancers.     * Max - Use the highest traffic from each resource. This is useful for resources that don't share traffic and for resources that share that traffic in a non-uniform way. Examples include CloudFront distributions and origin resources for CloudFront distributions.
cpgAggregation :: Lens' CreateProtectionGroup ProtectionGroupAggregation
cpgAggregation = lens _cpgAggregation (\s a -> s {_cpgAggregation = a})

-- | The criteria to use to choose the protected resources for inclusion in the group. You can include all resources that have protections, provide a list of resource Amazon Resource Names (ARNs), or include all resources of a specified resource type.
cpgPattern :: Lens' CreateProtectionGroup ProtectionGroupPattern
cpgPattern = lens _cpgPattern (\s a -> s {_cpgPattern = a})

instance AWSRequest CreateProtectionGroup where
  type Rs CreateProtectionGroup = CreateProtectionGroupResponse
  request = postJSON shield
  response =
    receiveEmpty
      (\s h x -> CreateProtectionGroupResponse' <$> (pure (fromEnum s)))

instance Hashable CreateProtectionGroup

instance NFData CreateProtectionGroup

instance ToHeaders CreateProtectionGroup where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSShield_20160616.CreateProtectionGroup" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateProtectionGroup where
  toJSON CreateProtectionGroup' {..} =
    object
      ( catMaybes
          [ ("ResourceType" .=) <$> _cpgResourceType,
            ("Members" .=) <$> _cpgMembers,
            Just ("ProtectionGroupId" .= _cpgProtectionGroupId),
            Just ("Aggregation" .= _cpgAggregation),
            Just ("Pattern" .= _cpgPattern)
          ]
      )

instance ToPath CreateProtectionGroup where
  toPath = const "/"

instance ToQuery CreateProtectionGroup where
  toQuery = const mempty

-- | /See:/ 'createProtectionGroupResponse' smart constructor.
newtype CreateProtectionGroupResponse = CreateProtectionGroupResponse'
  { _cpgrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateProtectionGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpgrsResponseStatus' - -- | The response status code.
createProtectionGroupResponse ::
  -- | 'cpgrsResponseStatus'
  Int ->
  CreateProtectionGroupResponse
createProtectionGroupResponse pResponseStatus_ =
  CreateProtectionGroupResponse'
    { _cpgrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
cpgrsResponseStatus :: Lens' CreateProtectionGroupResponse Int
cpgrsResponseStatus = lens _cpgrsResponseStatus (\s a -> s {_cpgrsResponseStatus = a})

instance NFData CreateProtectionGroupResponse
