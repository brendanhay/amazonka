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
-- Module      : Network.AWS.Shield.UpdateProtectionGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing protection group. A protection group is a grouping of protected resources so they can be handled as a collective. This resource grouping improves the accuracy of detection and reduces false positives.
module Network.AWS.Shield.UpdateProtectionGroup
  ( -- * Creating a Request
    updateProtectionGroup,
    UpdateProtectionGroup,

    -- * Request Lenses
    upgResourceType,
    upgMembers,
    upgProtectionGroupId,
    upgAggregation,
    upgPattern,

    -- * Destructuring the Response
    updateProtectionGroupResponse,
    UpdateProtectionGroupResponse,

    -- * Response Lenses
    upgrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Shield.Types

-- | /See:/ 'updateProtectionGroup' smart constructor.
data UpdateProtectionGroup = UpdateProtectionGroup'
  { _upgResourceType ::
      !(Maybe ProtectedResourceType),
    _upgMembers :: !(Maybe [Text]),
    _upgProtectionGroupId :: !Text,
    _upgAggregation :: !ProtectionGroupAggregation,
    _upgPattern :: !ProtectionGroupPattern
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateProtectionGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'upgResourceType' - The resource type to include in the protection group. All protected resources of this type are included in the protection group. You must set this when you set @Pattern@ to @BY_RESOURCE_TYPE@ and you must not set it for any other @Pattern@ setting.
--
-- * 'upgMembers' - The Amazon Resource Names (ARNs) of the resources to include in the protection group. You must set this when you set @Pattern@ to @ARBITRARY@ and you must not set it for any other @Pattern@ setting.
--
-- * 'upgProtectionGroupId' - The name of the protection group. You use this to identify the protection group in lists and to manage the protection group, for example to update, delete, or describe it.
--
-- * 'upgAggregation' - Defines how AWS Shield combines resource data for the group in order to detect, mitigate, and report events.     * Sum - Use the total traffic across the group. This is a good choice for most cases. Examples include Elastic IP addresses for EC2 instances that scale manually or automatically.     * Mean - Use the average of the traffic across the group. This is a good choice for resources that share traffic uniformly. Examples include accelerators and load balancers.     * Max - Use the highest traffic from each resource. This is useful for resources that don't share traffic and for resources that share that traffic in a non-uniform way. Examples include CloudFront distributions and origin resources for CloudFront distributions.
--
-- * 'upgPattern' - The criteria to use to choose the protected resources for inclusion in the group. You can include all resources that have protections, provide a list of resource Amazon Resource Names (ARNs), or include all resources of a specified resource type.
updateProtectionGroup ::
  -- | 'upgProtectionGroupId'
  Text ->
  -- | 'upgAggregation'
  ProtectionGroupAggregation ->
  -- | 'upgPattern'
  ProtectionGroupPattern ->
  UpdateProtectionGroup
updateProtectionGroup pProtectionGroupId_ pAggregation_ pPattern_ =
  UpdateProtectionGroup'
    { _upgResourceType = Nothing,
      _upgMembers = Nothing,
      _upgProtectionGroupId = pProtectionGroupId_,
      _upgAggregation = pAggregation_,
      _upgPattern = pPattern_
    }

-- | The resource type to include in the protection group. All protected resources of this type are included in the protection group. You must set this when you set @Pattern@ to @BY_RESOURCE_TYPE@ and you must not set it for any other @Pattern@ setting.
upgResourceType :: Lens' UpdateProtectionGroup (Maybe ProtectedResourceType)
upgResourceType = lens _upgResourceType (\s a -> s {_upgResourceType = a})

-- | The Amazon Resource Names (ARNs) of the resources to include in the protection group. You must set this when you set @Pattern@ to @ARBITRARY@ and you must not set it for any other @Pattern@ setting.
upgMembers :: Lens' UpdateProtectionGroup [Text]
upgMembers = lens _upgMembers (\s a -> s {_upgMembers = a}) . _Default . _Coerce

-- | The name of the protection group. You use this to identify the protection group in lists and to manage the protection group, for example to update, delete, or describe it.
upgProtectionGroupId :: Lens' UpdateProtectionGroup Text
upgProtectionGroupId = lens _upgProtectionGroupId (\s a -> s {_upgProtectionGroupId = a})

-- | Defines how AWS Shield combines resource data for the group in order to detect, mitigate, and report events.     * Sum - Use the total traffic across the group. This is a good choice for most cases. Examples include Elastic IP addresses for EC2 instances that scale manually or automatically.     * Mean - Use the average of the traffic across the group. This is a good choice for resources that share traffic uniformly. Examples include accelerators and load balancers.     * Max - Use the highest traffic from each resource. This is useful for resources that don't share traffic and for resources that share that traffic in a non-uniform way. Examples include CloudFront distributions and origin resources for CloudFront distributions.
upgAggregation :: Lens' UpdateProtectionGroup ProtectionGroupAggregation
upgAggregation = lens _upgAggregation (\s a -> s {_upgAggregation = a})

-- | The criteria to use to choose the protected resources for inclusion in the group. You can include all resources that have protections, provide a list of resource Amazon Resource Names (ARNs), or include all resources of a specified resource type.
upgPattern :: Lens' UpdateProtectionGroup ProtectionGroupPattern
upgPattern = lens _upgPattern (\s a -> s {_upgPattern = a})

instance AWSRequest UpdateProtectionGroup where
  type Rs UpdateProtectionGroup = UpdateProtectionGroupResponse
  request = postJSON shield
  response =
    receiveEmpty
      (\s h x -> UpdateProtectionGroupResponse' <$> (pure (fromEnum s)))

instance Hashable UpdateProtectionGroup

instance NFData UpdateProtectionGroup

instance ToHeaders UpdateProtectionGroup where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSShield_20160616.UpdateProtectionGroup" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UpdateProtectionGroup where
  toJSON UpdateProtectionGroup' {..} =
    object
      ( catMaybes
          [ ("ResourceType" .=) <$> _upgResourceType,
            ("Members" .=) <$> _upgMembers,
            Just ("ProtectionGroupId" .= _upgProtectionGroupId),
            Just ("Aggregation" .= _upgAggregation),
            Just ("Pattern" .= _upgPattern)
          ]
      )

instance ToPath UpdateProtectionGroup where
  toPath = const "/"

instance ToQuery UpdateProtectionGroup where
  toQuery = const mempty

-- | /See:/ 'updateProtectionGroupResponse' smart constructor.
newtype UpdateProtectionGroupResponse = UpdateProtectionGroupResponse'
  { _upgrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateProtectionGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'upgrsResponseStatus' - -- | The response status code.
updateProtectionGroupResponse ::
  -- | 'upgrsResponseStatus'
  Int ->
  UpdateProtectionGroupResponse
updateProtectionGroupResponse pResponseStatus_ =
  UpdateProtectionGroupResponse'
    { _upgrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
upgrsResponseStatus :: Lens' UpdateProtectionGroupResponse Int
upgrsResponseStatus = lens _upgrsResponseStatus (\s a -> s {_upgrsResponseStatus = a})

instance NFData UpdateProtectionGroupResponse
