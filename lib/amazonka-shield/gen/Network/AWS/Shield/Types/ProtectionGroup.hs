{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.ProtectionGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.ProtectionGroup where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Shield.Types.ProtectedResourceType
import Network.AWS.Shield.Types.ProtectionGroupAggregation
import Network.AWS.Shield.Types.ProtectionGroupPattern

-- | A grouping of protected resources that you and AWS Shield Advanced can monitor as a collective. This resource grouping improves the accuracy of detection and reduces false positives.
--
--
--
-- /See:/ 'protectionGroup' smart constructor.
data ProtectionGroup = ProtectionGroup'
  { _pgResourceType ::
      !(Maybe ProtectedResourceType),
    _pgProtectionGroupId :: !Text,
    _pgAggregation :: !ProtectionGroupAggregation,
    _pgPattern :: !ProtectionGroupPattern,
    _pgMembers :: ![Text]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProtectionGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pgResourceType' - The resource type to include in the protection group. All protected resources of this type are included in the protection group. You must set this when you set @Pattern@ to @BY_RESOURCE_TYPE@ and you must not set it for any other @Pattern@ setting.
--
-- * 'pgProtectionGroupId' - The name of the protection group. You use this to identify the protection group in lists and to manage the protection group, for example to update, delete, or describe it.
--
-- * 'pgAggregation' - Defines how AWS Shield combines resource data for the group in order to detect, mitigate, and report events.     * Sum - Use the total traffic across the group. This is a good choice for most cases. Examples include Elastic IP addresses for EC2 instances that scale manually or automatically.     * Mean - Use the average of the traffic across the group. This is a good choice for resources that share traffic uniformly. Examples include accelerators and load balancers.     * Max - Use the highest traffic from each resource. This is useful for resources that don't share traffic and for resources that share that traffic in a non-uniform way. Examples include CloudFront distributions and origin resources for CloudFront distributions.
--
-- * 'pgPattern' - The criteria to use to choose the protected resources for inclusion in the group. You can include all resources that have protections, provide a list of resource Amazon Resource Names (ARNs), or include all resources of a specified resource type.
--
-- * 'pgMembers' - The Amazon Resource Names (ARNs) of the resources to include in the protection group. You must set this when you set @Pattern@ to @ARBITRARY@ and you must not set it for any other @Pattern@ setting.
protectionGroup ::
  -- | 'pgProtectionGroupId'
  Text ->
  -- | 'pgAggregation'
  ProtectionGroupAggregation ->
  -- | 'pgPattern'
  ProtectionGroupPattern ->
  ProtectionGroup
protectionGroup pProtectionGroupId_ pAggregation_ pPattern_ =
  ProtectionGroup'
    { _pgResourceType = Nothing,
      _pgProtectionGroupId = pProtectionGroupId_,
      _pgAggregation = pAggregation_,
      _pgPattern = pPattern_,
      _pgMembers = mempty
    }

-- | The resource type to include in the protection group. All protected resources of this type are included in the protection group. You must set this when you set @Pattern@ to @BY_RESOURCE_TYPE@ and you must not set it for any other @Pattern@ setting.
pgResourceType :: Lens' ProtectionGroup (Maybe ProtectedResourceType)
pgResourceType = lens _pgResourceType (\s a -> s {_pgResourceType = a})

-- | The name of the protection group. You use this to identify the protection group in lists and to manage the protection group, for example to update, delete, or describe it.
pgProtectionGroupId :: Lens' ProtectionGroup Text
pgProtectionGroupId = lens _pgProtectionGroupId (\s a -> s {_pgProtectionGroupId = a})

-- | Defines how AWS Shield combines resource data for the group in order to detect, mitigate, and report events.     * Sum - Use the total traffic across the group. This is a good choice for most cases. Examples include Elastic IP addresses for EC2 instances that scale manually or automatically.     * Mean - Use the average of the traffic across the group. This is a good choice for resources that share traffic uniformly. Examples include accelerators and load balancers.     * Max - Use the highest traffic from each resource. This is useful for resources that don't share traffic and for resources that share that traffic in a non-uniform way. Examples include CloudFront distributions and origin resources for CloudFront distributions.
pgAggregation :: Lens' ProtectionGroup ProtectionGroupAggregation
pgAggregation = lens _pgAggregation (\s a -> s {_pgAggregation = a})

-- | The criteria to use to choose the protected resources for inclusion in the group. You can include all resources that have protections, provide a list of resource Amazon Resource Names (ARNs), or include all resources of a specified resource type.
pgPattern :: Lens' ProtectionGroup ProtectionGroupPattern
pgPattern = lens _pgPattern (\s a -> s {_pgPattern = a})

-- | The Amazon Resource Names (ARNs) of the resources to include in the protection group. You must set this when you set @Pattern@ to @ARBITRARY@ and you must not set it for any other @Pattern@ setting.
pgMembers :: Lens' ProtectionGroup [Text]
pgMembers = lens _pgMembers (\s a -> s {_pgMembers = a}) . _Coerce

instance FromJSON ProtectionGroup where
  parseJSON =
    withObject
      "ProtectionGroup"
      ( \x ->
          ProtectionGroup'
            <$> (x .:? "ResourceType")
            <*> (x .: "ProtectionGroupId")
            <*> (x .: "Aggregation")
            <*> (x .: "Pattern")
            <*> (x .:? "Members" .!= mempty)
      )

instance Hashable ProtectionGroup

instance NFData ProtectionGroup
