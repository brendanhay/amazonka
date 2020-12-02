{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.ResourceTargetDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.ResourceTargetDefinition where

import Network.AWS.CloudFormation.Types.RequiresRecreation
import Network.AWS.CloudFormation.Types.ResourceAttribute
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The field that AWS CloudFormation will change, such as the name of a resource's property, and whether the resource will be recreated.
--
--
--
-- /See:/ 'resourceTargetDefinition' smart constructor.
data ResourceTargetDefinition = ResourceTargetDefinition'
  { _rtdAttribute ::
      !(Maybe ResourceAttribute),
    _rtdRequiresRecreation ::
      !(Maybe RequiresRecreation),
    _rtdName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResourceTargetDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtdAttribute' - Indicates which resource attribute is triggering this update, such as a change in the resource attribute's @Metadata@ , @Properties@ , or @Tags@ .
--
-- * 'rtdRequiresRecreation' - If the @Attribute@ value is @Properties@ , indicates whether a change to this property causes the resource to be recreated. The value can be @Never@ , @Always@ , or @Conditionally@ . To determine the conditions for a @Conditionally@ recreation, see the update behavior for that <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html property> in the AWS CloudFormation User Guide.
--
-- * 'rtdName' - If the @Attribute@ value is @Properties@ , the name of the property. For all other attributes, the value is null.
resourceTargetDefinition ::
  ResourceTargetDefinition
resourceTargetDefinition =
  ResourceTargetDefinition'
    { _rtdAttribute = Nothing,
      _rtdRequiresRecreation = Nothing,
      _rtdName = Nothing
    }

-- | Indicates which resource attribute is triggering this update, such as a change in the resource attribute's @Metadata@ , @Properties@ , or @Tags@ .
rtdAttribute :: Lens' ResourceTargetDefinition (Maybe ResourceAttribute)
rtdAttribute = lens _rtdAttribute (\s a -> s {_rtdAttribute = a})

-- | If the @Attribute@ value is @Properties@ , indicates whether a change to this property causes the resource to be recreated. The value can be @Never@ , @Always@ , or @Conditionally@ . To determine the conditions for a @Conditionally@ recreation, see the update behavior for that <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html property> in the AWS CloudFormation User Guide.
rtdRequiresRecreation :: Lens' ResourceTargetDefinition (Maybe RequiresRecreation)
rtdRequiresRecreation = lens _rtdRequiresRecreation (\s a -> s {_rtdRequiresRecreation = a})

-- | If the @Attribute@ value is @Properties@ , the name of the property. For all other attributes, the value is null.
rtdName :: Lens' ResourceTargetDefinition (Maybe Text)
rtdName = lens _rtdName (\s a -> s {_rtdName = a})

instance FromXML ResourceTargetDefinition where
  parseXML x =
    ResourceTargetDefinition'
      <$> (x .@? "Attribute")
      <*> (x .@? "RequiresRecreation")
      <*> (x .@? "Name")

instance Hashable ResourceTargetDefinition

instance NFData ResourceTargetDefinition
