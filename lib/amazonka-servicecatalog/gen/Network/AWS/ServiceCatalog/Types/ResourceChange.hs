{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ResourceChange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ResourceChange where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.ServiceCatalog.Types.ChangeAction
import Network.AWS.ServiceCatalog.Types.Replacement
import Network.AWS.ServiceCatalog.Types.ResourceAttribute
import Network.AWS.ServiceCatalog.Types.ResourceChangeDetail

-- | Information about a resource change that will occur when a plan is executed.
--
--
--
-- /See:/ 'resourceChange' smart constructor.
data ResourceChange = ResourceChange'
  { _rcLogicalResourceId ::
      !(Maybe Text),
    _rcPhysicalResourceId :: !(Maybe Text),
    _rcResourceType :: !(Maybe Text),
    _rcAction :: !(Maybe ChangeAction),
    _rcScope :: !(Maybe [ResourceAttribute]),
    _rcDetails :: !(Maybe [ResourceChangeDetail]),
    _rcReplacement :: !(Maybe Replacement)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResourceChange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcLogicalResourceId' - The ID of the resource, as defined in the CloudFormation template.
--
-- * 'rcPhysicalResourceId' - The ID of the resource, if it was already created.
--
-- * 'rcResourceType' - The type of resource.
--
-- * 'rcAction' - The change action.
--
-- * 'rcScope' - The change scope.
--
-- * 'rcDetails' - Information about the resource changes.
--
-- * 'rcReplacement' - If the change type is @Modify@ , indicates whether the existing resource is deleted and replaced with a new one.
resourceChange ::
  ResourceChange
resourceChange =
  ResourceChange'
    { _rcLogicalResourceId = Nothing,
      _rcPhysicalResourceId = Nothing,
      _rcResourceType = Nothing,
      _rcAction = Nothing,
      _rcScope = Nothing,
      _rcDetails = Nothing,
      _rcReplacement = Nothing
    }

-- | The ID of the resource, as defined in the CloudFormation template.
rcLogicalResourceId :: Lens' ResourceChange (Maybe Text)
rcLogicalResourceId = lens _rcLogicalResourceId (\s a -> s {_rcLogicalResourceId = a})

-- | The ID of the resource, if it was already created.
rcPhysicalResourceId :: Lens' ResourceChange (Maybe Text)
rcPhysicalResourceId = lens _rcPhysicalResourceId (\s a -> s {_rcPhysicalResourceId = a})

-- | The type of resource.
rcResourceType :: Lens' ResourceChange (Maybe Text)
rcResourceType = lens _rcResourceType (\s a -> s {_rcResourceType = a})

-- | The change action.
rcAction :: Lens' ResourceChange (Maybe ChangeAction)
rcAction = lens _rcAction (\s a -> s {_rcAction = a})

-- | The change scope.
rcScope :: Lens' ResourceChange [ResourceAttribute]
rcScope = lens _rcScope (\s a -> s {_rcScope = a}) . _Default . _Coerce

-- | Information about the resource changes.
rcDetails :: Lens' ResourceChange [ResourceChangeDetail]
rcDetails = lens _rcDetails (\s a -> s {_rcDetails = a}) . _Default . _Coerce

-- | If the change type is @Modify@ , indicates whether the existing resource is deleted and replaced with a new one.
rcReplacement :: Lens' ResourceChange (Maybe Replacement)
rcReplacement = lens _rcReplacement (\s a -> s {_rcReplacement = a})

instance FromJSON ResourceChange where
  parseJSON =
    withObject
      "ResourceChange"
      ( \x ->
          ResourceChange'
            <$> (x .:? "LogicalResourceId")
            <*> (x .:? "PhysicalResourceId")
            <*> (x .:? "ResourceType")
            <*> (x .:? "Action")
            <*> (x .:? "Scope" .!= mempty)
            <*> (x .:? "Details" .!= mempty)
            <*> (x .:? "Replacement")
      )

instance Hashable ResourceChange

instance NFData ResourceChange
