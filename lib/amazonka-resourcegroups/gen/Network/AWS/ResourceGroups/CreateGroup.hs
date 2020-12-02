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
-- Module      : Network.AWS.ResourceGroups.CreateGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a resource group with the specified name and description. You can optionally include a resource query, or a service configuration.
module Network.AWS.ResourceGroups.CreateGroup
  ( -- * Creating a Request
    createGroup,
    CreateGroup,

    -- * Request Lenses
    cgResourceQuery,
    cgConfiguration,
    cgDescription,
    cgTags,
    cgName,

    -- * Destructuring the Response
    createGroupResponse,
    CreateGroupResponse,

    -- * Response Lenses
    cgrsGroup,
    cgrsGroupConfiguration,
    cgrsResourceQuery,
    cgrsTags,
    cgrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.ResourceGroups.Types
import Network.AWS.Response

-- | /See:/ 'createGroup' smart constructor.
data CreateGroup = CreateGroup'
  { _cgResourceQuery ::
      !(Maybe ResourceQuery),
    _cgConfiguration :: !(Maybe [GroupConfigurationItem]),
    _cgDescription :: !(Maybe Text),
    _cgTags :: !(Maybe (Map Text (Text))),
    _cgName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cgResourceQuery' - The resource query that determines which AWS resources are members of this group.
--
-- * 'cgConfiguration' - A configuration associates the resource group with an AWS service and specifies how the service can interact with the resources in the group. A configuration is an array of 'GroupConfigurationItem' elements.
--
-- * 'cgDescription' - The description of the resource group. Descriptions can consist of letters, numbers, hyphens, underscores, periods, and spaces.
--
-- * 'cgTags' - The tags to add to the group. A tag is key-value pair string.
--
-- * 'cgName' - The name of the group, which is the identifier of the group in other operations. You can't change the name of a resource group after you create it. A resource group name can consist of letters, numbers, hyphens, periods, and underscores. The name cannot start with @AWS@ or @aws@ ; these are reserved. A resource group name must be unique within each AWS Region in your AWS account.
createGroup ::
  -- | 'cgName'
  Text ->
  CreateGroup
createGroup pName_ =
  CreateGroup'
    { _cgResourceQuery = Nothing,
      _cgConfiguration = Nothing,
      _cgDescription = Nothing,
      _cgTags = Nothing,
      _cgName = pName_
    }

-- | The resource query that determines which AWS resources are members of this group.
cgResourceQuery :: Lens' CreateGroup (Maybe ResourceQuery)
cgResourceQuery = lens _cgResourceQuery (\s a -> s {_cgResourceQuery = a})

-- | A configuration associates the resource group with an AWS service and specifies how the service can interact with the resources in the group. A configuration is an array of 'GroupConfigurationItem' elements.
cgConfiguration :: Lens' CreateGroup [GroupConfigurationItem]
cgConfiguration = lens _cgConfiguration (\s a -> s {_cgConfiguration = a}) . _Default . _Coerce

-- | The description of the resource group. Descriptions can consist of letters, numbers, hyphens, underscores, periods, and spaces.
cgDescription :: Lens' CreateGroup (Maybe Text)
cgDescription = lens _cgDescription (\s a -> s {_cgDescription = a})

-- | The tags to add to the group. A tag is key-value pair string.
cgTags :: Lens' CreateGroup (HashMap Text (Text))
cgTags = lens _cgTags (\s a -> s {_cgTags = a}) . _Default . _Map

-- | The name of the group, which is the identifier of the group in other operations. You can't change the name of a resource group after you create it. A resource group name can consist of letters, numbers, hyphens, periods, and underscores. The name cannot start with @AWS@ or @aws@ ; these are reserved. A resource group name must be unique within each AWS Region in your AWS account.
cgName :: Lens' CreateGroup Text
cgName = lens _cgName (\s a -> s {_cgName = a})

instance AWSRequest CreateGroup where
  type Rs CreateGroup = CreateGroupResponse
  request = postJSON resourceGroups
  response =
    receiveJSON
      ( \s h x ->
          CreateGroupResponse'
            <$> (x .?> "Group")
            <*> (x .?> "GroupConfiguration")
            <*> (x .?> "ResourceQuery")
            <*> (x .?> "Tags" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable CreateGroup

instance NFData CreateGroup

instance ToHeaders CreateGroup where
  toHeaders = const mempty

instance ToJSON CreateGroup where
  toJSON CreateGroup' {..} =
    object
      ( catMaybes
          [ ("ResourceQuery" .=) <$> _cgResourceQuery,
            ("Configuration" .=) <$> _cgConfiguration,
            ("Description" .=) <$> _cgDescription,
            ("Tags" .=) <$> _cgTags,
            Just ("Name" .= _cgName)
          ]
      )

instance ToPath CreateGroup where
  toPath = const "/groups"

instance ToQuery CreateGroup where
  toQuery = const mempty

-- | /See:/ 'createGroupResponse' smart constructor.
data CreateGroupResponse = CreateGroupResponse'
  { _cgrsGroup ::
      !(Maybe Group),
    _cgrsGroupConfiguration ::
      !(Maybe GroupConfiguration),
    _cgrsResourceQuery :: !(Maybe ResourceQuery),
    _cgrsTags :: !(Maybe (Map Text (Text))),
    _cgrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cgrsGroup' - The description of the resource group.
--
-- * 'cgrsGroupConfiguration' - The service configuration associated with the resource group. AWS Resource Groups supports adding service configurations for the following resource group types:     * @AWS::EC2::CapacityReservationPool@ - Amazon EC2 capacity reservation pools. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/capacity-reservations-using.html#create-cr-group Working with capacity reservation groups> in the /EC2 Users Guide/ .
--
-- * 'cgrsResourceQuery' - The resource query associated with the group.
--
-- * 'cgrsTags' - The tags associated with the group.
--
-- * 'cgrsResponseStatus' - -- | The response status code.
createGroupResponse ::
  -- | 'cgrsResponseStatus'
  Int ->
  CreateGroupResponse
createGroupResponse pResponseStatus_ =
  CreateGroupResponse'
    { _cgrsGroup = Nothing,
      _cgrsGroupConfiguration = Nothing,
      _cgrsResourceQuery = Nothing,
      _cgrsTags = Nothing,
      _cgrsResponseStatus = pResponseStatus_
    }

-- | The description of the resource group.
cgrsGroup :: Lens' CreateGroupResponse (Maybe Group)
cgrsGroup = lens _cgrsGroup (\s a -> s {_cgrsGroup = a})

-- | The service configuration associated with the resource group. AWS Resource Groups supports adding service configurations for the following resource group types:     * @AWS::EC2::CapacityReservationPool@ - Amazon EC2 capacity reservation pools. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/capacity-reservations-using.html#create-cr-group Working with capacity reservation groups> in the /EC2 Users Guide/ .
cgrsGroupConfiguration :: Lens' CreateGroupResponse (Maybe GroupConfiguration)
cgrsGroupConfiguration = lens _cgrsGroupConfiguration (\s a -> s {_cgrsGroupConfiguration = a})

-- | The resource query associated with the group.
cgrsResourceQuery :: Lens' CreateGroupResponse (Maybe ResourceQuery)
cgrsResourceQuery = lens _cgrsResourceQuery (\s a -> s {_cgrsResourceQuery = a})

-- | The tags associated with the group.
cgrsTags :: Lens' CreateGroupResponse (HashMap Text (Text))
cgrsTags = lens _cgrsTags (\s a -> s {_cgrsTags = a}) . _Default . _Map

-- | -- | The response status code.
cgrsResponseStatus :: Lens' CreateGroupResponse Int
cgrsResponseStatus = lens _cgrsResponseStatus (\s a -> s {_cgrsResponseStatus = a})

instance NFData CreateGroupResponse
