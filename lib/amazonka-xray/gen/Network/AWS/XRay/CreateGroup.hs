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
-- Module      : Network.AWS.XRay.CreateGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a group resource with a name and a filter expression.
module Network.AWS.XRay.CreateGroup
  ( -- * Creating a Request
    createGroup,
    CreateGroup,

    -- * Request Lenses
    cgFilterExpression,
    cgInsightsConfiguration,
    cgTags,
    cgGroupName,

    -- * Destructuring the Response
    createGroupResponse,
    CreateGroupResponse,

    -- * Response Lenses
    cgrsGroup,
    cgrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.XRay.Types

-- | /See:/ 'createGroup' smart constructor.
data CreateGroup = CreateGroup'
  { _cgFilterExpression ::
      !(Maybe Text),
    _cgInsightsConfiguration :: !(Maybe InsightsConfiguration),
    _cgTags :: !(Maybe [Tag]),
    _cgGroupName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cgFilterExpression' - The filter expression defining criteria by which to group traces.
--
-- * 'cgInsightsConfiguration' - The structure containing configurations related to insights.     * The InsightsEnabled boolean can be set to true to enable insights for the new group or false to disable insights for the new group.     * The NotifcationsEnabled boolean can be set to true to enable insights notifications for the new group. Notifications may only be enabled on a group with InsightsEnabled set to true.
--
-- * 'cgTags' - A map that contains one or more tag keys and tag values to attach to an X-Ray group. For more information about ways to use tags, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS resources> in the /AWS General Reference/ . The following restrictions apply to tags:     * Maximum number of user-applied tags per resource: 50     * Maximum tag key length: 128 Unicode characters     * Maximum tag value length: 256 Unicode characters     * Valid values for key and value: a-z, A-Z, 0-9, space, and the following characters: _ . : / = + - and @     * Tag keys and values are case sensitive.     * Don't use @aws:@ as a prefix for keys; it's reserved for AWS use.
--
-- * 'cgGroupName' - The case-sensitive name of the new group. Default is a reserved name and names must be unique.
createGroup ::
  -- | 'cgGroupName'
  Text ->
  CreateGroup
createGroup pGroupName_ =
  CreateGroup'
    { _cgFilterExpression = Nothing,
      _cgInsightsConfiguration = Nothing,
      _cgTags = Nothing,
      _cgGroupName = pGroupName_
    }

-- | The filter expression defining criteria by which to group traces.
cgFilterExpression :: Lens' CreateGroup (Maybe Text)
cgFilterExpression = lens _cgFilterExpression (\s a -> s {_cgFilterExpression = a})

-- | The structure containing configurations related to insights.     * The InsightsEnabled boolean can be set to true to enable insights for the new group or false to disable insights for the new group.     * The NotifcationsEnabled boolean can be set to true to enable insights notifications for the new group. Notifications may only be enabled on a group with InsightsEnabled set to true.
cgInsightsConfiguration :: Lens' CreateGroup (Maybe InsightsConfiguration)
cgInsightsConfiguration = lens _cgInsightsConfiguration (\s a -> s {_cgInsightsConfiguration = a})

-- | A map that contains one or more tag keys and tag values to attach to an X-Ray group. For more information about ways to use tags, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS resources> in the /AWS General Reference/ . The following restrictions apply to tags:     * Maximum number of user-applied tags per resource: 50     * Maximum tag key length: 128 Unicode characters     * Maximum tag value length: 256 Unicode characters     * Valid values for key and value: a-z, A-Z, 0-9, space, and the following characters: _ . : / = + - and @     * Tag keys and values are case sensitive.     * Don't use @aws:@ as a prefix for keys; it's reserved for AWS use.
cgTags :: Lens' CreateGroup [Tag]
cgTags = lens _cgTags (\s a -> s {_cgTags = a}) . _Default . _Coerce

-- | The case-sensitive name of the new group. Default is a reserved name and names must be unique.
cgGroupName :: Lens' CreateGroup Text
cgGroupName = lens _cgGroupName (\s a -> s {_cgGroupName = a})

instance AWSRequest CreateGroup where
  type Rs CreateGroup = CreateGroupResponse
  request = postJSON xRay
  response =
    receiveJSON
      ( \s h x ->
          CreateGroupResponse' <$> (x .?> "Group") <*> (pure (fromEnum s))
      )

instance Hashable CreateGroup

instance NFData CreateGroup

instance ToHeaders CreateGroup where
  toHeaders = const mempty

instance ToJSON CreateGroup where
  toJSON CreateGroup' {..} =
    object
      ( catMaybes
          [ ("FilterExpression" .=) <$> _cgFilterExpression,
            ("InsightsConfiguration" .=) <$> _cgInsightsConfiguration,
            ("Tags" .=) <$> _cgTags,
            Just ("GroupName" .= _cgGroupName)
          ]
      )

instance ToPath CreateGroup where
  toPath = const "/CreateGroup"

instance ToQuery CreateGroup where
  toQuery = const mempty

-- | /See:/ 'createGroupResponse' smart constructor.
data CreateGroupResponse = CreateGroupResponse'
  { _cgrsGroup ::
      !(Maybe Group),
    _cgrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cgrsGroup' - The group that was created. Contains the name of the group that was created, the Amazon Resource Name (ARN) of the group that was generated based on the group name, the filter expression, and the insight configuration that was assigned to the group.
--
-- * 'cgrsResponseStatus' - -- | The response status code.
createGroupResponse ::
  -- | 'cgrsResponseStatus'
  Int ->
  CreateGroupResponse
createGroupResponse pResponseStatus_ =
  CreateGroupResponse'
    { _cgrsGroup = Nothing,
      _cgrsResponseStatus = pResponseStatus_
    }

-- | The group that was created. Contains the name of the group that was created, the Amazon Resource Name (ARN) of the group that was generated based on the group name, the filter expression, and the insight configuration that was assigned to the group.
cgrsGroup :: Lens' CreateGroupResponse (Maybe Group)
cgrsGroup = lens _cgrsGroup (\s a -> s {_cgrsGroup = a})

-- | -- | The response status code.
cgrsResponseStatus :: Lens' CreateGroupResponse Int
cgrsResponseStatus = lens _cgrsResponseStatus (\s a -> s {_cgrsResponseStatus = a})

instance NFData CreateGroupResponse
