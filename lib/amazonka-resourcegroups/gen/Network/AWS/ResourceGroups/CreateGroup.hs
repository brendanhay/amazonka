{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.CreateGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a group with a specified name, description, and resource query.
--
--
module Network.AWS.ResourceGroups.CreateGroup
    (
    -- * Creating a Request
      createGroup
    , CreateGroup
    -- * Request Lenses
    , cgDescription
    , cgTags
    , cgName
    , cgResourceQuery

    -- * Destructuring the Response
    , createGroupResponse
    , CreateGroupResponse
    -- * Response Lenses
    , cgrsGroup
    , cgrsResourceQuery
    , cgrsTags
    , cgrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.ResourceGroups.Types
import Network.AWS.ResourceGroups.Types.Product
import Network.AWS.Response

-- | /See:/ 'createGroup' smart constructor.
data CreateGroup = CreateGroup'
  { _cgDescription   :: !(Maybe Text)
  , _cgTags          :: !(Maybe (Map Text Text))
  , _cgName          :: !Text
  , _cgResourceQuery :: !ResourceQuery
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cgDescription' - The description of the resource group. Descriptions can have a maximum of 511 characters, including letters, numbers, hyphens, underscores, punctuation, and spaces.
--
-- * 'cgTags' - The tags to add to the group. A tag is a string-to-string map of key-value pairs. Tag keys can have a maximum character length of 127 characters, and tag values can have a maximum length of 255 characters.
--
-- * 'cgName' - The name of the group, which is the identifier of the group in other operations. A resource group name cannot be updated after it is created. A resource group name can have a maximum of 127 characters, including letters, numbers, hyphens, dots, and underscores. The name cannot start with @AWS@ or @aws@ ; these are reserved. A resource group name must be unique within your account.
--
-- * 'cgResourceQuery' - The resource query that determines which AWS resources are members of this group.
createGroup
    :: Text -- ^ 'cgName'
    -> ResourceQuery -- ^ 'cgResourceQuery'
    -> CreateGroup
createGroup pName_ pResourceQuery_ =
  CreateGroup'
    { _cgDescription = Nothing
    , _cgTags = Nothing
    , _cgName = pName_
    , _cgResourceQuery = pResourceQuery_
    }


-- | The description of the resource group. Descriptions can have a maximum of 511 characters, including letters, numbers, hyphens, underscores, punctuation, and spaces.
cgDescription :: Lens' CreateGroup (Maybe Text)
cgDescription = lens _cgDescription (\ s a -> s{_cgDescription = a})

-- | The tags to add to the group. A tag is a string-to-string map of key-value pairs. Tag keys can have a maximum character length of 127 characters, and tag values can have a maximum length of 255 characters.
cgTags :: Lens' CreateGroup (HashMap Text Text)
cgTags = lens _cgTags (\ s a -> s{_cgTags = a}) . _Default . _Map

-- | The name of the group, which is the identifier of the group in other operations. A resource group name cannot be updated after it is created. A resource group name can have a maximum of 127 characters, including letters, numbers, hyphens, dots, and underscores. The name cannot start with @AWS@ or @aws@ ; these are reserved. A resource group name must be unique within your account.
cgName :: Lens' CreateGroup Text
cgName = lens _cgName (\ s a -> s{_cgName = a})

-- | The resource query that determines which AWS resources are members of this group.
cgResourceQuery :: Lens' CreateGroup ResourceQuery
cgResourceQuery = lens _cgResourceQuery (\ s a -> s{_cgResourceQuery = a})

instance AWSRequest CreateGroup where
        type Rs CreateGroup = CreateGroupResponse
        request = postJSON resourceGroups
        response
          = receiveJSON
              (\ s h x ->
                 CreateGroupResponse' <$>
                   (x .?> "Group") <*> (x .?> "ResourceQuery") <*>
                     (x .?> "Tags" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable CreateGroup where

instance NFData CreateGroup where

instance ToHeaders CreateGroup where
        toHeaders = const mempty

instance ToJSON CreateGroup where
        toJSON CreateGroup'{..}
          = object
              (catMaybes
                 [("Description" .=) <$> _cgDescription,
                  ("Tags" .=) <$> _cgTags, Just ("Name" .= _cgName),
                  Just ("ResourceQuery" .= _cgResourceQuery)])

instance ToPath CreateGroup where
        toPath = const "/groups"

instance ToQuery CreateGroup where
        toQuery = const mempty

-- | /See:/ 'createGroupResponse' smart constructor.
data CreateGroupResponse = CreateGroupResponse'
  { _cgrsGroup          :: !(Maybe Group)
  , _cgrsResourceQuery  :: !(Maybe ResourceQuery)
  , _cgrsTags           :: !(Maybe (Map Text Text))
  , _cgrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cgrsGroup' - A full description of the resource group after it is created.
--
-- * 'cgrsResourceQuery' - The resource query associated with the group.
--
-- * 'cgrsTags' - The tags associated with the group.
--
-- * 'cgrsResponseStatus' - -- | The response status code.
createGroupResponse
    :: Int -- ^ 'cgrsResponseStatus'
    -> CreateGroupResponse
createGroupResponse pResponseStatus_ =
  CreateGroupResponse'
    { _cgrsGroup = Nothing
    , _cgrsResourceQuery = Nothing
    , _cgrsTags = Nothing
    , _cgrsResponseStatus = pResponseStatus_
    }


-- | A full description of the resource group after it is created.
cgrsGroup :: Lens' CreateGroupResponse (Maybe Group)
cgrsGroup = lens _cgrsGroup (\ s a -> s{_cgrsGroup = a})

-- | The resource query associated with the group.
cgrsResourceQuery :: Lens' CreateGroupResponse (Maybe ResourceQuery)
cgrsResourceQuery = lens _cgrsResourceQuery (\ s a -> s{_cgrsResourceQuery = a})

-- | The tags associated with the group.
cgrsTags :: Lens' CreateGroupResponse (HashMap Text Text)
cgrsTags = lens _cgrsTags (\ s a -> s{_cgrsTags = a}) . _Default . _Map

-- | -- | The response status code.
cgrsResponseStatus :: Lens' CreateGroupResponse Int
cgrsResponseStatus = lens _cgrsResponseStatus (\ s a -> s{_cgrsResponseStatus = a})

instance NFData CreateGroupResponse where
