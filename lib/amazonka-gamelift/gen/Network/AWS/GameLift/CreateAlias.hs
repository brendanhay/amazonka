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
-- Module      : Network.AWS.GameLift.CreateAlias
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an alias for a fleet. In most situations, you can use an alias ID in place of a fleet ID. An alias provides a level of abstraction for a fleet that is useful when redirecting player traffic from one fleet to another, such as when updating your game build.
--
--
-- Amazon GameLift supports two types of routing strategies for aliases: simple and terminal. A simple alias points to an active fleet. A terminal alias is used to display messaging or link to a URL instead of routing players to an active fleet. For example, you might use a terminal alias when a game version is no longer supported and you want to direct players to an upgrade site.
--
-- To create a fleet alias, specify an alias name, routing strategy, and optional description. Each simple alias can point to only one fleet, but a fleet can have multiple aliases. If successful, a new alias record is returned, including an alias ID and an ARN. You can reassign an alias to another fleet by calling @UpdateAlias@ .
--
--     * 'CreateAlias'
--
--     * 'ListAliases'
--
--     * 'DescribeAlias'
--
--     * 'UpdateAlias'
--
--     * 'DeleteAlias'
--
--     * 'ResolveAlias'
module Network.AWS.GameLift.CreateAlias
  ( -- * Creating a Request
    createAlias,
    CreateAlias,

    -- * Request Lenses
    caDescription,
    caTags,
    caName,
    caRoutingStrategy,

    -- * Destructuring the Response
    createAliasResponse,
    CreateAliasResponse,

    -- * Response Lenses
    carsAlias,
    carsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input for a request operation.
--
--
--
-- /See:/ 'createAlias' smart constructor.
data CreateAlias = CreateAlias'
  { _caDescription :: !(Maybe Text),
    _caTags :: !(Maybe [Tag]),
    _caName :: !Text,
    _caRoutingStrategy :: !RoutingStrategy
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateAlias' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'caDescription' - A human-readable description of the alias.
--
-- * 'caTags' - A list of labels to assign to the new alias resource. Tags are developer-defined key-value pairs. Tagging AWS resources are useful for resource management, access management and cost allocation. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in the /AWS General Reference/ . Once the resource is created, you can use 'TagResource' , 'UntagResource' , and 'ListTagsForResource' to add, remove, and view tags. The maximum tag limit may be lower than stated. See the AWS General Reference for actual tagging limits.
--
-- * 'caName' - A descriptive label that is associated with an alias. Alias names do not need to be unique.
--
-- * 'caRoutingStrategy' - The routing configuration, including routing type and fleet target, for the alias.
createAlias ::
  -- | 'caName'
  Text ->
  -- | 'caRoutingStrategy'
  RoutingStrategy ->
  CreateAlias
createAlias pName_ pRoutingStrategy_ =
  CreateAlias'
    { _caDescription = Nothing,
      _caTags = Nothing,
      _caName = pName_,
      _caRoutingStrategy = pRoutingStrategy_
    }

-- | A human-readable description of the alias.
caDescription :: Lens' CreateAlias (Maybe Text)
caDescription = lens _caDescription (\s a -> s {_caDescription = a})

-- | A list of labels to assign to the new alias resource. Tags are developer-defined key-value pairs. Tagging AWS resources are useful for resource management, access management and cost allocation. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in the /AWS General Reference/ . Once the resource is created, you can use 'TagResource' , 'UntagResource' , and 'ListTagsForResource' to add, remove, and view tags. The maximum tag limit may be lower than stated. See the AWS General Reference for actual tagging limits.
caTags :: Lens' CreateAlias [Tag]
caTags = lens _caTags (\s a -> s {_caTags = a}) . _Default . _Coerce

-- | A descriptive label that is associated with an alias. Alias names do not need to be unique.
caName :: Lens' CreateAlias Text
caName = lens _caName (\s a -> s {_caName = a})

-- | The routing configuration, including routing type and fleet target, for the alias.
caRoutingStrategy :: Lens' CreateAlias RoutingStrategy
caRoutingStrategy = lens _caRoutingStrategy (\s a -> s {_caRoutingStrategy = a})

instance AWSRequest CreateAlias where
  type Rs CreateAlias = CreateAliasResponse
  request = postJSON gameLift
  response =
    receiveJSON
      ( \s h x ->
          CreateAliasResponse' <$> (x .?> "Alias") <*> (pure (fromEnum s))
      )

instance Hashable CreateAlias

instance NFData CreateAlias

instance ToHeaders CreateAlias where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("GameLift.CreateAlias" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateAlias where
  toJSON CreateAlias' {..} =
    object
      ( catMaybes
          [ ("Description" .=) <$> _caDescription,
            ("Tags" .=) <$> _caTags,
            Just ("Name" .= _caName),
            Just ("RoutingStrategy" .= _caRoutingStrategy)
          ]
      )

instance ToPath CreateAlias where
  toPath = const "/"

instance ToQuery CreateAlias where
  toQuery = const mempty

-- | Represents the returned data in response to a request operation.
--
--
--
-- /See:/ 'createAliasResponse' smart constructor.
data CreateAliasResponse = CreateAliasResponse'
  { _carsAlias ::
      !(Maybe Alias),
    _carsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateAliasResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'carsAlias' - The newly created alias resource.
--
-- * 'carsResponseStatus' - -- | The response status code.
createAliasResponse ::
  -- | 'carsResponseStatus'
  Int ->
  CreateAliasResponse
createAliasResponse pResponseStatus_ =
  CreateAliasResponse'
    { _carsAlias = Nothing,
      _carsResponseStatus = pResponseStatus_
    }

-- | The newly created alias resource.
carsAlias :: Lens' CreateAliasResponse (Maybe Alias)
carsAlias = lens _carsAlias (\s a -> s {_carsAlias = a})

-- | -- | The response status code.
carsResponseStatus :: Lens' CreateAliasResponse Int
carsResponseStatus = lens _carsResponseStatus (\s a -> s {_carsResponseStatus = a})

instance NFData CreateAliasResponse
