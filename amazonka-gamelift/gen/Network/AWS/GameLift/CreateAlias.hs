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
-- Module      : Network.AWS.GameLift.CreateAlias
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an alias for a fleet. In most situations, you can use an alias ID in place of a fleet ID. By using a fleet alias instead of a specific fleet ID, you can switch gameplay and players to a new fleet without changing your game client or other game components. For example, for games in production, using an alias allows you to seamlessly redirect your player base to a new game server update.
--
--
-- Amazon GameLift supports two types of routing strategies for aliases: simple and terminal. A simple alias points to an active fleet. A terminal alias is used to display messaging or link to a URL instead of routing players to an active fleet. For example, you might use a terminal alias when a game version is no longer supported and you want to direct players to an upgrade site.
--
-- To create a fleet alias, specify an alias name, routing strategy, and optional description. Each simple alias can point to only one fleet, but a fleet can have multiple aliases. If successful, a new alias record is returned, including an alias ID, which you can reference when creating a game session. You can reassign an alias to another fleet by calling @UpdateAlias@ .
--
-- Alias-related operations include:
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
--
--
--
module Network.AWS.GameLift.CreateAlias
    (
    -- * Creating a Request
      createAlias
    , CreateAlias
    -- * Request Lenses
    , caDescription
    , caName
    , caRoutingStrategy

    -- * Destructuring the Response
    , createAliasResponse
    , CreateAliasResponse
    -- * Response Lenses
    , carsAlias
    , carsResponseStatus
    ) where

import Network.AWS.GameLift.Types
import Network.AWS.GameLift.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input for a request action.
--
--
--
-- /See:/ 'createAlias' smart constructor.
data CreateAlias = CreateAlias'
  { _caDescription     :: !(Maybe Text)
  , _caName            :: !Text
  , _caRoutingStrategy :: !RoutingStrategy
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateAlias' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'caDescription' - Human-readable description of an alias.
--
-- * 'caName' - Descriptive label that is associated with an alias. Alias names do not need to be unique.
--
-- * 'caRoutingStrategy' - Object that specifies the fleet and routing type to use for the alias.
createAlias
    :: Text -- ^ 'caName'
    -> RoutingStrategy -- ^ 'caRoutingStrategy'
    -> CreateAlias
createAlias pName_ pRoutingStrategy_ =
  CreateAlias'
    { _caDescription = Nothing
    , _caName = pName_
    , _caRoutingStrategy = pRoutingStrategy_
    }


-- | Human-readable description of an alias.
caDescription :: Lens' CreateAlias (Maybe Text)
caDescription = lens _caDescription (\ s a -> s{_caDescription = a})

-- | Descriptive label that is associated with an alias. Alias names do not need to be unique.
caName :: Lens' CreateAlias Text
caName = lens _caName (\ s a -> s{_caName = a})

-- | Object that specifies the fleet and routing type to use for the alias.
caRoutingStrategy :: Lens' CreateAlias RoutingStrategy
caRoutingStrategy = lens _caRoutingStrategy (\ s a -> s{_caRoutingStrategy = a})

instance AWSRequest CreateAlias where
        type Rs CreateAlias = CreateAliasResponse
        request = postJSON gameLift
        response
          = receiveJSON
              (\ s h x ->
                 CreateAliasResponse' <$>
                   (x .?> "Alias") <*> (pure (fromEnum s)))

instance Hashable CreateAlias where

instance NFData CreateAlias where

instance ToHeaders CreateAlias where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("GameLift.CreateAlias" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateAlias where
        toJSON CreateAlias'{..}
          = object
              (catMaybes
                 [("Description" .=) <$> _caDescription,
                  Just ("Name" .= _caName),
                  Just ("RoutingStrategy" .= _caRoutingStrategy)])

instance ToPath CreateAlias where
        toPath = const "/"

instance ToQuery CreateAlias where
        toQuery = const mempty

-- | Represents the returned data in response to a request action.
--
--
--
-- /See:/ 'createAliasResponse' smart constructor.
data CreateAliasResponse = CreateAliasResponse'
  { _carsAlias          :: !(Maybe Alias)
  , _carsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateAliasResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'carsAlias' - Object that describes the newly created alias record.
--
-- * 'carsResponseStatus' - -- | The response status code.
createAliasResponse
    :: Int -- ^ 'carsResponseStatus'
    -> CreateAliasResponse
createAliasResponse pResponseStatus_ =
  CreateAliasResponse'
    {_carsAlias = Nothing, _carsResponseStatus = pResponseStatus_}


-- | Object that describes the newly created alias record.
carsAlias :: Lens' CreateAliasResponse (Maybe Alias)
carsAlias = lens _carsAlias (\ s a -> s{_carsAlias = a})

-- | -- | The response status code.
carsResponseStatus :: Lens' CreateAliasResponse Int
carsResponseStatus = lens _carsResponseStatus (\ s a -> s{_carsResponseStatus = a})

instance NFData CreateAliasResponse where
