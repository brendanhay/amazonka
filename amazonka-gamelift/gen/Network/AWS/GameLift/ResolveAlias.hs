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
-- Module      : Network.AWS.GameLift.ResolveAlias
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the fleet ID that a specified alias is currently pointing to.
--
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
module Network.AWS.GameLift.ResolveAlias
    (
    -- * Creating a Request
      resolveAlias
    , ResolveAlias
    -- * Request Lenses
    , raAliasId

    -- * Destructuring the Response
    , resolveAliasResponse
    , ResolveAliasResponse
    -- * Response Lenses
    , rarsFleetId
    , rarsResponseStatus
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
-- /See:/ 'resolveAlias' smart constructor.
newtype ResolveAlias = ResolveAlias'
  { _raAliasId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResolveAlias' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'raAliasId' - Unique identifier for the alias you want to resolve.
resolveAlias
    :: Text -- ^ 'raAliasId'
    -> ResolveAlias
resolveAlias pAliasId_ = ResolveAlias' {_raAliasId = pAliasId_}


-- | Unique identifier for the alias you want to resolve.
raAliasId :: Lens' ResolveAlias Text
raAliasId = lens _raAliasId (\ s a -> s{_raAliasId = a})

instance AWSRequest ResolveAlias where
        type Rs ResolveAlias = ResolveAliasResponse
        request = postJSON gameLift
        response
          = receiveJSON
              (\ s h x ->
                 ResolveAliasResponse' <$>
                   (x .?> "FleetId") <*> (pure (fromEnum s)))

instance Hashable ResolveAlias where

instance NFData ResolveAlias where

instance ToHeaders ResolveAlias where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("GameLift.ResolveAlias" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ResolveAlias where
        toJSON ResolveAlias'{..}
          = object (catMaybes [Just ("AliasId" .= _raAliasId)])

instance ToPath ResolveAlias where
        toPath = const "/"

instance ToQuery ResolveAlias where
        toQuery = const mempty

-- | Represents the returned data in response to a request action.
--
--
--
-- /See:/ 'resolveAliasResponse' smart constructor.
data ResolveAliasResponse = ResolveAliasResponse'
  { _rarsFleetId        :: !(Maybe Text)
  , _rarsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResolveAliasResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rarsFleetId' - Fleet identifier that is associated with the requested alias.
--
-- * 'rarsResponseStatus' - -- | The response status code.
resolveAliasResponse
    :: Int -- ^ 'rarsResponseStatus'
    -> ResolveAliasResponse
resolveAliasResponse pResponseStatus_ =
  ResolveAliasResponse'
    {_rarsFleetId = Nothing, _rarsResponseStatus = pResponseStatus_}


-- | Fleet identifier that is associated with the requested alias.
rarsFleetId :: Lens' ResolveAliasResponse (Maybe Text)
rarsFleetId = lens _rarsFleetId (\ s a -> s{_rarsFleetId = a})

-- | -- | The response status code.
rarsResponseStatus :: Lens' ResolveAliasResponse Int
rarsResponseStatus = lens _rarsResponseStatus (\ s a -> s{_rarsResponseStatus = a})

instance NFData ResolveAliasResponse where
