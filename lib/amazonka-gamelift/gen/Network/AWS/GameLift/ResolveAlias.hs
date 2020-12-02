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
-- Module      : Network.AWS.GameLift.ResolveAlias
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the fleet ID that an alias is currently pointing to.
--
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
module Network.AWS.GameLift.ResolveAlias
  ( -- * Creating a Request
    resolveAlias,
    ResolveAlias,

    -- * Request Lenses
    raAliasId,

    -- * Destructuring the Response
    resolveAliasResponse,
    ResolveAliasResponse,

    -- * Response Lenses
    rarsFleetARN,
    rarsFleetId,
    rarsResponseStatus,
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
-- /See:/ 'resolveAlias' smart constructor.
newtype ResolveAlias = ResolveAlias' {_raAliasId :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResolveAlias' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'raAliasId' - The unique identifier of the alias that you want to retrieve a fleet ID for. You can use either the alias ID or ARN value.
resolveAlias ::
  -- | 'raAliasId'
  Text ->
  ResolveAlias
resolveAlias pAliasId_ = ResolveAlias' {_raAliasId = pAliasId_}

-- | The unique identifier of the alias that you want to retrieve a fleet ID for. You can use either the alias ID or ARN value.
raAliasId :: Lens' ResolveAlias Text
raAliasId = lens _raAliasId (\s a -> s {_raAliasId = a})

instance AWSRequest ResolveAlias where
  type Rs ResolveAlias = ResolveAliasResponse
  request = postJSON gameLift
  response =
    receiveJSON
      ( \s h x ->
          ResolveAliasResponse'
            <$> (x .?> "FleetArn") <*> (x .?> "FleetId") <*> (pure (fromEnum s))
      )

instance Hashable ResolveAlias

instance NFData ResolveAlias

instance ToHeaders ResolveAlias where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("GameLift.ResolveAlias" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ResolveAlias where
  toJSON ResolveAlias' {..} =
    object (catMaybes [Just ("AliasId" .= _raAliasId)])

instance ToPath ResolveAlias where
  toPath = const "/"

instance ToQuery ResolveAlias where
  toQuery = const mempty

-- | Represents the returned data in response to a request operation.
--
--
--
-- /See:/ 'resolveAliasResponse' smart constructor.
data ResolveAliasResponse = ResolveAliasResponse'
  { _rarsFleetARN ::
      !(Maybe Text),
    _rarsFleetId :: !(Maybe Text),
    _rarsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResolveAliasResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rarsFleetARN' - The Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) associated with the GameLift fleet resource that this alias points to.
--
-- * 'rarsFleetId' - The fleet identifier that the alias is pointing to.
--
-- * 'rarsResponseStatus' - -- | The response status code.
resolveAliasResponse ::
  -- | 'rarsResponseStatus'
  Int ->
  ResolveAliasResponse
resolveAliasResponse pResponseStatus_ =
  ResolveAliasResponse'
    { _rarsFleetARN = Nothing,
      _rarsFleetId = Nothing,
      _rarsResponseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) associated with the GameLift fleet resource that this alias points to.
rarsFleetARN :: Lens' ResolveAliasResponse (Maybe Text)
rarsFleetARN = lens _rarsFleetARN (\s a -> s {_rarsFleetARN = a})

-- | The fleet identifier that the alias is pointing to.
rarsFleetId :: Lens' ResolveAliasResponse (Maybe Text)
rarsFleetId = lens _rarsFleetId (\s a -> s {_rarsFleetId = a})

-- | -- | The response status code.
rarsResponseStatus :: Lens' ResolveAliasResponse Int
rarsResponseStatus = lens _rarsResponseStatus (\s a -> s {_rarsResponseStatus = a})

instance NFData ResolveAliasResponse
