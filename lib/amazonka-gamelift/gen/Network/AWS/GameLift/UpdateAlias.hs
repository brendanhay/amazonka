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
-- Module      : Network.AWS.GameLift.UpdateAlias
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates properties for an alias. To update properties, specify the alias ID to be updated and provide the information to be changed. To reassign an alias to another fleet, provide an updated routing strategy. If successful, the updated alias record is returned.
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
module Network.AWS.GameLift.UpdateAlias
  ( -- * Creating a Request
    updateAlias,
    UpdateAlias,

    -- * Request Lenses
    uaRoutingStrategy,
    uaName,
    uaDescription,
    uaAliasId,

    -- * Destructuring the Response
    updateAliasResponse,
    UpdateAliasResponse,

    -- * Response Lenses
    uarsAlias,
    uarsResponseStatus,
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
-- /See:/ 'updateAlias' smart constructor.
data UpdateAlias = UpdateAlias'
  { _uaRoutingStrategy ::
      !(Maybe RoutingStrategy),
    _uaName :: !(Maybe Text),
    _uaDescription :: !(Maybe Text),
    _uaAliasId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateAlias' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uaRoutingStrategy' - The routing configuration, including routing type and fleet target, for the alias.
--
-- * 'uaName' - A descriptive label that is associated with an alias. Alias names do not need to be unique.
--
-- * 'uaDescription' - A human-readable description of the alias.
--
-- * 'uaAliasId' - A unique identifier for the alias that you want to update. You can use either the alias ID or ARN value.
updateAlias ::
  -- | 'uaAliasId'
  Text ->
  UpdateAlias
updateAlias pAliasId_ =
  UpdateAlias'
    { _uaRoutingStrategy = Nothing,
      _uaName = Nothing,
      _uaDescription = Nothing,
      _uaAliasId = pAliasId_
    }

-- | The routing configuration, including routing type and fleet target, for the alias.
uaRoutingStrategy :: Lens' UpdateAlias (Maybe RoutingStrategy)
uaRoutingStrategy = lens _uaRoutingStrategy (\s a -> s {_uaRoutingStrategy = a})

-- | A descriptive label that is associated with an alias. Alias names do not need to be unique.
uaName :: Lens' UpdateAlias (Maybe Text)
uaName = lens _uaName (\s a -> s {_uaName = a})

-- | A human-readable description of the alias.
uaDescription :: Lens' UpdateAlias (Maybe Text)
uaDescription = lens _uaDescription (\s a -> s {_uaDescription = a})

-- | A unique identifier for the alias that you want to update. You can use either the alias ID or ARN value.
uaAliasId :: Lens' UpdateAlias Text
uaAliasId = lens _uaAliasId (\s a -> s {_uaAliasId = a})

instance AWSRequest UpdateAlias where
  type Rs UpdateAlias = UpdateAliasResponse
  request = postJSON gameLift
  response =
    receiveJSON
      ( \s h x ->
          UpdateAliasResponse' <$> (x .?> "Alias") <*> (pure (fromEnum s))
      )

instance Hashable UpdateAlias

instance NFData UpdateAlias

instance ToHeaders UpdateAlias where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("GameLift.UpdateAlias" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UpdateAlias where
  toJSON UpdateAlias' {..} =
    object
      ( catMaybes
          [ ("RoutingStrategy" .=) <$> _uaRoutingStrategy,
            ("Name" .=) <$> _uaName,
            ("Description" .=) <$> _uaDescription,
            Just ("AliasId" .= _uaAliasId)
          ]
      )

instance ToPath UpdateAlias where
  toPath = const "/"

instance ToQuery UpdateAlias where
  toQuery = const mempty

-- | Represents the returned data in response to a request operation.
--
--
--
-- /See:/ 'updateAliasResponse' smart constructor.
data UpdateAliasResponse = UpdateAliasResponse'
  { _uarsAlias ::
      !(Maybe Alias),
    _uarsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateAliasResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uarsAlias' - The updated alias resource.
--
-- * 'uarsResponseStatus' - -- | The response status code.
updateAliasResponse ::
  -- | 'uarsResponseStatus'
  Int ->
  UpdateAliasResponse
updateAliasResponse pResponseStatus_ =
  UpdateAliasResponse'
    { _uarsAlias = Nothing,
      _uarsResponseStatus = pResponseStatus_
    }

-- | The updated alias resource.
uarsAlias :: Lens' UpdateAliasResponse (Maybe Alias)
uarsAlias = lens _uarsAlias (\s a -> s {_uarsAlias = a})

-- | -- | The response status code.
uarsResponseStatus :: Lens' UpdateAliasResponse Int
uarsResponseStatus = lens _uarsResponseStatus (\s a -> s {_uarsResponseStatus = a})

instance NFData UpdateAliasResponse
