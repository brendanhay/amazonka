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
-- Module      : Network.AWS.Connect.UpdateRoutingProfileName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the name and description of a routing profile. The request accepts the following data in JSON format. At least @Name@ or @Description@ must be provided.
module Network.AWS.Connect.UpdateRoutingProfileName
  ( -- * Creating a Request
    updateRoutingProfileName,
    UpdateRoutingProfileName,

    -- * Request Lenses
    urpnName,
    urpnDescription,
    urpnInstanceId,
    urpnRoutingProfileId,

    -- * Destructuring the Response
    updateRoutingProfileNameResponse,
    UpdateRoutingProfileNameResponse,
  )
where

import Network.AWS.Connect.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateRoutingProfileName' smart constructor.
data UpdateRoutingProfileName = UpdateRoutingProfileName'
  { _urpnName ::
      !(Maybe Text),
    _urpnDescription :: !(Maybe Text),
    _urpnInstanceId :: !Text,
    _urpnRoutingProfileId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateRoutingProfileName' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urpnName' - The name of the routing profile. Must not be more than 127 characters.
--
-- * 'urpnDescription' - The description of the routing profile. Must not be more than 250 characters.
--
-- * 'urpnInstanceId' - The identifier of the Amazon Connect instance.
--
-- * 'urpnRoutingProfileId' - The identifier of the routing profile.
updateRoutingProfileName ::
  -- | 'urpnInstanceId'
  Text ->
  -- | 'urpnRoutingProfileId'
  Text ->
  UpdateRoutingProfileName
updateRoutingProfileName pInstanceId_ pRoutingProfileId_ =
  UpdateRoutingProfileName'
    { _urpnName = Nothing,
      _urpnDescription = Nothing,
      _urpnInstanceId = pInstanceId_,
      _urpnRoutingProfileId = pRoutingProfileId_
    }

-- | The name of the routing profile. Must not be more than 127 characters.
urpnName :: Lens' UpdateRoutingProfileName (Maybe Text)
urpnName = lens _urpnName (\s a -> s {_urpnName = a})

-- | The description of the routing profile. Must not be more than 250 characters.
urpnDescription :: Lens' UpdateRoutingProfileName (Maybe Text)
urpnDescription = lens _urpnDescription (\s a -> s {_urpnDescription = a})

-- | The identifier of the Amazon Connect instance.
urpnInstanceId :: Lens' UpdateRoutingProfileName Text
urpnInstanceId = lens _urpnInstanceId (\s a -> s {_urpnInstanceId = a})

-- | The identifier of the routing profile.
urpnRoutingProfileId :: Lens' UpdateRoutingProfileName Text
urpnRoutingProfileId = lens _urpnRoutingProfileId (\s a -> s {_urpnRoutingProfileId = a})

instance AWSRequest UpdateRoutingProfileName where
  type Rs UpdateRoutingProfileName = UpdateRoutingProfileNameResponse
  request = postJSON connect
  response = receiveNull UpdateRoutingProfileNameResponse'

instance Hashable UpdateRoutingProfileName

instance NFData UpdateRoutingProfileName

instance ToHeaders UpdateRoutingProfileName where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON UpdateRoutingProfileName where
  toJSON UpdateRoutingProfileName' {..} =
    object
      ( catMaybes
          [ ("Name" .=) <$> _urpnName,
            ("Description" .=) <$> _urpnDescription
          ]
      )

instance ToPath UpdateRoutingProfileName where
  toPath UpdateRoutingProfileName' {..} =
    mconcat
      [ "/routing-profiles/",
        toBS _urpnInstanceId,
        "/",
        toBS _urpnRoutingProfileId,
        "/name"
      ]

instance ToQuery UpdateRoutingProfileName where
  toQuery = const mempty

-- | /See:/ 'updateRoutingProfileNameResponse' smart constructor.
data UpdateRoutingProfileNameResponse = UpdateRoutingProfileNameResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateRoutingProfileNameResponse' with the minimum fields required to make a request.
updateRoutingProfileNameResponse ::
  UpdateRoutingProfileNameResponse
updateRoutingProfileNameResponse =
  UpdateRoutingProfileNameResponse'

instance NFData UpdateRoutingProfileNameResponse
