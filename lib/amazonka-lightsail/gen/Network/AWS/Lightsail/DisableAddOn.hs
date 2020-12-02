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
-- Module      : Network.AWS.Lightsail.DisableAddOn
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables an add-on for an Amazon Lightsail resource. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Lightsail Dev Guide> .
module Network.AWS.Lightsail.DisableAddOn
  ( -- * Creating a Request
    disableAddOn,
    DisableAddOn,

    -- * Request Lenses
    daoAddOnType,
    daoResourceName,

    -- * Destructuring the Response
    disableAddOnResponse,
    DisableAddOnResponse,

    -- * Response Lenses
    daorsOperations,
    daorsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'disableAddOn' smart constructor.
data DisableAddOn = DisableAddOn'
  { _daoAddOnType :: !AddOnType,
    _daoResourceName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DisableAddOn' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daoAddOnType' - The add-on type to disable.
--
-- * 'daoResourceName' - The name of the source resource for which to disable the add-on.
disableAddOn ::
  -- | 'daoAddOnType'
  AddOnType ->
  -- | 'daoResourceName'
  Text ->
  DisableAddOn
disableAddOn pAddOnType_ pResourceName_ =
  DisableAddOn'
    { _daoAddOnType = pAddOnType_,
      _daoResourceName = pResourceName_
    }

-- | The add-on type to disable.
daoAddOnType :: Lens' DisableAddOn AddOnType
daoAddOnType = lens _daoAddOnType (\s a -> s {_daoAddOnType = a})

-- | The name of the source resource for which to disable the add-on.
daoResourceName :: Lens' DisableAddOn Text
daoResourceName = lens _daoResourceName (\s a -> s {_daoResourceName = a})

instance AWSRequest DisableAddOn where
  type Rs DisableAddOn = DisableAddOnResponse
  request = postJSON lightsail
  response =
    receiveJSON
      ( \s h x ->
          DisableAddOnResponse'
            <$> (x .?> "operations" .!@ mempty) <*> (pure (fromEnum s))
      )

instance Hashable DisableAddOn

instance NFData DisableAddOn

instance ToHeaders DisableAddOn where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Lightsail_20161128.DisableAddOn" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DisableAddOn where
  toJSON DisableAddOn' {..} =
    object
      ( catMaybes
          [ Just ("addOnType" .= _daoAddOnType),
            Just ("resourceName" .= _daoResourceName)
          ]
      )

instance ToPath DisableAddOn where
  toPath = const "/"

instance ToQuery DisableAddOn where
  toQuery = const mempty

-- | /See:/ 'disableAddOnResponse' smart constructor.
data DisableAddOnResponse = DisableAddOnResponse'
  { _daorsOperations ::
      !(Maybe [Operation]),
    _daorsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DisableAddOnResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daorsOperations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- * 'daorsResponseStatus' - -- | The response status code.
disableAddOnResponse ::
  -- | 'daorsResponseStatus'
  Int ->
  DisableAddOnResponse
disableAddOnResponse pResponseStatus_ =
  DisableAddOnResponse'
    { _daorsOperations = Nothing,
      _daorsResponseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
daorsOperations :: Lens' DisableAddOnResponse [Operation]
daorsOperations = lens _daorsOperations (\s a -> s {_daorsOperations = a}) . _Default . _Coerce

-- | -- | The response status code.
daorsResponseStatus :: Lens' DisableAddOnResponse Int
daorsResponseStatus = lens _daorsResponseStatus (\s a -> s {_daorsResponseStatus = a})

instance NFData DisableAddOnResponse
