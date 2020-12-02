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
-- Module      : Network.AWS.SES.PutConfigurationSetDeliveryOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or updates the delivery options for a configuration set.
module Network.AWS.SES.PutConfigurationSetDeliveryOptions
  ( -- * Creating a Request
    putConfigurationSetDeliveryOptions,
    PutConfigurationSetDeliveryOptions,

    -- * Request Lenses
    pcsdoDeliveryOptions,
    pcsdoConfigurationSetName,

    -- * Destructuring the Response
    putConfigurationSetDeliveryOptionsResponse,
    PutConfigurationSetDeliveryOptionsResponse,

    -- * Response Lenses
    pcsdorsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SES.Types

-- | A request to modify the delivery options for a configuration set.
--
--
--
-- /See:/ 'putConfigurationSetDeliveryOptions' smart constructor.
data PutConfigurationSetDeliveryOptions = PutConfigurationSetDeliveryOptions'
  { _pcsdoDeliveryOptions ::
      !( Maybe
           DeliveryOptions
       ),
    _pcsdoConfigurationSetName ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutConfigurationSetDeliveryOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pcsdoDeliveryOptions' - Specifies whether messages that use the configuration set are required to use Transport Layer Security (TLS).
--
-- * 'pcsdoConfigurationSetName' - The name of the configuration set that you want to specify the delivery options for.
putConfigurationSetDeliveryOptions ::
  -- | 'pcsdoConfigurationSetName'
  Text ->
  PutConfigurationSetDeliveryOptions
putConfigurationSetDeliveryOptions pConfigurationSetName_ =
  PutConfigurationSetDeliveryOptions'
    { _pcsdoDeliveryOptions =
        Nothing,
      _pcsdoConfigurationSetName = pConfigurationSetName_
    }

-- | Specifies whether messages that use the configuration set are required to use Transport Layer Security (TLS).
pcsdoDeliveryOptions :: Lens' PutConfigurationSetDeliveryOptions (Maybe DeliveryOptions)
pcsdoDeliveryOptions = lens _pcsdoDeliveryOptions (\s a -> s {_pcsdoDeliveryOptions = a})

-- | The name of the configuration set that you want to specify the delivery options for.
pcsdoConfigurationSetName :: Lens' PutConfigurationSetDeliveryOptions Text
pcsdoConfigurationSetName = lens _pcsdoConfigurationSetName (\s a -> s {_pcsdoConfigurationSetName = a})

instance AWSRequest PutConfigurationSetDeliveryOptions where
  type
    Rs PutConfigurationSetDeliveryOptions =
      PutConfigurationSetDeliveryOptionsResponse
  request = postQuery ses
  response =
    receiveXMLWrapper
      "PutConfigurationSetDeliveryOptionsResult"
      ( \s h x ->
          PutConfigurationSetDeliveryOptionsResponse'
            <$> (pure (fromEnum s))
      )

instance Hashable PutConfigurationSetDeliveryOptions

instance NFData PutConfigurationSetDeliveryOptions

instance ToHeaders PutConfigurationSetDeliveryOptions where
  toHeaders = const mempty

instance ToPath PutConfigurationSetDeliveryOptions where
  toPath = const "/"

instance ToQuery PutConfigurationSetDeliveryOptions where
  toQuery PutConfigurationSetDeliveryOptions' {..} =
    mconcat
      [ "Action" =: ("PutConfigurationSetDeliveryOptions" :: ByteString),
        "Version" =: ("2010-12-01" :: ByteString),
        "DeliveryOptions" =: _pcsdoDeliveryOptions,
        "ConfigurationSetName" =: _pcsdoConfigurationSetName
      ]

-- | An HTTP 200 response if the request succeeds, or an error message if the request fails.
--
--
--
-- /See:/ 'putConfigurationSetDeliveryOptionsResponse' smart constructor.
newtype PutConfigurationSetDeliveryOptionsResponse = PutConfigurationSetDeliveryOptionsResponse'
  { _pcsdorsResponseStatus ::
      Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'PutConfigurationSetDeliveryOptionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pcsdorsResponseStatus' - -- | The response status code.
putConfigurationSetDeliveryOptionsResponse ::
  -- | 'pcsdorsResponseStatus'
  Int ->
  PutConfigurationSetDeliveryOptionsResponse
putConfigurationSetDeliveryOptionsResponse pResponseStatus_ =
  PutConfigurationSetDeliveryOptionsResponse'
    { _pcsdorsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
pcsdorsResponseStatus :: Lens' PutConfigurationSetDeliveryOptionsResponse Int
pcsdorsResponseStatus = lens _pcsdorsResponseStatus (\s a -> s {_pcsdorsResponseStatus = a})

instance NFData PutConfigurationSetDeliveryOptionsResponse
