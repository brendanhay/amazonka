{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.DeliveryOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.DeliveryOptions where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SES.Types.TLSPolicy

-- | Specifies whether messages that use the configuration set are required to use Transport Layer Security (TLS).
--
--
--
-- /See:/ 'deliveryOptions' smart constructor.
newtype DeliveryOptions = DeliveryOptions'
  { _doTLSPolicy ::
      Maybe TLSPolicy
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeliveryOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'doTLSPolicy' - Specifies whether messages that use the configuration set are required to use Transport Layer Security (TLS). If the value is @Require@ , messages are only delivered if a TLS connection can be established. If the value is @Optional@ , messages can be delivered in plain text if a TLS connection can't be established.
deliveryOptions ::
  DeliveryOptions
deliveryOptions = DeliveryOptions' {_doTLSPolicy = Nothing}

-- | Specifies whether messages that use the configuration set are required to use Transport Layer Security (TLS). If the value is @Require@ , messages are only delivered if a TLS connection can be established. If the value is @Optional@ , messages can be delivered in plain text if a TLS connection can't be established.
doTLSPolicy :: Lens' DeliveryOptions (Maybe TLSPolicy)
doTLSPolicy = lens _doTLSPolicy (\s a -> s {_doTLSPolicy = a})

instance FromXML DeliveryOptions where
  parseXML x = DeliveryOptions' <$> (x .@? "TlsPolicy")

instance Hashable DeliveryOptions

instance NFData DeliveryOptions

instance ToQuery DeliveryOptions where
  toQuery DeliveryOptions' {..} =
    mconcat ["TlsPolicy" =: _doTLSPolicy]
