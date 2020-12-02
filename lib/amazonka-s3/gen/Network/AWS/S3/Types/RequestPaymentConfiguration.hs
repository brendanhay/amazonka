{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.RequestPaymentConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.RequestPaymentConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.Payer

-- | Container for Payer.
--
--
--
-- /See:/ 'requestPaymentConfiguration' smart constructor.
newtype RequestPaymentConfiguration = RequestPaymentConfiguration'
  { _rpcPayer ::
      Payer
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RequestPaymentConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rpcPayer' - Specifies who pays for the download and request fees.
requestPaymentConfiguration ::
  -- | 'rpcPayer'
  Payer ->
  RequestPaymentConfiguration
requestPaymentConfiguration pPayer_ =
  RequestPaymentConfiguration' {_rpcPayer = pPayer_}

-- | Specifies who pays for the download and request fees.
rpcPayer :: Lens' RequestPaymentConfiguration Payer
rpcPayer = lens _rpcPayer (\s a -> s {_rpcPayer = a})

instance Hashable RequestPaymentConfiguration

instance NFData RequestPaymentConfiguration

instance ToXML RequestPaymentConfiguration where
  toXML RequestPaymentConfiguration' {..} =
    mconcat ["Payer" @= _rpcPayer]
