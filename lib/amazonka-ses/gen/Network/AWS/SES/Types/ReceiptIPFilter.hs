{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.ReceiptIPFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.ReceiptIPFilter where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SES.Types.ReceiptFilterPolicy

-- | A receipt IP address filter enables you to specify whether to accept or reject mail originating from an IP address or range of IP addresses.
--
--
-- For information about setting up IP address filters, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-ip-filters.html Amazon SES Developer Guide> .
--
--
-- /See:/ 'receiptIPFilter' smart constructor.
data ReceiptIPFilter = ReceiptIPFilter'
  { _rifPolicy ::
      !ReceiptFilterPolicy,
    _rifCidr :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReceiptIPFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rifPolicy' - Indicates whether to block or allow incoming mail from the specified IP addresses.
--
-- * 'rifCidr' - A single IP address or a range of IP addresses that you want to block or allow, specified in Classless Inter-Domain Routing (CIDR) notation. An example of a single email address is 10.0.0.1. An example of a range of IP addresses is 10.0.0.1/24. For more information about CIDR notation, see <https://tools.ietf.org/html/rfc2317 RFC 2317> .
receiptIPFilter ::
  -- | 'rifPolicy'
  ReceiptFilterPolicy ->
  -- | 'rifCidr'
  Text ->
  ReceiptIPFilter
receiptIPFilter pPolicy_ pCidr_ =
  ReceiptIPFilter' {_rifPolicy = pPolicy_, _rifCidr = pCidr_}

-- | Indicates whether to block or allow incoming mail from the specified IP addresses.
rifPolicy :: Lens' ReceiptIPFilter ReceiptFilterPolicy
rifPolicy = lens _rifPolicy (\s a -> s {_rifPolicy = a})

-- | A single IP address or a range of IP addresses that you want to block or allow, specified in Classless Inter-Domain Routing (CIDR) notation. An example of a single email address is 10.0.0.1. An example of a range of IP addresses is 10.0.0.1/24. For more information about CIDR notation, see <https://tools.ietf.org/html/rfc2317 RFC 2317> .
rifCidr :: Lens' ReceiptIPFilter Text
rifCidr = lens _rifCidr (\s a -> s {_rifCidr = a})

instance FromXML ReceiptIPFilter where
  parseXML x = ReceiptIPFilter' <$> (x .@ "Policy") <*> (x .@ "Cidr")

instance Hashable ReceiptIPFilter

instance NFData ReceiptIPFilter

instance ToQuery ReceiptIPFilter where
  toQuery ReceiptIPFilter' {..} =
    mconcat ["Policy" =: _rifPolicy, "Cidr" =: _rifCidr]
