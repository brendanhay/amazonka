{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.ReceiptFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.ReceiptFilter where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SES.Types.ReceiptIPFilter

-- | A receipt IP address filter enables you to specify whether to accept or reject mail originating from an IP address or range of IP addresses.
--
--
-- For information about setting up IP address filters, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-ip-filters.html Amazon SES Developer Guide> .
--
--
-- /See:/ 'receiptFilter' smart constructor.
data ReceiptFilter = ReceiptFilter'
  { _rfName :: !Text,
    _rfIPFilter :: !ReceiptIPFilter
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReceiptFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rfName' - The name of the IP address filter. The name must:     * This value can only contain ASCII letters (a-z, A-Z), numbers (0-9), underscores (_), or dashes (-).     * Start and end with a letter or number.     * Contain less than 64 characters.
--
-- * 'rfIPFilter' - A structure that provides the IP addresses to block or allow, and whether to block or allow incoming mail from them.
receiptFilter ::
  -- | 'rfName'
  Text ->
  -- | 'rfIPFilter'
  ReceiptIPFilter ->
  ReceiptFilter
receiptFilter pName_ pIPFilter_ =
  ReceiptFilter' {_rfName = pName_, _rfIPFilter = pIPFilter_}

-- | The name of the IP address filter. The name must:     * This value can only contain ASCII letters (a-z, A-Z), numbers (0-9), underscores (_), or dashes (-).     * Start and end with a letter or number.     * Contain less than 64 characters.
rfName :: Lens' ReceiptFilter Text
rfName = lens _rfName (\s a -> s {_rfName = a})

-- | A structure that provides the IP addresses to block or allow, and whether to block or allow incoming mail from them.
rfIPFilter :: Lens' ReceiptFilter ReceiptIPFilter
rfIPFilter = lens _rfIPFilter (\s a -> s {_rfIPFilter = a})

instance FromXML ReceiptFilter where
  parseXML x = ReceiptFilter' <$> (x .@ "Name") <*> (x .@ "IpFilter")

instance Hashable ReceiptFilter

instance NFData ReceiptFilter

instance ToQuery ReceiptFilter where
  toQuery ReceiptFilter' {..} =
    mconcat ["Name" =: _rfName, "IpFilter" =: _rfIPFilter]
