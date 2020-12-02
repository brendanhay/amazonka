{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.ReceiptRuleSetMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.ReceiptRuleSetMetadata where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a receipt rule set.
--
--
-- A receipt rule set is a collection of rules that specify what Amazon SES should do with mail it receives on behalf of your account's verified domains.
--
-- For information about setting up receipt rule sets, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-receipt-rule-set.html Amazon SES Developer Guide> .
--
--
-- /See:/ 'receiptRuleSetMetadata' smart constructor.
data ReceiptRuleSetMetadata = ReceiptRuleSetMetadata'
  { _rrsmName ::
      !(Maybe Text),
    _rrsmCreatedTimestamp :: !(Maybe ISO8601)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReceiptRuleSetMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrsmName' - The name of the receipt rule set. The name must:     * This value can only contain ASCII letters (a-z, A-Z), numbers (0-9), underscores (_), or dashes (-).     * Start and end with a letter or number.     * Contain less than 64 characters.
--
-- * 'rrsmCreatedTimestamp' - The date and time the receipt rule set was created.
receiptRuleSetMetadata ::
  ReceiptRuleSetMetadata
receiptRuleSetMetadata =
  ReceiptRuleSetMetadata'
    { _rrsmName = Nothing,
      _rrsmCreatedTimestamp = Nothing
    }

-- | The name of the receipt rule set. The name must:     * This value can only contain ASCII letters (a-z, A-Z), numbers (0-9), underscores (_), or dashes (-).     * Start and end with a letter or number.     * Contain less than 64 characters.
rrsmName :: Lens' ReceiptRuleSetMetadata (Maybe Text)
rrsmName = lens _rrsmName (\s a -> s {_rrsmName = a})

-- | The date and time the receipt rule set was created.
rrsmCreatedTimestamp :: Lens' ReceiptRuleSetMetadata (Maybe UTCTime)
rrsmCreatedTimestamp = lens _rrsmCreatedTimestamp (\s a -> s {_rrsmCreatedTimestamp = a}) . mapping _Time

instance FromXML ReceiptRuleSetMetadata where
  parseXML x =
    ReceiptRuleSetMetadata'
      <$> (x .@? "Name") <*> (x .@? "CreatedTimestamp")

instance Hashable ReceiptRuleSetMetadata

instance NFData ReceiptRuleSetMetadata
