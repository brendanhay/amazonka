{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.ReceiptRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.ReceiptRule where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SES.Types.ReceiptAction
import Network.AWS.SES.Types.TLSPolicy

-- | Receipt rules enable you to specify which actions Amazon SES should take when it receives mail on behalf of one or more email addresses or domains that you own.
--
--
-- Each receipt rule defines a set of email addresses or domains that it applies to. If the email addresses or domains match at least one recipient address of the message, Amazon SES executes all of the receipt rule's actions on the message.
--
-- For information about setting up receipt rules, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-receipt-rules.html Amazon SES Developer Guide> .
--
--
-- /See:/ 'receiptRule' smart constructor.
data ReceiptRule = ReceiptRule'
  { _rrScanEnabled :: !(Maybe Bool),
    _rrEnabled :: !(Maybe Bool),
    _rrActions :: !(Maybe [ReceiptAction]),
    _rrRecipients :: !(Maybe [Text]),
    _rrTLSPolicy :: !(Maybe TLSPolicy),
    _rrName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReceiptRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrScanEnabled' - If @true@ , then messages that this receipt rule applies to are scanned for spam and viruses. The default value is @false@ .
--
-- * 'rrEnabled' - If @true@ , the receipt rule is active. The default value is @false@ .
--
-- * 'rrActions' - An ordered list of actions to perform on messages that match at least one of the recipient email addresses or domains specified in the receipt rule.
--
-- * 'rrRecipients' - The recipient domains and email addresses that the receipt rule applies to. If this field is not specified, this rule will match all recipients under all verified domains.
--
-- * 'rrTLSPolicy' - Specifies whether Amazon SES should require that incoming email is delivered over a connection encrypted with Transport Layer Security (TLS). If this parameter is set to @Require@ , Amazon SES will bounce emails that are not received over TLS. The default is @Optional@ .
--
-- * 'rrName' - The name of the receipt rule. The name must:     * This value can only contain ASCII letters (a-z, A-Z), numbers (0-9), underscores (_), or dashes (-).     * Start and end with a letter or number.     * Contain less than 64 characters.
receiptRule ::
  -- | 'rrName'
  Text ->
  ReceiptRule
receiptRule pName_ =
  ReceiptRule'
    { _rrScanEnabled = Nothing,
      _rrEnabled = Nothing,
      _rrActions = Nothing,
      _rrRecipients = Nothing,
      _rrTLSPolicy = Nothing,
      _rrName = pName_
    }

-- | If @true@ , then messages that this receipt rule applies to are scanned for spam and viruses. The default value is @false@ .
rrScanEnabled :: Lens' ReceiptRule (Maybe Bool)
rrScanEnabled = lens _rrScanEnabled (\s a -> s {_rrScanEnabled = a})

-- | If @true@ , the receipt rule is active. The default value is @false@ .
rrEnabled :: Lens' ReceiptRule (Maybe Bool)
rrEnabled = lens _rrEnabled (\s a -> s {_rrEnabled = a})

-- | An ordered list of actions to perform on messages that match at least one of the recipient email addresses or domains specified in the receipt rule.
rrActions :: Lens' ReceiptRule [ReceiptAction]
rrActions = lens _rrActions (\s a -> s {_rrActions = a}) . _Default . _Coerce

-- | The recipient domains and email addresses that the receipt rule applies to. If this field is not specified, this rule will match all recipients under all verified domains.
rrRecipients :: Lens' ReceiptRule [Text]
rrRecipients = lens _rrRecipients (\s a -> s {_rrRecipients = a}) . _Default . _Coerce

-- | Specifies whether Amazon SES should require that incoming email is delivered over a connection encrypted with Transport Layer Security (TLS). If this parameter is set to @Require@ , Amazon SES will bounce emails that are not received over TLS. The default is @Optional@ .
rrTLSPolicy :: Lens' ReceiptRule (Maybe TLSPolicy)
rrTLSPolicy = lens _rrTLSPolicy (\s a -> s {_rrTLSPolicy = a})

-- | The name of the receipt rule. The name must:     * This value can only contain ASCII letters (a-z, A-Z), numbers (0-9), underscores (_), or dashes (-).     * Start and end with a letter or number.     * Contain less than 64 characters.
rrName :: Lens' ReceiptRule Text
rrName = lens _rrName (\s a -> s {_rrName = a})

instance FromXML ReceiptRule where
  parseXML x =
    ReceiptRule'
      <$> (x .@? "ScanEnabled")
      <*> (x .@? "Enabled")
      <*> (x .@? "Actions" .!@ mempty >>= may (parseXMLList "member"))
      <*> (x .@? "Recipients" .!@ mempty >>= may (parseXMLList "member"))
      <*> (x .@? "TlsPolicy")
      <*> (x .@ "Name")

instance Hashable ReceiptRule

instance NFData ReceiptRule

instance ToQuery ReceiptRule where
  toQuery ReceiptRule' {..} =
    mconcat
      [ "ScanEnabled" =: _rrScanEnabled,
        "Enabled" =: _rrEnabled,
        "Actions" =: toQuery (toQueryList "member" <$> _rrActions),
        "Recipients" =: toQuery (toQueryList "member" <$> _rrRecipients),
        "TlsPolicy" =: _rrTLSPolicy,
        "Name" =: _rrName
      ]
