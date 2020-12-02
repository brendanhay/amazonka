{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.LoggingConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.LoggingConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.WAFRegional.Types.FieldToMatch

-- | The Amazon Kinesis Data Firehose, @RedactedFields@ information, and the web ACL Amazon Resource Name (ARN).
--
--
--
-- /See:/ 'loggingConfiguration' smart constructor.
data LoggingConfiguration = LoggingConfiguration'
  { _lcRedactedFields ::
      !(Maybe [FieldToMatch]),
    _lcResourceARN :: !Text,
    _lcLogDestinationConfigs :: !(List1 Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LoggingConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcRedactedFields' - The parts of the request that you want redacted from the logs. For example, if you redact the cookie field, the cookie field in the firehose will be @xxx@ .
--
-- * 'lcResourceARN' - The Amazon Resource Name (ARN) of the web ACL that you want to associate with @LogDestinationConfigs@ .
--
-- * 'lcLogDestinationConfigs' - An array of Amazon Kinesis Data Firehose ARNs.
loggingConfiguration ::
  -- | 'lcResourceARN'
  Text ->
  -- | 'lcLogDestinationConfigs'
  NonEmpty Text ->
  LoggingConfiguration
loggingConfiguration pResourceARN_ pLogDestinationConfigs_ =
  LoggingConfiguration'
    { _lcRedactedFields = Nothing,
      _lcResourceARN = pResourceARN_,
      _lcLogDestinationConfigs = _List1 # pLogDestinationConfigs_
    }

-- | The parts of the request that you want redacted from the logs. For example, if you redact the cookie field, the cookie field in the firehose will be @xxx@ .
lcRedactedFields :: Lens' LoggingConfiguration [FieldToMatch]
lcRedactedFields = lens _lcRedactedFields (\s a -> s {_lcRedactedFields = a}) . _Default . _Coerce

-- | The Amazon Resource Name (ARN) of the web ACL that you want to associate with @LogDestinationConfigs@ .
lcResourceARN :: Lens' LoggingConfiguration Text
lcResourceARN = lens _lcResourceARN (\s a -> s {_lcResourceARN = a})

-- | An array of Amazon Kinesis Data Firehose ARNs.
lcLogDestinationConfigs :: Lens' LoggingConfiguration (NonEmpty Text)
lcLogDestinationConfigs = lens _lcLogDestinationConfigs (\s a -> s {_lcLogDestinationConfigs = a}) . _List1

instance FromJSON LoggingConfiguration where
  parseJSON =
    withObject
      "LoggingConfiguration"
      ( \x ->
          LoggingConfiguration'
            <$> (x .:? "RedactedFields" .!= mempty)
            <*> (x .: "ResourceArn")
            <*> (x .: "LogDestinationConfigs")
      )

instance Hashable LoggingConfiguration

instance NFData LoggingConfiguration

instance ToJSON LoggingConfiguration where
  toJSON LoggingConfiguration' {..} =
    object
      ( catMaybes
          [ ("RedactedFields" .=) <$> _lcRedactedFields,
            Just ("ResourceArn" .= _lcResourceARN),
            Just ("LogDestinationConfigs" .= _lcLogDestinationConfigs)
          ]
      )
