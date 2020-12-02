{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.SanitizationWarning
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.SanitizationWarning where

import Network.AWS.Lens
import Network.AWS.MQ.Types.SanitizationWarningReason
import Network.AWS.Prelude

-- | Returns information about the XML element or attribute that was sanitized in the configuration.
--
-- /See:/ 'sanitizationWarning' smart constructor.
data SanitizationWarning = SanitizationWarning'
  { _swReason ::
      !(Maybe SanitizationWarningReason),
    _swAttributeName :: !(Maybe Text),
    _swElementName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SanitizationWarning' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'swReason' - Required. The reason for which the XML elements or attributes were sanitized.
--
-- * 'swAttributeName' - The name of the XML attribute that has been sanitized.
--
-- * 'swElementName' - The name of the XML element that has been sanitized.
sanitizationWarning ::
  SanitizationWarning
sanitizationWarning =
  SanitizationWarning'
    { _swReason = Nothing,
      _swAttributeName = Nothing,
      _swElementName = Nothing
    }

-- | Required. The reason for which the XML elements or attributes were sanitized.
swReason :: Lens' SanitizationWarning (Maybe SanitizationWarningReason)
swReason = lens _swReason (\s a -> s {_swReason = a})

-- | The name of the XML attribute that has been sanitized.
swAttributeName :: Lens' SanitizationWarning (Maybe Text)
swAttributeName = lens _swAttributeName (\s a -> s {_swAttributeName = a})

-- | The name of the XML element that has been sanitized.
swElementName :: Lens' SanitizationWarning (Maybe Text)
swElementName = lens _swElementName (\s a -> s {_swElementName = a})

instance FromJSON SanitizationWarning where
  parseJSON =
    withObject
      "SanitizationWarning"
      ( \x ->
          SanitizationWarning'
            <$> (x .:? "reason")
            <*> (x .:? "attributeName")
            <*> (x .:? "elementName")
      )

instance Hashable SanitizationWarning

instance NFData SanitizationWarning
