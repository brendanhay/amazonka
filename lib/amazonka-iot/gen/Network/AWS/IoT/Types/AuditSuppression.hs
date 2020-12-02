{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AuditSuppression
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AuditSuppression where

import Network.AWS.IoT.Types.ResourceIdentifier
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Filters out specific findings of a Device Defender audit.
--
--
--
-- /See:/ 'auditSuppression' smart constructor.
data AuditSuppression = AuditSuppression'
  { _asExpirationDate ::
      !(Maybe POSIX),
    _asSuppressIndefinitely :: !(Maybe Bool),
    _asDescription :: !(Maybe Text),
    _asCheckName :: !Text,
    _asResourceIdentifier :: !ResourceIdentifier
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AuditSuppression' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asExpirationDate' - The expiration date (epoch timestamp in seconds) that you want the suppression to adhere to.
--
-- * 'asSuppressIndefinitely' - Indicates whether a suppression should exist indefinitely or not.
--
-- * 'asDescription' - The description of the audit suppression.
--
-- * 'asCheckName' - Undocumented member.
--
-- * 'asResourceIdentifier' - Undocumented member.
auditSuppression ::
  -- | 'asCheckName'
  Text ->
  -- | 'asResourceIdentifier'
  ResourceIdentifier ->
  AuditSuppression
auditSuppression pCheckName_ pResourceIdentifier_ =
  AuditSuppression'
    { _asExpirationDate = Nothing,
      _asSuppressIndefinitely = Nothing,
      _asDescription = Nothing,
      _asCheckName = pCheckName_,
      _asResourceIdentifier = pResourceIdentifier_
    }

-- | The expiration date (epoch timestamp in seconds) that you want the suppression to adhere to.
asExpirationDate :: Lens' AuditSuppression (Maybe UTCTime)
asExpirationDate = lens _asExpirationDate (\s a -> s {_asExpirationDate = a}) . mapping _Time

-- | Indicates whether a suppression should exist indefinitely or not.
asSuppressIndefinitely :: Lens' AuditSuppression (Maybe Bool)
asSuppressIndefinitely = lens _asSuppressIndefinitely (\s a -> s {_asSuppressIndefinitely = a})

-- | The description of the audit suppression.
asDescription :: Lens' AuditSuppression (Maybe Text)
asDescription = lens _asDescription (\s a -> s {_asDescription = a})

-- | Undocumented member.
asCheckName :: Lens' AuditSuppression Text
asCheckName = lens _asCheckName (\s a -> s {_asCheckName = a})

-- | Undocumented member.
asResourceIdentifier :: Lens' AuditSuppression ResourceIdentifier
asResourceIdentifier = lens _asResourceIdentifier (\s a -> s {_asResourceIdentifier = a})

instance FromJSON AuditSuppression where
  parseJSON =
    withObject
      "AuditSuppression"
      ( \x ->
          AuditSuppression'
            <$> (x .:? "expirationDate")
            <*> (x .:? "suppressIndefinitely")
            <*> (x .:? "description")
            <*> (x .: "checkName")
            <*> (x .: "resourceIdentifier")
      )

instance Hashable AuditSuppression

instance NFData AuditSuppression
