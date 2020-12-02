{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.SAMLProviderListEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.SAMLProviderListEntry where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains the list of SAML providers for this account.
--
--
--
-- /See:/ 'sAMLProviderListEntry' smart constructor.
data SAMLProviderListEntry = SAMLProviderListEntry'
  { _samlpleARN ::
      !(Maybe Text),
    _samlpleCreateDate :: !(Maybe ISO8601),
    _samlpleValidUntil :: !(Maybe ISO8601)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SAMLProviderListEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'samlpleARN' - The Amazon Resource Name (ARN) of the SAML provider.
--
-- * 'samlpleCreateDate' - The date and time when the SAML provider was created.
--
-- * 'samlpleValidUntil' - The expiration date and time for the SAML provider.
sAMLProviderListEntry ::
  SAMLProviderListEntry
sAMLProviderListEntry =
  SAMLProviderListEntry'
    { _samlpleARN = Nothing,
      _samlpleCreateDate = Nothing,
      _samlpleValidUntil = Nothing
    }

-- | The Amazon Resource Name (ARN) of the SAML provider.
samlpleARN :: Lens' SAMLProviderListEntry (Maybe Text)
samlpleARN = lens _samlpleARN (\s a -> s {_samlpleARN = a})

-- | The date and time when the SAML provider was created.
samlpleCreateDate :: Lens' SAMLProviderListEntry (Maybe UTCTime)
samlpleCreateDate = lens _samlpleCreateDate (\s a -> s {_samlpleCreateDate = a}) . mapping _Time

-- | The expiration date and time for the SAML provider.
samlpleValidUntil :: Lens' SAMLProviderListEntry (Maybe UTCTime)
samlpleValidUntil = lens _samlpleValidUntil (\s a -> s {_samlpleValidUntil = a}) . mapping _Time

instance FromXML SAMLProviderListEntry where
  parseXML x =
    SAMLProviderListEntry'
      <$> (x .@? "Arn") <*> (x .@? "CreateDate") <*> (x .@? "ValidUntil")

instance Hashable SAMLProviderListEntry

instance NFData SAMLProviderListEntry
