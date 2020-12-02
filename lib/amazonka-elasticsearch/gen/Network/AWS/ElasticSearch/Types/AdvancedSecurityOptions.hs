{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.AdvancedSecurityOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.AdvancedSecurityOptions where

import Network.AWS.ElasticSearch.Types.SAMLOptionsOutput
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the advanced security configuration: whether advanced security is enabled, whether the internal database option is enabled.
--
--
--
-- /See:/ 'advancedSecurityOptions' smart constructor.
data AdvancedSecurityOptions = AdvancedSecurityOptions'
  { _asoEnabled ::
      !(Maybe Bool),
    _asoInternalUserDatabaseEnabled ::
      !(Maybe Bool),
    _asoSAMLOptions ::
      !(Maybe SAMLOptionsOutput)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AdvancedSecurityOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asoEnabled' - True if advanced security is enabled.
--
-- * 'asoInternalUserDatabaseEnabled' - True if the internal user database is enabled.
--
-- * 'asoSAMLOptions' - Describes the SAML application configured for a domain.
advancedSecurityOptions ::
  AdvancedSecurityOptions
advancedSecurityOptions =
  AdvancedSecurityOptions'
    { _asoEnabled = Nothing,
      _asoInternalUserDatabaseEnabled = Nothing,
      _asoSAMLOptions = Nothing
    }

-- | True if advanced security is enabled.
asoEnabled :: Lens' AdvancedSecurityOptions (Maybe Bool)
asoEnabled = lens _asoEnabled (\s a -> s {_asoEnabled = a})

-- | True if the internal user database is enabled.
asoInternalUserDatabaseEnabled :: Lens' AdvancedSecurityOptions (Maybe Bool)
asoInternalUserDatabaseEnabled = lens _asoInternalUserDatabaseEnabled (\s a -> s {_asoInternalUserDatabaseEnabled = a})

-- | Describes the SAML application configured for a domain.
asoSAMLOptions :: Lens' AdvancedSecurityOptions (Maybe SAMLOptionsOutput)
asoSAMLOptions = lens _asoSAMLOptions (\s a -> s {_asoSAMLOptions = a})

instance FromJSON AdvancedSecurityOptions where
  parseJSON =
    withObject
      "AdvancedSecurityOptions"
      ( \x ->
          AdvancedSecurityOptions'
            <$> (x .:? "Enabled")
            <*> (x .:? "InternalUserDatabaseEnabled")
            <*> (x .:? "SAMLOptions")
      )

instance Hashable AdvancedSecurityOptions

instance NFData AdvancedSecurityOptions
