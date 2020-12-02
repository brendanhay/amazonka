{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.DelegatedService
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.DelegatedService where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about the AWS service for which the account is a delegated administrator.
--
--
--
-- /See:/ 'delegatedService' smart constructor.
data DelegatedService = DelegatedService'
  { _dsServicePrincipal ::
      !(Maybe Text),
    _dsDelegationEnabledDate :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DelegatedService' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsServicePrincipal' - The name of a service that can request an operation for the specified service. This is typically in the form of a URL, such as: @/servicename/ .amazonaws.com@ .
--
-- * 'dsDelegationEnabledDate' - The date that the account became a delegated administrator for this service.
delegatedService ::
  DelegatedService
delegatedService =
  DelegatedService'
    { _dsServicePrincipal = Nothing,
      _dsDelegationEnabledDate = Nothing
    }

-- | The name of a service that can request an operation for the specified service. This is typically in the form of a URL, such as: @/servicename/ .amazonaws.com@ .
dsServicePrincipal :: Lens' DelegatedService (Maybe Text)
dsServicePrincipal = lens _dsServicePrincipal (\s a -> s {_dsServicePrincipal = a})

-- | The date that the account became a delegated administrator for this service.
dsDelegationEnabledDate :: Lens' DelegatedService (Maybe UTCTime)
dsDelegationEnabledDate = lens _dsDelegationEnabledDate (\s a -> s {_dsDelegationEnabledDate = a}) . mapping _Time

instance FromJSON DelegatedService where
  parseJSON =
    withObject
      "DelegatedService"
      ( \x ->
          DelegatedService'
            <$> (x .:? "ServicePrincipal") <*> (x .:? "DelegationEnabledDate")
      )

instance Hashable DelegatedService

instance NFData DelegatedService
