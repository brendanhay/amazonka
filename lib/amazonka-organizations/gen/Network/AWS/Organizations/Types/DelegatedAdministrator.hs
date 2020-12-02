{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.DelegatedAdministrator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.DelegatedAdministrator where

import Network.AWS.Lens
import Network.AWS.Organizations.Types.AccountJoinedMethod
import Network.AWS.Organizations.Types.AccountStatus
import Network.AWS.Prelude

-- | Contains information about the delegated administrator.
--
--
--
-- /See:/ 'delegatedAdministrator' smart constructor.
data DelegatedAdministrator = DelegatedAdministrator'
  { _daStatus ::
      !(Maybe AccountStatus),
    _daJoinedMethod ::
      !(Maybe AccountJoinedMethod),
    _daEmail :: !(Maybe (Sensitive Text)),
    _daARN :: !(Maybe Text),
    _daJoinedTimestamp :: !(Maybe POSIX),
    _daDelegationEnabledDate :: !(Maybe POSIX),
    _daName :: !(Maybe (Sensitive Text)),
    _daId :: !(Maybe Text)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'DelegatedAdministrator' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daStatus' - The status of the delegated administrator's account in the organization.
--
-- * 'daJoinedMethod' - The method by which the delegated administrator's account joined the organization.
--
-- * 'daEmail' - The email address that is associated with the delegated administrator's AWS account.
--
-- * 'daARN' - The Amazon Resource Name (ARN) of the delegated administrator's account.
--
-- * 'daJoinedTimestamp' - The date when the delegated administrator's account became a part of the organization.
--
-- * 'daDelegationEnabledDate' - The date when the account was made a delegated administrator.
--
-- * 'daName' - The friendly name of the delegated administrator's account.
--
-- * 'daId' - The unique identifier (ID) of the delegated administrator's account.
delegatedAdministrator ::
  DelegatedAdministrator
delegatedAdministrator =
  DelegatedAdministrator'
    { _daStatus = Nothing,
      _daJoinedMethod = Nothing,
      _daEmail = Nothing,
      _daARN = Nothing,
      _daJoinedTimestamp = Nothing,
      _daDelegationEnabledDate = Nothing,
      _daName = Nothing,
      _daId = Nothing
    }

-- | The status of the delegated administrator's account in the organization.
daStatus :: Lens' DelegatedAdministrator (Maybe AccountStatus)
daStatus = lens _daStatus (\s a -> s {_daStatus = a})

-- | The method by which the delegated administrator's account joined the organization.
daJoinedMethod :: Lens' DelegatedAdministrator (Maybe AccountJoinedMethod)
daJoinedMethod = lens _daJoinedMethod (\s a -> s {_daJoinedMethod = a})

-- | The email address that is associated with the delegated administrator's AWS account.
daEmail :: Lens' DelegatedAdministrator (Maybe Text)
daEmail = lens _daEmail (\s a -> s {_daEmail = a}) . mapping _Sensitive

-- | The Amazon Resource Name (ARN) of the delegated administrator's account.
daARN :: Lens' DelegatedAdministrator (Maybe Text)
daARN = lens _daARN (\s a -> s {_daARN = a})

-- | The date when the delegated administrator's account became a part of the organization.
daJoinedTimestamp :: Lens' DelegatedAdministrator (Maybe UTCTime)
daJoinedTimestamp = lens _daJoinedTimestamp (\s a -> s {_daJoinedTimestamp = a}) . mapping _Time

-- | The date when the account was made a delegated administrator.
daDelegationEnabledDate :: Lens' DelegatedAdministrator (Maybe UTCTime)
daDelegationEnabledDate = lens _daDelegationEnabledDate (\s a -> s {_daDelegationEnabledDate = a}) . mapping _Time

-- | The friendly name of the delegated administrator's account.
daName :: Lens' DelegatedAdministrator (Maybe Text)
daName = lens _daName (\s a -> s {_daName = a}) . mapping _Sensitive

-- | The unique identifier (ID) of the delegated administrator's account.
daId :: Lens' DelegatedAdministrator (Maybe Text)
daId = lens _daId (\s a -> s {_daId = a})

instance FromJSON DelegatedAdministrator where
  parseJSON =
    withObject
      "DelegatedAdministrator"
      ( \x ->
          DelegatedAdministrator'
            <$> (x .:? "Status")
            <*> (x .:? "JoinedMethod")
            <*> (x .:? "Email")
            <*> (x .:? "Arn")
            <*> (x .:? "JoinedTimestamp")
            <*> (x .:? "DelegationEnabledDate")
            <*> (x .:? "Name")
            <*> (x .:? "Id")
      )

instance Hashable DelegatedAdministrator

instance NFData DelegatedAdministrator
