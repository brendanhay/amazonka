{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.AccountQuota
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.AccountQuota where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a quota for an AWS account, for example, the number of replication instances allowed.
--
--
--
-- /See:/ 'accountQuota' smart constructor.
data AccountQuota = AccountQuota'
  { _aqMax :: !(Maybe Integer),
    _aqUsed :: !(Maybe Integer),
    _aqAccountQuotaName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AccountQuota' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aqMax' - The maximum allowed value for the quota.
--
-- * 'aqUsed' - The amount currently used toward the quota maximum.
--
-- * 'aqAccountQuotaName' - The name of the AWS DMS quota for this AWS account.
accountQuota ::
  AccountQuota
accountQuota =
  AccountQuota'
    { _aqMax = Nothing,
      _aqUsed = Nothing,
      _aqAccountQuotaName = Nothing
    }

-- | The maximum allowed value for the quota.
aqMax :: Lens' AccountQuota (Maybe Integer)
aqMax = lens _aqMax (\s a -> s {_aqMax = a})

-- | The amount currently used toward the quota maximum.
aqUsed :: Lens' AccountQuota (Maybe Integer)
aqUsed = lens _aqUsed (\s a -> s {_aqUsed = a})

-- | The name of the AWS DMS quota for this AWS account.
aqAccountQuotaName :: Lens' AccountQuota (Maybe Text)
aqAccountQuotaName = lens _aqAccountQuotaName (\s a -> s {_aqAccountQuotaName = a})

instance FromJSON AccountQuota where
  parseJSON =
    withObject
      "AccountQuota"
      ( \x ->
          AccountQuota'
            <$> (x .:? "Max") <*> (x .:? "Used") <*> (x .:? "AccountQuotaName")
      )

instance Hashable AccountQuota

instance NFData AccountQuota
