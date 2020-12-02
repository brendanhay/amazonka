{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DBSecurityGroupMembership
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBSecurityGroupMembership where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | This data type is used as a response element in the following actions:
--
--
--     * @ModifyDBInstance@
--
--     * @RebootDBInstance@
--
--     * @RestoreDBInstanceFromDBSnapshot@
--
--     * @RestoreDBInstanceToPointInTime@
--
--
--
--
-- /See:/ 'dbSecurityGroupMembership' smart constructor.
data DBSecurityGroupMembership = DBSecurityGroupMembership'
  { _dsgmStatus ::
      !(Maybe Text),
    _dsgmDBSecurityGroupName ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DBSecurityGroupMembership' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsgmStatus' - The status of the DB security group.
--
-- * 'dsgmDBSecurityGroupName' - The name of the DB security group.
dbSecurityGroupMembership ::
  DBSecurityGroupMembership
dbSecurityGroupMembership =
  DBSecurityGroupMembership'
    { _dsgmStatus = Nothing,
      _dsgmDBSecurityGroupName = Nothing
    }

-- | The status of the DB security group.
dsgmStatus :: Lens' DBSecurityGroupMembership (Maybe Text)
dsgmStatus = lens _dsgmStatus (\s a -> s {_dsgmStatus = a})

-- | The name of the DB security group.
dsgmDBSecurityGroupName :: Lens' DBSecurityGroupMembership (Maybe Text)
dsgmDBSecurityGroupName = lens _dsgmDBSecurityGroupName (\s a -> s {_dsgmDBSecurityGroupName = a})

instance FromXML DBSecurityGroupMembership where
  parseXML x =
    DBSecurityGroupMembership'
      <$> (x .@? "Status") <*> (x .@? "DBSecurityGroupName")

instance Hashable DBSecurityGroupMembership

instance NFData DBSecurityGroupMembership
