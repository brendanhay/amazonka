{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DBInstanceRole
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBInstanceRole where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an AWS Identity and Access Management (IAM) role that is associated with a DB instance.
--
--
--
-- /See:/ 'dbInstanceRole' smart constructor.
data DBInstanceRole = DBInstanceRole'
  { _dirStatus :: !(Maybe Text),
    _dirFeatureName :: !(Maybe Text),
    _dirRoleARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DBInstanceRole' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dirStatus' - Describes the state of association between the IAM role and the DB instance. The Status property returns one of the following values:     * @ACTIVE@ - the IAM role ARN is associated with the DB instance and can be used to access other AWS services on your behalf.     * @PENDING@ - the IAM role ARN is being associated with the DB instance.     * @INVALID@ - the IAM role ARN is associated with the DB instance, but the DB instance is unable to assume the IAM role in order to access other AWS services on your behalf.
--
-- * 'dirFeatureName' - The name of the feature associated with the AWS Identity and Access Management (IAM) role. For the list of supported feature names, see @DBEngineVersion@ .
--
-- * 'dirRoleARN' - The Amazon Resource Name (ARN) of the IAM role that is associated with the DB instance.
dbInstanceRole ::
  DBInstanceRole
dbInstanceRole =
  DBInstanceRole'
    { _dirStatus = Nothing,
      _dirFeatureName = Nothing,
      _dirRoleARN = Nothing
    }

-- | Describes the state of association between the IAM role and the DB instance. The Status property returns one of the following values:     * @ACTIVE@ - the IAM role ARN is associated with the DB instance and can be used to access other AWS services on your behalf.     * @PENDING@ - the IAM role ARN is being associated with the DB instance.     * @INVALID@ - the IAM role ARN is associated with the DB instance, but the DB instance is unable to assume the IAM role in order to access other AWS services on your behalf.
dirStatus :: Lens' DBInstanceRole (Maybe Text)
dirStatus = lens _dirStatus (\s a -> s {_dirStatus = a})

-- | The name of the feature associated with the AWS Identity and Access Management (IAM) role. For the list of supported feature names, see @DBEngineVersion@ .
dirFeatureName :: Lens' DBInstanceRole (Maybe Text)
dirFeatureName = lens _dirFeatureName (\s a -> s {_dirFeatureName = a})

-- | The Amazon Resource Name (ARN) of the IAM role that is associated with the DB instance.
dirRoleARN :: Lens' DBInstanceRole (Maybe Text)
dirRoleARN = lens _dirRoleARN (\s a -> s {_dirRoleARN = a})

instance FromXML DBInstanceRole where
  parseXML x =
    DBInstanceRole'
      <$> (x .@? "Status") <*> (x .@? "FeatureName") <*> (x .@? "RoleArn")

instance Hashable DBInstanceRole

instance NFData DBInstanceRole
