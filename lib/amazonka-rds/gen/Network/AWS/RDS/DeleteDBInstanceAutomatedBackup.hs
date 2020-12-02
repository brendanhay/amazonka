{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DeleteDBInstanceAutomatedBackup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes automated backups based on the source instance's @DbiResourceId@ value or the restorable instance's resource ID.
module Network.AWS.RDS.DeleteDBInstanceAutomatedBackup
  ( -- * Creating a Request
    deleteDBInstanceAutomatedBackup,
    DeleteDBInstanceAutomatedBackup,

    -- * Request Lenses
    ddiabDBiResourceId,

    -- * Destructuring the Response
    deleteDBInstanceAutomatedBackupResponse,
    DeleteDBInstanceAutomatedBackupResponse,

    -- * Response Lenses
    ddbiabrsDBInstanceAutomatedBackup,
    ddbiabrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.Request
import Network.AWS.Response

-- | Parameter input for the @DeleteDBInstanceAutomatedBackup@ operation.
--
--
--
-- /See:/ 'deleteDBInstanceAutomatedBackup' smart constructor.
newtype DeleteDBInstanceAutomatedBackup = DeleteDBInstanceAutomatedBackup'
  { _ddiabDBiResourceId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteDBInstanceAutomatedBackup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddiabDBiResourceId' - The identifier for the source DB instance, which can't be changed and which is unique to an AWS Region.
deleteDBInstanceAutomatedBackup ::
  -- | 'ddiabDBiResourceId'
  Text ->
  DeleteDBInstanceAutomatedBackup
deleteDBInstanceAutomatedBackup pDBiResourceId_ =
  DeleteDBInstanceAutomatedBackup'
    { _ddiabDBiResourceId =
        pDBiResourceId_
    }

-- | The identifier for the source DB instance, which can't be changed and which is unique to an AWS Region.
ddiabDBiResourceId :: Lens' DeleteDBInstanceAutomatedBackup Text
ddiabDBiResourceId = lens _ddiabDBiResourceId (\s a -> s {_ddiabDBiResourceId = a})

instance AWSRequest DeleteDBInstanceAutomatedBackup where
  type
    Rs DeleteDBInstanceAutomatedBackup =
      DeleteDBInstanceAutomatedBackupResponse
  request = postQuery rds
  response =
    receiveXMLWrapper
      "DeleteDBInstanceAutomatedBackupResult"
      ( \s h x ->
          DeleteDBInstanceAutomatedBackupResponse'
            <$> (x .@? "DBInstanceAutomatedBackup") <*> (pure (fromEnum s))
      )

instance Hashable DeleteDBInstanceAutomatedBackup

instance NFData DeleteDBInstanceAutomatedBackup

instance ToHeaders DeleteDBInstanceAutomatedBackup where
  toHeaders = const mempty

instance ToPath DeleteDBInstanceAutomatedBackup where
  toPath = const "/"

instance ToQuery DeleteDBInstanceAutomatedBackup where
  toQuery DeleteDBInstanceAutomatedBackup' {..} =
    mconcat
      [ "Action" =: ("DeleteDBInstanceAutomatedBackup" :: ByteString),
        "Version" =: ("2014-10-31" :: ByteString),
        "DbiResourceId" =: _ddiabDBiResourceId
      ]

-- | /See:/ 'deleteDBInstanceAutomatedBackupResponse' smart constructor.
data DeleteDBInstanceAutomatedBackupResponse = DeleteDBInstanceAutomatedBackupResponse'
  { _ddbiabrsDBInstanceAutomatedBackup ::
      !( Maybe
           DBInstanceAutomatedBackup
       ),
    _ddbiabrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteDBInstanceAutomatedBackupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddbiabrsDBInstanceAutomatedBackup' - Undocumented member.
--
-- * 'ddbiabrsResponseStatus' - -- | The response status code.
deleteDBInstanceAutomatedBackupResponse ::
  -- | 'ddbiabrsResponseStatus'
  Int ->
  DeleteDBInstanceAutomatedBackupResponse
deleteDBInstanceAutomatedBackupResponse pResponseStatus_ =
  DeleteDBInstanceAutomatedBackupResponse'
    { _ddbiabrsDBInstanceAutomatedBackup =
        Nothing,
      _ddbiabrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
ddbiabrsDBInstanceAutomatedBackup :: Lens' DeleteDBInstanceAutomatedBackupResponse (Maybe DBInstanceAutomatedBackup)
ddbiabrsDBInstanceAutomatedBackup = lens _ddbiabrsDBInstanceAutomatedBackup (\s a -> s {_ddbiabrsDBInstanceAutomatedBackup = a})

-- | -- | The response status code.
ddbiabrsResponseStatus :: Lens' DeleteDBInstanceAutomatedBackupResponse Int
ddbiabrsResponseStatus = lens _ddbiabrsResponseStatus (\s a -> s {_ddbiabrsResponseStatus = a})

instance NFData DeleteDBInstanceAutomatedBackupResponse
