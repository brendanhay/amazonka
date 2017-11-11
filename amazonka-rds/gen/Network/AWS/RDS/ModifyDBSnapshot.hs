{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.ModifyDBSnapshot
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a manual DB snapshot, which can be encrypted or not encrypted, with a new engine version. You can update the engine version to either a new major or minor engine version.
--
--
-- Amazon RDS supports upgrading a MySQL DB snapshot from MySQL 5.1 to MySQL 5.5.
--
module Network.AWS.RDS.ModifyDBSnapshot
    (
    -- * Creating a Request
      modifyDBSnapshot
    , ModifyDBSnapshot
    -- * Request Lenses
    , mdsEngineVersion
    , mdsDBSnapshotIdentifier

    -- * Destructuring the Response
    , modifyDBSnapshotResponse
    , ModifyDBSnapshotResponse
    -- * Response Lenses
    , mdsrsDBSnapshot
    , mdsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.RDS.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'modifyDBSnapshot' smart constructor.
data ModifyDBSnapshot = ModifyDBSnapshot'
  { _mdsEngineVersion        :: {-# NOUNPACK #-}!(Maybe Text)
  , _mdsDBSnapshotIdentifier :: {-# NOUNPACK #-}!Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyDBSnapshot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mdsEngineVersion' - The engine version to update the DB snapshot to.
--
-- * 'mdsDBSnapshotIdentifier' - The identifier of the DB snapshot to modify.
modifyDBSnapshot
    :: Text -- ^ 'mdsDBSnapshotIdentifier'
    -> ModifyDBSnapshot
modifyDBSnapshot pDBSnapshotIdentifier_ =
  ModifyDBSnapshot'
  { _mdsEngineVersion = Nothing
  , _mdsDBSnapshotIdentifier = pDBSnapshotIdentifier_
  }


-- | The engine version to update the DB snapshot to.
mdsEngineVersion :: Lens' ModifyDBSnapshot (Maybe Text)
mdsEngineVersion = lens _mdsEngineVersion (\ s a -> s{_mdsEngineVersion = a});

-- | The identifier of the DB snapshot to modify.
mdsDBSnapshotIdentifier :: Lens' ModifyDBSnapshot Text
mdsDBSnapshotIdentifier = lens _mdsDBSnapshotIdentifier (\ s a -> s{_mdsDBSnapshotIdentifier = a});

instance AWSRequest ModifyDBSnapshot where
        type Rs ModifyDBSnapshot = ModifyDBSnapshotResponse
        request = postQuery rds
        response
          = receiveXMLWrapper "ModifyDBSnapshotResult"
              (\ s h x ->
                 ModifyDBSnapshotResponse' <$>
                   (x .@? "DBSnapshot") <*> (pure (fromEnum s)))

instance Hashable ModifyDBSnapshot where

instance NFData ModifyDBSnapshot where

instance ToHeaders ModifyDBSnapshot where
        toHeaders = const mempty

instance ToPath ModifyDBSnapshot where
        toPath = const "/"

instance ToQuery ModifyDBSnapshot where
        toQuery ModifyDBSnapshot'{..}
          = mconcat
              ["Action" =: ("ModifyDBSnapshot" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "EngineVersion" =: _mdsEngineVersion,
               "DBSnapshotIdentifier" =: _mdsDBSnapshotIdentifier]

-- | /See:/ 'modifyDBSnapshotResponse' smart constructor.
data ModifyDBSnapshotResponse = ModifyDBSnapshotResponse'
  { _mdsrsDBSnapshot     :: {-# NOUNPACK #-}!(Maybe DBSnapshot)
  , _mdsrsResponseStatus :: {-# NOUNPACK #-}!Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyDBSnapshotResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mdsrsDBSnapshot' - Undocumented member.
--
-- * 'mdsrsResponseStatus' - -- | The response status code.
modifyDBSnapshotResponse
    :: Int -- ^ 'mdsrsResponseStatus'
    -> ModifyDBSnapshotResponse
modifyDBSnapshotResponse pResponseStatus_ =
  ModifyDBSnapshotResponse'
  {_mdsrsDBSnapshot = Nothing, _mdsrsResponseStatus = pResponseStatus_}


-- | Undocumented member.
mdsrsDBSnapshot :: Lens' ModifyDBSnapshotResponse (Maybe DBSnapshot)
mdsrsDBSnapshot = lens _mdsrsDBSnapshot (\ s a -> s{_mdsrsDBSnapshot = a});

-- | -- | The response status code.
mdsrsResponseStatus :: Lens' ModifyDBSnapshotResponse Int
mdsrsResponseStatus = lens _mdsrsResponseStatus (\ s a -> s{_mdsrsResponseStatus = a});

instance NFData ModifyDBSnapshotResponse where
