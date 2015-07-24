{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DeleteSnapshotCopyGrant
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified snapshot copy grant.
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_DeleteSnapshotCopyGrant.html>
module Network.AWS.Redshift.DeleteSnapshotCopyGrant
    (
    -- * Request
      DeleteSnapshotCopyGrant
    -- ** Request constructor
    , deleteSnapshotCopyGrant
    -- ** Request lenses
    , dscgSnapshotCopyGrantName

    -- * Response
    , DeleteSnapshotCopyGrantResponse
    -- ** Response constructor
    , deleteSnapshotCopyGrantResponse
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Redshift.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- | The result of the @DeleteSnapshotCopyGrant@ action.
--
-- /See:/ 'deleteSnapshotCopyGrant' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dscgSnapshotCopyGrantName'
newtype DeleteSnapshotCopyGrant = DeleteSnapshotCopyGrant'
    { _dscgSnapshotCopyGrantName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteSnapshotCopyGrant' smart constructor.
deleteSnapshotCopyGrant :: Text -> DeleteSnapshotCopyGrant
deleteSnapshotCopyGrant pSnapshotCopyGrantName_ =
    DeleteSnapshotCopyGrant'
    { _dscgSnapshotCopyGrantName = pSnapshotCopyGrantName_
    }

-- | The name of the snapshot copy grant to delete.
dscgSnapshotCopyGrantName :: Lens' DeleteSnapshotCopyGrant Text
dscgSnapshotCopyGrantName = lens _dscgSnapshotCopyGrantName (\ s a -> s{_dscgSnapshotCopyGrantName = a});

instance AWSRequest DeleteSnapshotCopyGrant where
        type Sv DeleteSnapshotCopyGrant = Redshift
        type Rs DeleteSnapshotCopyGrant =
             DeleteSnapshotCopyGrantResponse
        request = post "DeleteSnapshotCopyGrant"
        response
          = receiveNull DeleteSnapshotCopyGrantResponse'

instance ToHeaders DeleteSnapshotCopyGrant where
        toHeaders = const mempty

instance ToPath DeleteSnapshotCopyGrant where
        toPath = const "/"

instance ToQuery DeleteSnapshotCopyGrant where
        toQuery DeleteSnapshotCopyGrant'{..}
          = mconcat
              ["Action" =:
                 ("DeleteSnapshotCopyGrant" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "SnapshotCopyGrantName" =:
                 _dscgSnapshotCopyGrantName]

-- | /See:/ 'deleteSnapshotCopyGrantResponse' smart constructor.
data DeleteSnapshotCopyGrantResponse =
    DeleteSnapshotCopyGrantResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteSnapshotCopyGrantResponse' smart constructor.
deleteSnapshotCopyGrantResponse :: DeleteSnapshotCopyGrantResponse
deleteSnapshotCopyGrantResponse = DeleteSnapshotCopyGrantResponse'
