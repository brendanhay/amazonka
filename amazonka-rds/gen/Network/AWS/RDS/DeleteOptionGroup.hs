{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DeleteOptionGroup
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing option group.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_DeleteOptionGroup.html>
module Network.AWS.RDS.DeleteOptionGroup
    (
    -- * Request
      DeleteOptionGroup
    -- ** Request constructor
    , deleteOptionGroup
    -- ** Request lenses
    , drqOptionGroupName

    -- * Response
    , DeleteOptionGroupResponse
    -- ** Response constructor
    , deleteOptionGroupResponse
    ) where

import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'deleteOptionGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drqOptionGroupName'
newtype DeleteOptionGroup = DeleteOptionGroup'
    { _drqOptionGroupName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteOptionGroup' smart constructor.
deleteOptionGroup :: Text -> DeleteOptionGroup
deleteOptionGroup pOptionGroupName =
    DeleteOptionGroup'
    { _drqOptionGroupName = pOptionGroupName
    }

-- | The name of the option group to be deleted.
--
-- You cannot delete default option groups.
drqOptionGroupName :: Lens' DeleteOptionGroup Text
drqOptionGroupName = lens _drqOptionGroupName (\ s a -> s{_drqOptionGroupName = a});

instance AWSRequest DeleteOptionGroup where
        type Sv DeleteOptionGroup = RDS
        type Rs DeleteOptionGroup = DeleteOptionGroupResponse
        request = post
        response = receiveNull DeleteOptionGroupResponse'

instance ToHeaders DeleteOptionGroup where
        toHeaders = const mempty

instance ToPath DeleteOptionGroup where
        toPath = const "/"

instance ToQuery DeleteOptionGroup where
        toQuery DeleteOptionGroup'{..}
          = mconcat
              ["Action" =: ("DeleteOptionGroup" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "OptionGroupName" =: _drqOptionGroupName]

-- | /See:/ 'deleteOptionGroupResponse' smart constructor.
data DeleteOptionGroupResponse =
    DeleteOptionGroupResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteOptionGroupResponse' smart constructor.
deleteOptionGroupResponse :: DeleteOptionGroupResponse
deleteOptionGroupResponse = DeleteOptionGroupResponse'
