{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.RDS.DeleteOptionGroup
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Deletes an existing option group.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_DeleteOptionGroup.html>
module Network.AWS.RDS.DeleteOptionGroup
    (
    -- * Request
      DeleteOptionGroup
    -- ** Request constructor
    , deleteOptionGroup
    -- ** Request lenses
    , delOptionGroupName

    -- * Response
    , DeleteOptionGroupResponse
    -- ** Response constructor
    , deleteOptionGroupResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteOptionGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'delOptionGroupName'
newtype DeleteOptionGroup = DeleteOptionGroup'{_delOptionGroupName :: Text} deriving (Eq, Read, Show)

-- | 'DeleteOptionGroup' smart constructor.
deleteOptionGroup :: Text -> DeleteOptionGroup
deleteOptionGroup pOptionGroupName = DeleteOptionGroup'{_delOptionGroupName = pOptionGroupName};

-- | The name of the option group to be deleted.
--
-- You cannot delete default option groups.
delOptionGroupName :: Lens' DeleteOptionGroup Text
delOptionGroupName = lens _delOptionGroupName (\ s a -> s{_delOptionGroupName = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

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
               "OptionGroupName" =: _delOptionGroupName]

-- | /See:/ 'deleteOptionGroupResponse' smart constructor.
data DeleteOptionGroupResponse = DeleteOptionGroupResponse' deriving (Eq, Read, Show)

-- | 'DeleteOptionGroupResponse' smart constructor.
deleteOptionGroupResponse :: DeleteOptionGroupResponse
deleteOptionGroupResponse = DeleteOptionGroupResponse';
