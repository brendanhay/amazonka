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
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing option group.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_DeleteOptionGroup.html AWS API Reference> for DeleteOptionGroup.
module Network.AWS.RDS.DeleteOptionGroup
    (
    -- * Creating a Request
      DeleteOptionGroup
    , deleteOptionGroup
    -- * Request Lenses
    , dOptionGroupName

    -- * Destructuring the Response
    , DeleteOptionGroupResponse
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
-- * 'dOptionGroupName'
newtype DeleteOptionGroup = DeleteOptionGroup'
    { _dOptionGroupName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteOptionGroup' smart constructor.
deleteOptionGroup :: Text -> DeleteOptionGroup
deleteOptionGroup pOptionGroupName_ =
    DeleteOptionGroup'
    { _dOptionGroupName = pOptionGroupName_
    }

-- | The name of the option group to be deleted.
--
-- You cannot delete default option groups.
dOptionGroupName :: Lens' DeleteOptionGroup Text
dOptionGroupName = lens _dOptionGroupName (\ s a -> s{_dOptionGroupName = a});

instance AWSRequest DeleteOptionGroup where
        type Sv DeleteOptionGroup = RDS
        type Rs DeleteOptionGroup = DeleteOptionGroupResponse
        request = postQuery
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
               "OptionGroupName" =: _dOptionGroupName]

-- | /See:/ 'deleteOptionGroupResponse' smart constructor.
data DeleteOptionGroupResponse =
    DeleteOptionGroupResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteOptionGroupResponse' smart constructor.
deleteOptionGroupResponse :: DeleteOptionGroupResponse
deleteOptionGroupResponse = DeleteOptionGroupResponse'
