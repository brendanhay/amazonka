{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.RemoveTagsFromOnPremisesInstances
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- | Removes one or more tags from one or more on-premises instances.
--
-- <http://docs.aws.amazon.com/codedeploy/latest/APIReference/API_RemoveTagsFromOnPremisesInstances.html>
module Network.AWS.CodeDeploy.RemoveTagsFromOnPremisesInstances
    (
    -- * Request
      RemoveTagsFromOnPremisesInstances
    -- ** Request constructor
    , removeTagsFromOnPremisesInstances
    -- ** Request lenses
    , rtfopiTags
    , rtfopiInstanceNames

    -- * Response
    , RemoveTagsFromOnPremisesInstancesResponse
    -- ** Response constructor
    , removeTagsFromOnPremisesInstancesResponse
    ) where

import           Network.AWS.CodeDeploy.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a remove tags from on-premises instances
-- operation.
--
-- /See:/ 'removeTagsFromOnPremisesInstances' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rtfopiTags'
--
-- * 'rtfopiInstanceNames'
data RemoveTagsFromOnPremisesInstances = RemoveTagsFromOnPremisesInstances'
    { _rtfopiTags          :: ![Tag]
    , _rtfopiInstanceNames :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RemoveTagsFromOnPremisesInstances' smart constructor.
removeTagsFromOnPremisesInstances :: RemoveTagsFromOnPremisesInstances
removeTagsFromOnPremisesInstances =
    RemoveTagsFromOnPremisesInstances'
    { _rtfopiTags = mempty
    , _rtfopiInstanceNames = mempty
    }

-- | The tag key-value pairs to remove from the on-premises instances.
rtfopiTags :: Lens' RemoveTagsFromOnPremisesInstances [Tag]
rtfopiTags = lens _rtfopiTags (\ s a -> s{_rtfopiTags = a});

-- | The names of the on-premises instances to remove tags from.
rtfopiInstanceNames :: Lens' RemoveTagsFromOnPremisesInstances [Text]
rtfopiInstanceNames = lens _rtfopiInstanceNames (\ s a -> s{_rtfopiInstanceNames = a});

instance AWSRequest RemoveTagsFromOnPremisesInstances
         where
        type Sv RemoveTagsFromOnPremisesInstances =
             CodeDeploy
        type Rs RemoveTagsFromOnPremisesInstances =
             RemoveTagsFromOnPremisesInstancesResponse
        request = postJSON
        response
          = receiveNull
              RemoveTagsFromOnPremisesInstancesResponse'

instance ToHeaders RemoveTagsFromOnPremisesInstances
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeDeploy_20141006.RemoveTagsFromOnPremisesInstances"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RemoveTagsFromOnPremisesInstances
         where
        toJSON RemoveTagsFromOnPremisesInstances'{..}
          = object
              ["tags" .= _rtfopiTags,
               "instanceNames" .= _rtfopiInstanceNames]

instance ToPath RemoveTagsFromOnPremisesInstances
         where
        toPath = const "/"

instance ToQuery RemoveTagsFromOnPremisesInstances
         where
        toQuery = const mempty

-- | /See:/ 'removeTagsFromOnPremisesInstancesResponse' smart constructor.
data RemoveTagsFromOnPremisesInstancesResponse =
    RemoveTagsFromOnPremisesInstancesResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RemoveTagsFromOnPremisesInstancesResponse' smart constructor.
removeTagsFromOnPremisesInstancesResponse :: RemoveTagsFromOnPremisesInstancesResponse
removeTagsFromOnPremisesInstancesResponse =
    RemoveTagsFromOnPremisesInstancesResponse'
