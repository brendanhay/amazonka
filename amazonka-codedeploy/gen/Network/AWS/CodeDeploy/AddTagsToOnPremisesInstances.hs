{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.AddTagsToOnPremisesInstances
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Adds tags to on-premises instances.
--
-- <http://docs.aws.amazon.com/codedeploy/latest/APIReference/API_AddTagsToOnPremisesInstances.html>
module Network.AWS.CodeDeploy.AddTagsToOnPremisesInstances
    (
    -- * Request
      AddTagsToOnPremisesInstances
    -- ** Request constructor
    , addTagsToOnPremisesInstances
    -- ** Request lenses
    , attopiTags
    , attopiInstanceNames

    -- * Response
    , AddTagsToOnPremisesInstancesResponse
    -- ** Response constructor
    , addTagsToOnPremisesInstancesResponse
    ) where

import           Network.AWS.CodeDeploy.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of an adds tags to on-premises instance operation.
--
-- /See:/ 'addTagsToOnPremisesInstances' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'attopiTags'
--
-- * 'attopiInstanceNames'
data AddTagsToOnPremisesInstances = AddTagsToOnPremisesInstances'
    { _attopiTags          :: ![Tag]
    , _attopiInstanceNames :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AddTagsToOnPremisesInstances' smart constructor.
addTagsToOnPremisesInstances :: AddTagsToOnPremisesInstances
addTagsToOnPremisesInstances =
    AddTagsToOnPremisesInstances'
    { _attopiTags = mempty
    , _attopiInstanceNames = mempty
    }

-- | The tag key-value pairs to add to the on-premises instances.
--
-- Keys and values are both required. Keys cannot be nulls or empty
-- strings. Value-only tags are not allowed.
attopiTags :: Lens' AddTagsToOnPremisesInstances [Tag]
attopiTags = lens _attopiTags (\ s a -> s{_attopiTags = a}) . _Coerce;

-- | The names of the on-premises instances to add tags to.
attopiInstanceNames :: Lens' AddTagsToOnPremisesInstances [Text]
attopiInstanceNames = lens _attopiInstanceNames (\ s a -> s{_attopiInstanceNames = a}) . _Coerce;

instance AWSRequest AddTagsToOnPremisesInstances
         where
        type Sv AddTagsToOnPremisesInstances = CodeDeploy
        type Rs AddTagsToOnPremisesInstances =
             AddTagsToOnPremisesInstancesResponse
        request = postJSON
        response
          = receiveNull AddTagsToOnPremisesInstancesResponse'

instance ToHeaders AddTagsToOnPremisesInstances where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeDeploy_20141006.AddTagsToOnPremisesInstances"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AddTagsToOnPremisesInstances where
        toJSON AddTagsToOnPremisesInstances'{..}
          = object
              ["tags" .= _attopiTags,
               "instanceNames" .= _attopiInstanceNames]

instance ToPath AddTagsToOnPremisesInstances where
        toPath = const "/"

instance ToQuery AddTagsToOnPremisesInstances where
        toQuery = const mempty

-- | /See:/ 'addTagsToOnPremisesInstancesResponse' smart constructor.
data AddTagsToOnPremisesInstancesResponse =
    AddTagsToOnPremisesInstancesResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AddTagsToOnPremisesInstancesResponse' smart constructor.
addTagsToOnPremisesInstancesResponse :: AddTagsToOnPremisesInstancesResponse
addTagsToOnPremisesInstancesResponse = AddTagsToOnPremisesInstancesResponse'
