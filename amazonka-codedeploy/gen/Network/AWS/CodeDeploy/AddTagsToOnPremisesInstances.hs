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
-- Module      : Network.AWS.CodeDeploy.AddTagsToOnPremisesInstances
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds tags to on-premises instances.
--
--
module Network.AWS.CodeDeploy.AddTagsToOnPremisesInstances
    (
    -- * Creating a Request
      addTagsToOnPremisesInstances
    , AddTagsToOnPremisesInstances
    -- * Request Lenses
    , attopiTags
    , attopiInstanceNames

    -- * Destructuring the Response
    , addTagsToOnPremisesInstancesResponse
    , AddTagsToOnPremisesInstancesResponse
    ) where

import Network.AWS.CodeDeploy.Types
import Network.AWS.CodeDeploy.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of, and adds tags to, an on-premises instance operation.
--
--
--
-- /See:/ 'addTagsToOnPremisesInstances' smart constructor.
data AddTagsToOnPremisesInstances = AddTagsToOnPremisesInstances'
  { _attopiTags          :: ![Tag]
  , _attopiInstanceNames :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddTagsToOnPremisesInstances' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'attopiTags' - The tag key-value pairs to add to the on-premises instances. Keys and values are both required. Keys cannot be null or empty strings. Value-only tags are not allowed.
--
-- * 'attopiInstanceNames' - The names of the on-premises instances to which to add tags.
addTagsToOnPremisesInstances
    :: AddTagsToOnPremisesInstances
addTagsToOnPremisesInstances =
  AddTagsToOnPremisesInstances'
    {_attopiTags = mempty, _attopiInstanceNames = mempty}


-- | The tag key-value pairs to add to the on-premises instances. Keys and values are both required. Keys cannot be null or empty strings. Value-only tags are not allowed.
attopiTags :: Lens' AddTagsToOnPremisesInstances [Tag]
attopiTags = lens _attopiTags (\ s a -> s{_attopiTags = a}) . _Coerce

-- | The names of the on-premises instances to which to add tags.
attopiInstanceNames :: Lens' AddTagsToOnPremisesInstances [Text]
attopiInstanceNames = lens _attopiInstanceNames (\ s a -> s{_attopiInstanceNames = a}) . _Coerce

instance AWSRequest AddTagsToOnPremisesInstances
         where
        type Rs AddTagsToOnPremisesInstances =
             AddTagsToOnPremisesInstancesResponse
        request = postJSON codeDeploy
        response
          = receiveNull AddTagsToOnPremisesInstancesResponse'

instance Hashable AddTagsToOnPremisesInstances where

instance NFData AddTagsToOnPremisesInstances where

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
              (catMaybes
                 [Just ("tags" .= _attopiTags),
                  Just ("instanceNames" .= _attopiInstanceNames)])

instance ToPath AddTagsToOnPremisesInstances where
        toPath = const "/"

instance ToQuery AddTagsToOnPremisesInstances where
        toQuery = const mempty

-- | /See:/ 'addTagsToOnPremisesInstancesResponse' smart constructor.
data AddTagsToOnPremisesInstancesResponse =
  AddTagsToOnPremisesInstancesResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddTagsToOnPremisesInstancesResponse' with the minimum fields required to make a request.
--
addTagsToOnPremisesInstancesResponse
    :: AddTagsToOnPremisesInstancesResponse
addTagsToOnPremisesInstancesResponse = AddTagsToOnPremisesInstancesResponse'


instance NFData AddTagsToOnPremisesInstancesResponse
         where
