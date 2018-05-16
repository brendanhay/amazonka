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
-- Module      : Network.AWS.CodeDeploy.RemoveTagsFromOnPremisesInstances
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes one or more tags from one or more on-premises instances.
--
--
module Network.AWS.CodeDeploy.RemoveTagsFromOnPremisesInstances
    (
    -- * Creating a Request
      removeTagsFromOnPremisesInstances
    , RemoveTagsFromOnPremisesInstances
    -- * Request Lenses
    , rtfopiTags
    , rtfopiInstanceNames

    -- * Destructuring the Response
    , removeTagsFromOnPremisesInstancesResponse
    , RemoveTagsFromOnPremisesInstancesResponse
    ) where

import Network.AWS.CodeDeploy.Types
import Network.AWS.CodeDeploy.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a RemoveTagsFromOnPremisesInstances operation.
--
--
--
-- /See:/ 'removeTagsFromOnPremisesInstances' smart constructor.
data RemoveTagsFromOnPremisesInstances = RemoveTagsFromOnPremisesInstances'
  { _rtfopiTags          :: ![Tag]
  , _rtfopiInstanceNames :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RemoveTagsFromOnPremisesInstances' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtfopiTags' - The tag key-value pairs to remove from the on-premises instances.
--
-- * 'rtfopiInstanceNames' - The names of the on-premises instances from which to remove tags.
removeTagsFromOnPremisesInstances
    :: RemoveTagsFromOnPremisesInstances
removeTagsFromOnPremisesInstances =
  RemoveTagsFromOnPremisesInstances'
    {_rtfopiTags = mempty, _rtfopiInstanceNames = mempty}


-- | The tag key-value pairs to remove from the on-premises instances.
rtfopiTags :: Lens' RemoveTagsFromOnPremisesInstances [Tag]
rtfopiTags = lens _rtfopiTags (\ s a -> s{_rtfopiTags = a}) . _Coerce

-- | The names of the on-premises instances from which to remove tags.
rtfopiInstanceNames :: Lens' RemoveTagsFromOnPremisesInstances [Text]
rtfopiInstanceNames = lens _rtfopiInstanceNames (\ s a -> s{_rtfopiInstanceNames = a}) . _Coerce

instance AWSRequest RemoveTagsFromOnPremisesInstances
         where
        type Rs RemoveTagsFromOnPremisesInstances =
             RemoveTagsFromOnPremisesInstancesResponse
        request = postJSON codeDeploy
        response
          = receiveNull
              RemoveTagsFromOnPremisesInstancesResponse'

instance Hashable RemoveTagsFromOnPremisesInstances
         where

instance NFData RemoveTagsFromOnPremisesInstances
         where

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
              (catMaybes
                 [Just ("tags" .= _rtfopiTags),
                  Just ("instanceNames" .= _rtfopiInstanceNames)])

instance ToPath RemoveTagsFromOnPremisesInstances
         where
        toPath = const "/"

instance ToQuery RemoveTagsFromOnPremisesInstances
         where
        toQuery = const mempty

-- | /See:/ 'removeTagsFromOnPremisesInstancesResponse' smart constructor.
data RemoveTagsFromOnPremisesInstancesResponse =
  RemoveTagsFromOnPremisesInstancesResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RemoveTagsFromOnPremisesInstancesResponse' with the minimum fields required to make a request.
--
removeTagsFromOnPremisesInstancesResponse
    :: RemoveTagsFromOnPremisesInstancesResponse
removeTagsFromOnPremisesInstancesResponse =
  RemoveTagsFromOnPremisesInstancesResponse'


instance NFData
           RemoveTagsFromOnPremisesInstancesResponse
         where
