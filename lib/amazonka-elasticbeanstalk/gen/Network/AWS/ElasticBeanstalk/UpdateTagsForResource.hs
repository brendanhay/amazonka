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
-- Module      : Network.AWS.ElasticBeanstalk.UpdateTagsForResource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update the list of tags applied to an AWS Elastic Beanstalk resource. Two lists can be passed: @TagsToAdd@ for tags to add or update, and @TagsToRemove@ .
--
--
-- Currently, Elastic Beanstalk only supports tagging of Elastic Beanstalk environments. For details about environment tagging, see <http://docs.aws.amazon.com/elasticbeanstalk/latest/dg/using-features.tagging.html Tagging Resources in Your Elastic Beanstalk Environment> .
--
-- If you create a custom IAM user policy to control permission to this operation, specify one of the following two virtual actions (or both) instead of the API operation name:
--
--     * elasticbeanstalk:AddTags    * Controls permission to call @UpdateTagsForResource@ and pass a list of tags to add in the @TagsToAdd@ parameter.
--
--     * elasticbeanstalk:RemoveTags    * Controls permission to call @UpdateTagsForResource@ and pass a list of tag keys to remove in the @TagsToRemove@ parameter.
--
--
--
-- For details about creating a custom user policy, see <http://docs.aws.amazon.com/elasticbeanstalk/latest/dg/AWSHowTo.iam.managed-policies.html#AWSHowTo.iam.policies Creating a Custom User Policy> .
--
module Network.AWS.ElasticBeanstalk.UpdateTagsForResource
    (
    -- * Creating a Request
      updateTagsForResource
    , UpdateTagsForResource
    -- * Request Lenses
    , utfrTagsToRemove
    , utfrTagsToAdd
    , utfrResourceARN

    -- * Destructuring the Response
    , updateTagsForResourceResponse
    , UpdateTagsForResourceResponse
    ) where

import Network.AWS.ElasticBeanstalk.Types
import Network.AWS.ElasticBeanstalk.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateTagsForResource' smart constructor.
data UpdateTagsForResource = UpdateTagsForResource'
  { _utfrTagsToRemove :: !(Maybe [Text])
  , _utfrTagsToAdd    :: !(Maybe [Tag])
  , _utfrResourceARN  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateTagsForResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'utfrTagsToRemove' - A list of tag keys to remove. If a tag key doesn't exist, it is silently ignored.
--
-- * 'utfrTagsToAdd' - A list of tags to add or update. If a key of an existing tag is added, the tag's value is updated.
--
-- * 'utfrResourceARN' - The Amazon Resource Name (ARN) of the resouce to be updated. Must be the ARN of an Elastic Beanstalk environment.
updateTagsForResource
    :: Text -- ^ 'utfrResourceARN'
    -> UpdateTagsForResource
updateTagsForResource pResourceARN_ =
  UpdateTagsForResource'
    { _utfrTagsToRemove = Nothing
    , _utfrTagsToAdd = Nothing
    , _utfrResourceARN = pResourceARN_
    }


-- | A list of tag keys to remove. If a tag key doesn't exist, it is silently ignored.
utfrTagsToRemove :: Lens' UpdateTagsForResource [Text]
utfrTagsToRemove = lens _utfrTagsToRemove (\ s a -> s{_utfrTagsToRemove = a}) . _Default . _Coerce

-- | A list of tags to add or update. If a key of an existing tag is added, the tag's value is updated.
utfrTagsToAdd :: Lens' UpdateTagsForResource [Tag]
utfrTagsToAdd = lens _utfrTagsToAdd (\ s a -> s{_utfrTagsToAdd = a}) . _Default . _Coerce

-- | The Amazon Resource Name (ARN) of the resouce to be updated. Must be the ARN of an Elastic Beanstalk environment.
utfrResourceARN :: Lens' UpdateTagsForResource Text
utfrResourceARN = lens _utfrResourceARN (\ s a -> s{_utfrResourceARN = a})

instance AWSRequest UpdateTagsForResource where
        type Rs UpdateTagsForResource =
             UpdateTagsForResourceResponse
        request = postQuery elasticBeanstalk
        response = receiveNull UpdateTagsForResourceResponse'

instance Hashable UpdateTagsForResource where

instance NFData UpdateTagsForResource where

instance ToHeaders UpdateTagsForResource where
        toHeaders = const mempty

instance ToPath UpdateTagsForResource where
        toPath = const "/"

instance ToQuery UpdateTagsForResource where
        toQuery UpdateTagsForResource'{..}
          = mconcat
              ["Action" =: ("UpdateTagsForResource" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "TagsToRemove" =:
                 toQuery (toQueryList "member" <$> _utfrTagsToRemove),
               "TagsToAdd" =:
                 toQuery (toQueryList "member" <$> _utfrTagsToAdd),
               "ResourceArn" =: _utfrResourceARN]

-- | /See:/ 'updateTagsForResourceResponse' smart constructor.
data UpdateTagsForResourceResponse =
  UpdateTagsForResourceResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateTagsForResourceResponse' with the minimum fields required to make a request.
--
updateTagsForResourceResponse
    :: UpdateTagsForResourceResponse
updateTagsForResourceResponse = UpdateTagsForResourceResponse'


instance NFData UpdateTagsForResourceResponse where
