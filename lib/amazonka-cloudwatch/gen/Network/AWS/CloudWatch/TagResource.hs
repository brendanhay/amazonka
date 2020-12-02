{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.TagResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Assigns one or more tags (key-value pairs) to the specified CloudWatch resource. Currently, the only CloudWatch resources that can be tagged are alarms and Contributor Insights rules.
--
--
-- Tags can help you organize and categorize your resources. You can also use them to scope user permissions by granting a user permission to access or change only resources with certain tag values.
--
-- Tags don't have any semantic meaning to AWS and are interpreted strictly as strings of characters.
--
-- You can use the @TagResource@ action with an alarm that already has tags. If you specify a new tag key for the alarm, this tag is appended to the list of tags associated with the alarm. If you specify a tag key that is already associated with the alarm, the new tag value that you specify replaces the previous value for that tag.
--
-- You can associate as many as 50 tags with a CloudWatch resource.
module Network.AWS.CloudWatch.TagResource
  ( -- * Creating a Request
    tagResource,
    TagResource,

    -- * Request Lenses
    trResourceARN,
    trTags,

    -- * Destructuring the Response
    tagResourceResponse,
    TagResourceResponse,

    -- * Response Lenses
    trrsResponseStatus,
  )
where

import Network.AWS.CloudWatch.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'tagResource' smart constructor.
data TagResource = TagResource'
  { _trResourceARN :: !Text,
    _trTags :: ![Tag]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TagResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'trResourceARN' - The ARN of the CloudWatch resource that you're adding tags to. The ARN format of an alarm is @arn:aws:cloudwatch:/Region/ :/account-id/ :alarm:/alarm-name/ @  The ARN format of a Contributor Insights rule is @arn:aws:cloudwatch:/Region/ :/account-id/ :insight-rule:/insight-rule-name/ @  For more information about ARN format, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/list_amazoncloudwatch.html#amazoncloudwatch-resources-for-iam-policies Resource Types Defined by Amazon CloudWatch> in the /Amazon Web Services General Reference/ .
--
-- * 'trTags' - The list of key-value pairs to associate with the alarm.
tagResource ::
  -- | 'trResourceARN'
  Text ->
  TagResource
tagResource pResourceARN_ =
  TagResource' {_trResourceARN = pResourceARN_, _trTags = mempty}

-- | The ARN of the CloudWatch resource that you're adding tags to. The ARN format of an alarm is @arn:aws:cloudwatch:/Region/ :/account-id/ :alarm:/alarm-name/ @  The ARN format of a Contributor Insights rule is @arn:aws:cloudwatch:/Region/ :/account-id/ :insight-rule:/insight-rule-name/ @  For more information about ARN format, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/list_amazoncloudwatch.html#amazoncloudwatch-resources-for-iam-policies Resource Types Defined by Amazon CloudWatch> in the /Amazon Web Services General Reference/ .
trResourceARN :: Lens' TagResource Text
trResourceARN = lens _trResourceARN (\s a -> s {_trResourceARN = a})

-- | The list of key-value pairs to associate with the alarm.
trTags :: Lens' TagResource [Tag]
trTags = lens _trTags (\s a -> s {_trTags = a}) . _Coerce

instance AWSRequest TagResource where
  type Rs TagResource = TagResourceResponse
  request = postQuery cloudWatch
  response =
    receiveXMLWrapper
      "TagResourceResult"
      (\s h x -> TagResourceResponse' <$> (pure (fromEnum s)))

instance Hashable TagResource

instance NFData TagResource

instance ToHeaders TagResource where
  toHeaders = const mempty

instance ToPath TagResource where
  toPath = const "/"

instance ToQuery TagResource where
  toQuery TagResource' {..} =
    mconcat
      [ "Action" =: ("TagResource" :: ByteString),
        "Version" =: ("2010-08-01" :: ByteString),
        "ResourceARN" =: _trResourceARN,
        "Tags" =: toQueryList "member" _trTags
      ]

-- | /See:/ 'tagResourceResponse' smart constructor.
newtype TagResourceResponse = TagResourceResponse'
  { _trrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TagResourceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'trrsResponseStatus' - -- | The response status code.
tagResourceResponse ::
  -- | 'trrsResponseStatus'
  Int ->
  TagResourceResponse
tagResourceResponse pResponseStatus_ =
  TagResourceResponse' {_trrsResponseStatus = pResponseStatus_}

-- | -- | The response status code.
trrsResponseStatus :: Lens' TagResourceResponse Int
trrsResponseStatus = lens _trrsResponseStatus (\s a -> s {_trrsResponseStatus = a})

instance NFData TagResourceResponse
