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
-- Module      : Network.AWS.CloudFront.CreateKeyGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a key group that you can use with <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html CloudFront signed URLs and signed cookies> .
--
--
-- To create a key group, you must specify at least one public key for the key group. After you create a key group, you can reference it from one or more cache behaviors. When you reference a key group in a cache behavior, CloudFront requires signed URLs or signed cookies for all requests that match the cache behavior. The URLs or cookies must be signed with a private key whose corresponding public key is in the key group. The signed URL or cookie contains information about which public key CloudFront should use to verify the signature. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving private content> in the /Amazon CloudFront Developer Guide/ .
module Network.AWS.CloudFront.CreateKeyGroup
  ( -- * Creating a Request
    createKeyGroup,
    CreateKeyGroup,

    -- * Request Lenses
    ckgKeyGroupConfig,

    -- * Destructuring the Response
    createKeyGroupResponse,
    CreateKeyGroupResponse,

    -- * Response Lenses
    ckgrsETag,
    ckgrsLocation,
    ckgrsKeyGroup,
    ckgrsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createKeyGroup' smart constructor.
newtype CreateKeyGroup = CreateKeyGroup'
  { _ckgKeyGroupConfig ::
      KeyGroupConfig
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateKeyGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ckgKeyGroupConfig' - A key group configuration.
createKeyGroup ::
  -- | 'ckgKeyGroupConfig'
  KeyGroupConfig ->
  CreateKeyGroup
createKeyGroup pKeyGroupConfig_ =
  CreateKeyGroup' {_ckgKeyGroupConfig = pKeyGroupConfig_}

-- | A key group configuration.
ckgKeyGroupConfig :: Lens' CreateKeyGroup KeyGroupConfig
ckgKeyGroupConfig = lens _ckgKeyGroupConfig (\s a -> s {_ckgKeyGroupConfig = a})

instance AWSRequest CreateKeyGroup where
  type Rs CreateKeyGroup = CreateKeyGroupResponse
  request = postXML cloudFront
  response =
    receiveXML
      ( \s h x ->
          CreateKeyGroupResponse'
            <$> (h .#? "ETag")
            <*> (h .#? "Location")
            <*> (parseXML x)
            <*> (pure (fromEnum s))
      )

instance Hashable CreateKeyGroup

instance NFData CreateKeyGroup

instance ToElement CreateKeyGroup where
  toElement =
    mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}KeyGroupConfig"
      . _ckgKeyGroupConfig

instance ToHeaders CreateKeyGroup where
  toHeaders = const mempty

instance ToPath CreateKeyGroup where
  toPath = const "/2020-05-31/key-group"

instance ToQuery CreateKeyGroup where
  toQuery = const mempty

-- | /See:/ 'createKeyGroupResponse' smart constructor.
data CreateKeyGroupResponse = CreateKeyGroupResponse'
  { _ckgrsETag ::
      !(Maybe Text),
    _ckgrsLocation :: !(Maybe Text),
    _ckgrsKeyGroup :: !(Maybe KeyGroup),
    _ckgrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateKeyGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ckgrsETag' - The identifier for this version of the key group.
--
-- * 'ckgrsLocation' - The URL of the key group.
--
-- * 'ckgrsKeyGroup' - The key group that was just created.
--
-- * 'ckgrsResponseStatus' - -- | The response status code.
createKeyGroupResponse ::
  -- | 'ckgrsResponseStatus'
  Int ->
  CreateKeyGroupResponse
createKeyGroupResponse pResponseStatus_ =
  CreateKeyGroupResponse'
    { _ckgrsETag = Nothing,
      _ckgrsLocation = Nothing,
      _ckgrsKeyGroup = Nothing,
      _ckgrsResponseStatus = pResponseStatus_
    }

-- | The identifier for this version of the key group.
ckgrsETag :: Lens' CreateKeyGroupResponse (Maybe Text)
ckgrsETag = lens _ckgrsETag (\s a -> s {_ckgrsETag = a})

-- | The URL of the key group.
ckgrsLocation :: Lens' CreateKeyGroupResponse (Maybe Text)
ckgrsLocation = lens _ckgrsLocation (\s a -> s {_ckgrsLocation = a})

-- | The key group that was just created.
ckgrsKeyGroup :: Lens' CreateKeyGroupResponse (Maybe KeyGroup)
ckgrsKeyGroup = lens _ckgrsKeyGroup (\s a -> s {_ckgrsKeyGroup = a})

-- | -- | The response status code.
ckgrsResponseStatus :: Lens' CreateKeyGroupResponse Int
ckgrsResponseStatus = lens _ckgrsResponseStatus (\s a -> s {_ckgrsResponseStatus = a})

instance NFData CreateKeyGroupResponse
