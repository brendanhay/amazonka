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
-- Module      : Network.AWS.ElasticBeanstalk.DescribePlatformVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a platform version. Provides full details. Compare to 'ListPlatformVersions' , which provides summary information about a list of platform versions.
--
--
-- For definitions of platform version and other platform-related terms, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/platforms-glossary.html AWS Elastic Beanstalk Platforms Glossary> .
module Network.AWS.ElasticBeanstalk.DescribePlatformVersion
  ( -- * Creating a Request
    describePlatformVersion,
    DescribePlatformVersion,

    -- * Request Lenses
    dPlatformARN,

    -- * Destructuring the Response
    describePlatformVersionResponse,
    DescribePlatformVersionResponse,

    -- * Response Lenses
    drsPlatformDescription,
    drsResponseStatus,
  )
where

import Network.AWS.ElasticBeanstalk.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describePlatformVersion' smart constructor.
newtype DescribePlatformVersion = DescribePlatformVersion'
  { _dPlatformARN ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribePlatformVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dPlatformARN' - The ARN of the platform version.
describePlatformVersion ::
  DescribePlatformVersion
describePlatformVersion =
  DescribePlatformVersion' {_dPlatformARN = Nothing}

-- | The ARN of the platform version.
dPlatformARN :: Lens' DescribePlatformVersion (Maybe Text)
dPlatformARN = lens _dPlatformARN (\s a -> s {_dPlatformARN = a})

instance AWSRequest DescribePlatformVersion where
  type Rs DescribePlatformVersion = DescribePlatformVersionResponse
  request = postQuery elasticBeanstalk
  response =
    receiveXMLWrapper
      "DescribePlatformVersionResult"
      ( \s h x ->
          DescribePlatformVersionResponse'
            <$> (x .@? "PlatformDescription") <*> (pure (fromEnum s))
      )

instance Hashable DescribePlatformVersion

instance NFData DescribePlatformVersion

instance ToHeaders DescribePlatformVersion where
  toHeaders = const mempty

instance ToPath DescribePlatformVersion where
  toPath = const "/"

instance ToQuery DescribePlatformVersion where
  toQuery DescribePlatformVersion' {..} =
    mconcat
      [ "Action" =: ("DescribePlatformVersion" :: ByteString),
        "Version" =: ("2010-12-01" :: ByteString),
        "PlatformArn" =: _dPlatformARN
      ]

-- | /See:/ 'describePlatformVersionResponse' smart constructor.
data DescribePlatformVersionResponse = DescribePlatformVersionResponse'
  { _drsPlatformDescription ::
      !( Maybe
           PlatformDescription
       ),
    _drsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribePlatformVersionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsPlatformDescription' - Detailed information about the platform version.
--
-- * 'drsResponseStatus' - -- | The response status code.
describePlatformVersionResponse ::
  -- | 'drsResponseStatus'
  Int ->
  DescribePlatformVersionResponse
describePlatformVersionResponse pResponseStatus_ =
  DescribePlatformVersionResponse'
    { _drsPlatformDescription =
        Nothing,
      _drsResponseStatus = pResponseStatus_
    }

-- | Detailed information about the platform version.
drsPlatformDescription :: Lens' DescribePlatformVersionResponse (Maybe PlatformDescription)
drsPlatformDescription = lens _drsPlatformDescription (\s a -> s {_drsPlatformDescription = a})

-- | -- | The response status code.
drsResponseStatus :: Lens' DescribePlatformVersionResponse Int
drsResponseStatus = lens _drsResponseStatus (\s a -> s {_drsResponseStatus = a})

instance NFData DescribePlatformVersionResponse
