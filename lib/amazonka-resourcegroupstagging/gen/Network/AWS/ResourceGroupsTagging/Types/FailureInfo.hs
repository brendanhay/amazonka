{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroupsTagging.Types.FailureInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroupsTagging.Types.FailureInfo where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.ResourceGroupsTagging.Types.ResourceErrorCode

-- | Information about the errors that are returned for each failed resource. This information can include @InternalServiceException@ and @InvalidParameterException@ errors. It can also include any valid error code returned by the AWS service that hosts the resource that the ARN key represents.
--
--
-- The following are common error codes that you might receive from other AWS services:
--
--     * __InternalServiceException__ – This can mean that the Resource Groups Tagging API didn't receive a response from another AWS service. It can also mean the the resource type in the request is not supported by the Resource Groups Tagging API. In these cases, it's safe to retry the request and then call <http://docs.aws.amazon.com/resourcegroupstagging/latest/APIReference/API_GetResources.html GetResources> to verify the changes.
--
--     * __AccessDeniedException__ – This can mean that you need permission to calling tagging operations in the AWS service that contains the resource. For example, to use the Resource Groups Tagging API to tag a CloudWatch alarm resource, you need permission to call <http://docs.aws.amazon.com/resourcegroupstagging/latest/APIReference/API_TagResources.html @TagResources@ > /and/ <http://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_TagResource.html @TagResource@ > in the CloudWatch API.
--
--
--
-- For more information on errors that are generated from other AWS services, see the documentation for that service.
--
--
-- /See:/ 'failureInfo' smart constructor.
data FailureInfo = FailureInfo'
  { _fiErrorCode ::
      !(Maybe ResourceErrorCode),
    _fiErrorMessage :: !(Maybe Text),
    _fiStatusCode :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FailureInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fiErrorCode' - The code of the common error. Valid values include @InternalServiceException@ , @InvalidParameterException@ , and any valid error code returned by the AWS service that hosts the resource that you want to tag.
--
-- * 'fiErrorMessage' - The message of the common error.
--
-- * 'fiStatusCode' - The HTTP status code of the common error.
failureInfo ::
  FailureInfo
failureInfo =
  FailureInfo'
    { _fiErrorCode = Nothing,
      _fiErrorMessage = Nothing,
      _fiStatusCode = Nothing
    }

-- | The code of the common error. Valid values include @InternalServiceException@ , @InvalidParameterException@ , and any valid error code returned by the AWS service that hosts the resource that you want to tag.
fiErrorCode :: Lens' FailureInfo (Maybe ResourceErrorCode)
fiErrorCode = lens _fiErrorCode (\s a -> s {_fiErrorCode = a})

-- | The message of the common error.
fiErrorMessage :: Lens' FailureInfo (Maybe Text)
fiErrorMessage = lens _fiErrorMessage (\s a -> s {_fiErrorMessage = a})

-- | The HTTP status code of the common error.
fiStatusCode :: Lens' FailureInfo (Maybe Int)
fiStatusCode = lens _fiStatusCode (\s a -> s {_fiStatusCode = a})

instance FromJSON FailureInfo where
  parseJSON =
    withObject
      "FailureInfo"
      ( \x ->
          FailureInfo'
            <$> (x .:? "ErrorCode")
            <*> (x .:? "ErrorMessage")
            <*> (x .:? "StatusCode")
      )

instance Hashable FailureInfo

instance NFData FailureInfo
