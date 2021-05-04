{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroupsTagging.Types.FailureInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroupsTagging.Types.FailureInfo where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.ResourceGroupsTagging.Types.ResourceErrorCode

-- | Information about the errors that are returned for each failed resource.
-- This information can include @InternalServiceException@ and
-- @InvalidParameterException@ errors. It can also include any valid error
-- code returned by the AWS service that hosts the resource that the ARN
-- key represents.
--
-- The following are common error codes that you might receive from other
-- AWS services:
--
-- -   __InternalServiceException__ – This can mean that the Resource
--     Groups Tagging API didn\'t receive a response from another AWS
--     service. It can also mean the the resource type in the request is
--     not supported by the Resource Groups Tagging API. In these cases,
--     it\'s safe to retry the request and then call
--     <http://docs.aws.amazon.com/resourcegroupstagging/latest/APIReference/API_GetResources.html GetResources>
--     to verify the changes.
--
-- -   __AccessDeniedException__ – This can mean that you need permission
--     to calling tagging operations in the AWS service that contains the
--     resource. For example, to use the Resource Groups Tagging API to tag
--     a CloudWatch alarm resource, you need permission to call
--     <http://docs.aws.amazon.com/resourcegroupstagging/latest/APIReference/API_TagResources.html TagResources>
--     /and/
--     <http://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_TagResource.html TagResource>
--     in the CloudWatch API.
--
-- For more information on errors that are generated from other AWS
-- services, see the documentation for that service.
--
-- /See:/ 'newFailureInfo' smart constructor.
data FailureInfo = FailureInfo'
  { -- | The HTTP status code of the common error.
    statusCode :: Prelude.Maybe Prelude.Int,
    -- | The message of the common error.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The code of the common error. Valid values include
    -- @InternalServiceException@, @InvalidParameterException@, and any valid
    -- error code returned by the AWS service that hosts the resource that you
    -- want to tag.
    errorCode :: Prelude.Maybe ResourceErrorCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'FailureInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statusCode', 'failureInfo_statusCode' - The HTTP status code of the common error.
--
-- 'errorMessage', 'failureInfo_errorMessage' - The message of the common error.
--
-- 'errorCode', 'failureInfo_errorCode' - The code of the common error. Valid values include
-- @InternalServiceException@, @InvalidParameterException@, and any valid
-- error code returned by the AWS service that hosts the resource that you
-- want to tag.
newFailureInfo ::
  FailureInfo
newFailureInfo =
  FailureInfo'
    { statusCode = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      errorCode = Prelude.Nothing
    }

-- | The HTTP status code of the common error.
failureInfo_statusCode :: Lens.Lens' FailureInfo (Prelude.Maybe Prelude.Int)
failureInfo_statusCode = Lens.lens (\FailureInfo' {statusCode} -> statusCode) (\s@FailureInfo' {} a -> s {statusCode = a} :: FailureInfo)

-- | The message of the common error.
failureInfo_errorMessage :: Lens.Lens' FailureInfo (Prelude.Maybe Prelude.Text)
failureInfo_errorMessage = Lens.lens (\FailureInfo' {errorMessage} -> errorMessage) (\s@FailureInfo' {} a -> s {errorMessage = a} :: FailureInfo)

-- | The code of the common error. Valid values include
-- @InternalServiceException@, @InvalidParameterException@, and any valid
-- error code returned by the AWS service that hosts the resource that you
-- want to tag.
failureInfo_errorCode :: Lens.Lens' FailureInfo (Prelude.Maybe ResourceErrorCode)
failureInfo_errorCode = Lens.lens (\FailureInfo' {errorCode} -> errorCode) (\s@FailureInfo' {} a -> s {errorCode = a} :: FailureInfo)

instance Prelude.FromJSON FailureInfo where
  parseJSON =
    Prelude.withObject
      "FailureInfo"
      ( \x ->
          FailureInfo'
            Prelude.<$> (x Prelude..:? "StatusCode")
            Prelude.<*> (x Prelude..:? "ErrorMessage")
            Prelude.<*> (x Prelude..:? "ErrorCode")
      )

instance Prelude.Hashable FailureInfo

instance Prelude.NFData FailureInfo
