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
-- Module      : Amazonka.ResourceGroupsTagging.Types.FailureInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResourceGroupsTagging.Types.FailureInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.ResourceGroupsTagging.Types.ResourceErrorCode

-- | Information about the errors that are returned for each failed resource.
-- This information can include @InternalServiceException@ and
-- @InvalidParameterException@ errors. It can also include any valid error
-- code returned by the Amazon Web Services service that hosts the resource
-- that the ARN key represents.
--
-- The following are common error codes that you might receive from other
-- Amazon Web Services services:
--
-- -   __InternalServiceException__ – This can mean that the Resource
--     Groups Tagging API didn\'t receive a response from another Amazon
--     Web Services service. It can also mean that the resource type in the
--     request is not supported by the Resource Groups Tagging API. In
--     these cases, it\'s safe to retry the request and then call
--     <https://docs.aws.amazon.com/resourcegroupstagging/latest/APIReference/API_GetResources.html GetResources>
--     to verify the changes.
--
-- -   __AccessDeniedException__ – This can mean that you need permission
--     to call the tagging operations in the Amazon Web Services service
--     that contains the resource. For example, to use the Resource Groups
--     Tagging API to tag a Amazon CloudWatch alarm resource, you need
--     permission to call both
--     <https://docs.aws.amazon.com/resourcegroupstagging/latest/APIReference/API_TagResources.html TagResources>
--     /and/
--     <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_TagResource.html TagResource>
--     in the CloudWatch API.
--
-- For more information on errors that are generated from other Amazon Web
-- Services services, see the documentation for that service.
--
-- /See:/ 'newFailureInfo' smart constructor.
data FailureInfo = FailureInfo'
  { -- | The message of the common error.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The code of the common error. Valid values include
    -- @InternalServiceException@, @InvalidParameterException@, and any valid
    -- error code returned by the Amazon Web Services service that hosts the
    -- resource that you want to tag.
    errorCode :: Prelude.Maybe ResourceErrorCode,
    -- | The HTTP status code of the common error.
    statusCode :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FailureInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorMessage', 'failureInfo_errorMessage' - The message of the common error.
--
-- 'errorCode', 'failureInfo_errorCode' - The code of the common error. Valid values include
-- @InternalServiceException@, @InvalidParameterException@, and any valid
-- error code returned by the Amazon Web Services service that hosts the
-- resource that you want to tag.
--
-- 'statusCode', 'failureInfo_statusCode' - The HTTP status code of the common error.
newFailureInfo ::
  FailureInfo
newFailureInfo =
  FailureInfo'
    { errorMessage = Prelude.Nothing,
      errorCode = Prelude.Nothing,
      statusCode = Prelude.Nothing
    }

-- | The message of the common error.
failureInfo_errorMessage :: Lens.Lens' FailureInfo (Prelude.Maybe Prelude.Text)
failureInfo_errorMessage = Lens.lens (\FailureInfo' {errorMessage} -> errorMessage) (\s@FailureInfo' {} a -> s {errorMessage = a} :: FailureInfo)

-- | The code of the common error. Valid values include
-- @InternalServiceException@, @InvalidParameterException@, and any valid
-- error code returned by the Amazon Web Services service that hosts the
-- resource that you want to tag.
failureInfo_errorCode :: Lens.Lens' FailureInfo (Prelude.Maybe ResourceErrorCode)
failureInfo_errorCode = Lens.lens (\FailureInfo' {errorCode} -> errorCode) (\s@FailureInfo' {} a -> s {errorCode = a} :: FailureInfo)

-- | The HTTP status code of the common error.
failureInfo_statusCode :: Lens.Lens' FailureInfo (Prelude.Maybe Prelude.Int)
failureInfo_statusCode = Lens.lens (\FailureInfo' {statusCode} -> statusCode) (\s@FailureInfo' {} a -> s {statusCode = a} :: FailureInfo)

instance Data.FromJSON FailureInfo where
  parseJSON =
    Data.withObject
      "FailureInfo"
      ( \x ->
          FailureInfo'
            Prelude.<$> (x Data..:? "ErrorMessage")
            Prelude.<*> (x Data..:? "ErrorCode")
            Prelude.<*> (x Data..:? "StatusCode")
      )

instance Prelude.Hashable FailureInfo where
  hashWithSalt _salt FailureInfo' {..} =
    _salt `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` errorCode
      `Prelude.hashWithSalt` statusCode

instance Prelude.NFData FailureInfo where
  rnf FailureInfo' {..} =
    Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf statusCode
