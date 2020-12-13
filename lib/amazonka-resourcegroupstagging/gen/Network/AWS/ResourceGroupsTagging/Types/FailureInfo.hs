{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroupsTagging.Types.FailureInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroupsTagging.Types.FailureInfo
  ( FailureInfo (..),

    -- * Smart constructor
    mkFailureInfo,

    -- * Lenses
    fiErrorCode,
    fiErrorMessage,
    fiStatusCode,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.ResourceGroupsTagging.Types.ResourceErrorCode

-- | Information about the errors that are returned for each failed resource. This information can include @InternalServiceException@ and @InvalidParameterException@ errors. It can also include any valid error code returned by the AWS service that hosts the resource that the ARN key represents.
--
-- The following are common error codes that you might receive from other AWS services:
--
--     * __InternalServiceException__ – This can mean that the Resource Groups Tagging API didn't receive a response from another AWS service. It can also mean the the resource type in the request is not supported by the Resource Groups Tagging API. In these cases, it's safe to retry the request and then call <http://docs.aws.amazon.com/resourcegroupstagging/latest/APIReference/API_GetResources.html GetResources> to verify the changes.
--
--
--     * __AccessDeniedException__ – This can mean that you need permission to calling tagging operations in the AWS service that contains the resource. For example, to use the Resource Groups Tagging API to tag a CloudWatch alarm resource, you need permission to call <http://docs.aws.amazon.com/resourcegroupstagging/latest/APIReference/API_TagResources.html @TagResources@ > /and/ <http://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_TagResource.html @TagResource@ > in the CloudWatch API.
--
--
-- For more information on errors that are generated from other AWS services, see the documentation for that service.
--
-- /See:/ 'mkFailureInfo' smart constructor.
data FailureInfo = FailureInfo'
  { -- | The code of the common error. Valid values include @InternalServiceException@ , @InvalidParameterException@ , and any valid error code returned by the AWS service that hosts the resource that you want to tag.
    errorCode :: Lude.Maybe ResourceErrorCode,
    -- | The message of the common error.
    errorMessage :: Lude.Maybe Lude.Text,
    -- | The HTTP status code of the common error.
    statusCode :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FailureInfo' with the minimum fields required to make a request.
--
-- * 'errorCode' - The code of the common error. Valid values include @InternalServiceException@ , @InvalidParameterException@ , and any valid error code returned by the AWS service that hosts the resource that you want to tag.
-- * 'errorMessage' - The message of the common error.
-- * 'statusCode' - The HTTP status code of the common error.
mkFailureInfo ::
  FailureInfo
mkFailureInfo =
  FailureInfo'
    { errorCode = Lude.Nothing,
      errorMessage = Lude.Nothing,
      statusCode = Lude.Nothing
    }

-- | The code of the common error. Valid values include @InternalServiceException@ , @InvalidParameterException@ , and any valid error code returned by the AWS service that hosts the resource that you want to tag.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fiErrorCode :: Lens.Lens' FailureInfo (Lude.Maybe ResourceErrorCode)
fiErrorCode = Lens.lens (errorCode :: FailureInfo -> Lude.Maybe ResourceErrorCode) (\s a -> s {errorCode = a} :: FailureInfo)
{-# DEPRECATED fiErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | The message of the common error.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fiErrorMessage :: Lens.Lens' FailureInfo (Lude.Maybe Lude.Text)
fiErrorMessage = Lens.lens (errorMessage :: FailureInfo -> Lude.Maybe Lude.Text) (\s a -> s {errorMessage = a} :: FailureInfo)
{-# DEPRECATED fiErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

-- | The HTTP status code of the common error.
--
-- /Note:/ Consider using 'statusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fiStatusCode :: Lens.Lens' FailureInfo (Lude.Maybe Lude.Int)
fiStatusCode = Lens.lens (statusCode :: FailureInfo -> Lude.Maybe Lude.Int) (\s a -> s {statusCode = a} :: FailureInfo)
{-# DEPRECATED fiStatusCode "Use generic-lens or generic-optics with 'statusCode' instead." #-}

instance Lude.FromJSON FailureInfo where
  parseJSON =
    Lude.withObject
      "FailureInfo"
      ( \x ->
          FailureInfo'
            Lude.<$> (x Lude..:? "ErrorCode")
            Lude.<*> (x Lude..:? "ErrorMessage")
            Lude.<*> (x Lude..:? "StatusCode")
      )
