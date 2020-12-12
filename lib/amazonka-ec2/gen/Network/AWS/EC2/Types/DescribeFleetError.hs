{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DescribeFleetError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DescribeFleetError
  ( DescribeFleetError (..),

    -- * Smart constructor
    mkDescribeFleetError,

    -- * Lenses
    dfeLifecycle,
    dfeLaunchTemplateAndOverrides,
    dfeErrorCode,
    dfeErrorMessage,
  )
where

import Network.AWS.EC2.Types.InstanceLifecycle
import Network.AWS.EC2.Types.LaunchTemplateAndOverridesResponse
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the instances that could not be launched by the fleet.
--
-- /See:/ 'mkDescribeFleetError' smart constructor.
data DescribeFleetError = DescribeFleetError'
  { lifecycle ::
      Lude.Maybe InstanceLifecycle,
    launchTemplateAndOverrides ::
      Lude.Maybe LaunchTemplateAndOverridesResponse,
    errorCode :: Lude.Maybe Lude.Text,
    errorMessage :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeFleetError' with the minimum fields required to make a request.
--
-- * 'errorCode' - The error code that indicates why the instance could not be launched. For more information about error codes, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html.html Error Codes> .
-- * 'errorMessage' - The error message that describes why the instance could not be launched. For more information about error messages, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html.html Error Codes> .
-- * 'launchTemplateAndOverrides' - The launch templates and overrides that were used for launching the instances. The values that you specify in the Overrides replace the values in the launch template.
-- * 'lifecycle' - Indicates if the instance that could not be launched was a Spot Instance or On-Demand Instance.
mkDescribeFleetError ::
  DescribeFleetError
mkDescribeFleetError =
  DescribeFleetError'
    { lifecycle = Lude.Nothing,
      launchTemplateAndOverrides = Lude.Nothing,
      errorCode = Lude.Nothing,
      errorMessage = Lude.Nothing
    }

-- | Indicates if the instance that could not be launched was a Spot Instance or On-Demand Instance.
--
-- /Note:/ Consider using 'lifecycle' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfeLifecycle :: Lens.Lens' DescribeFleetError (Lude.Maybe InstanceLifecycle)
dfeLifecycle = Lens.lens (lifecycle :: DescribeFleetError -> Lude.Maybe InstanceLifecycle) (\s a -> s {lifecycle = a} :: DescribeFleetError)
{-# DEPRECATED dfeLifecycle "Use generic-lens or generic-optics with 'lifecycle' instead." #-}

-- | The launch templates and overrides that were used for launching the instances. The values that you specify in the Overrides replace the values in the launch template.
--
-- /Note:/ Consider using 'launchTemplateAndOverrides' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfeLaunchTemplateAndOverrides :: Lens.Lens' DescribeFleetError (Lude.Maybe LaunchTemplateAndOverridesResponse)
dfeLaunchTemplateAndOverrides = Lens.lens (launchTemplateAndOverrides :: DescribeFleetError -> Lude.Maybe LaunchTemplateAndOverridesResponse) (\s a -> s {launchTemplateAndOverrides = a} :: DescribeFleetError)
{-# DEPRECATED dfeLaunchTemplateAndOverrides "Use generic-lens or generic-optics with 'launchTemplateAndOverrides' instead." #-}

-- | The error code that indicates why the instance could not be launched. For more information about error codes, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html.html Error Codes> .
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfeErrorCode :: Lens.Lens' DescribeFleetError (Lude.Maybe Lude.Text)
dfeErrorCode = Lens.lens (errorCode :: DescribeFleetError -> Lude.Maybe Lude.Text) (\s a -> s {errorCode = a} :: DescribeFleetError)
{-# DEPRECATED dfeErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | The error message that describes why the instance could not be launched. For more information about error messages, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html.html Error Codes> .
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfeErrorMessage :: Lens.Lens' DescribeFleetError (Lude.Maybe Lude.Text)
dfeErrorMessage = Lens.lens (errorMessage :: DescribeFleetError -> Lude.Maybe Lude.Text) (\s a -> s {errorMessage = a} :: DescribeFleetError)
{-# DEPRECATED dfeErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

instance Lude.FromXML DescribeFleetError where
  parseXML x =
    DescribeFleetError'
      Lude.<$> (x Lude..@? "lifecycle")
      Lude.<*> (x Lude..@? "launchTemplateAndOverrides")
      Lude.<*> (x Lude..@? "errorCode")
      Lude.<*> (x Lude..@? "errorMessage")
