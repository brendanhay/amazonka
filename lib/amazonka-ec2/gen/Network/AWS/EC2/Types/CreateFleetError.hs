{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CreateFleetError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CreateFleetError
  ( CreateFleetError (..),

    -- * Smart constructor
    mkCreateFleetError,

    -- * Lenses
    cfeLifecycle,
    cfeLaunchTemplateAndOverrides,
    cfeErrorCode,
    cfeErrorMessage,
  )
where

import Network.AWS.EC2.Types.InstanceLifecycle
import Network.AWS.EC2.Types.LaunchTemplateAndOverridesResponse
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the instances that could not be launched by the fleet.
--
-- /See:/ 'mkCreateFleetError' smart constructor.
data CreateFleetError = CreateFleetError'
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

-- | Creates a value of 'CreateFleetError' with the minimum fields required to make a request.
--
-- * 'errorCode' - The error code that indicates why the instance could not be launched. For more information about error codes, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html.html Error Codes> .
-- * 'errorMessage' - The error message that describes why the instance could not be launched. For more information about error messages, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html.html Error Codes> .
-- * 'launchTemplateAndOverrides' - The launch templates and overrides that were used for launching the instances. The values that you specify in the Overrides replace the values in the launch template.
-- * 'lifecycle' - Indicates if the instance that could not be launched was a Spot Instance or On-Demand Instance.
mkCreateFleetError ::
  CreateFleetError
mkCreateFleetError =
  CreateFleetError'
    { lifecycle = Lude.Nothing,
      launchTemplateAndOverrides = Lude.Nothing,
      errorCode = Lude.Nothing,
      errorMessage = Lude.Nothing
    }

-- | Indicates if the instance that could not be launched was a Spot Instance or On-Demand Instance.
--
-- /Note:/ Consider using 'lifecycle' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfeLifecycle :: Lens.Lens' CreateFleetError (Lude.Maybe InstanceLifecycle)
cfeLifecycle = Lens.lens (lifecycle :: CreateFleetError -> Lude.Maybe InstanceLifecycle) (\s a -> s {lifecycle = a} :: CreateFleetError)
{-# DEPRECATED cfeLifecycle "Use generic-lens or generic-optics with 'lifecycle' instead." #-}

-- | The launch templates and overrides that were used for launching the instances. The values that you specify in the Overrides replace the values in the launch template.
--
-- /Note:/ Consider using 'launchTemplateAndOverrides' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfeLaunchTemplateAndOverrides :: Lens.Lens' CreateFleetError (Lude.Maybe LaunchTemplateAndOverridesResponse)
cfeLaunchTemplateAndOverrides = Lens.lens (launchTemplateAndOverrides :: CreateFleetError -> Lude.Maybe LaunchTemplateAndOverridesResponse) (\s a -> s {launchTemplateAndOverrides = a} :: CreateFleetError)
{-# DEPRECATED cfeLaunchTemplateAndOverrides "Use generic-lens or generic-optics with 'launchTemplateAndOverrides' instead." #-}

-- | The error code that indicates why the instance could not be launched. For more information about error codes, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html.html Error Codes> .
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfeErrorCode :: Lens.Lens' CreateFleetError (Lude.Maybe Lude.Text)
cfeErrorCode = Lens.lens (errorCode :: CreateFleetError -> Lude.Maybe Lude.Text) (\s a -> s {errorCode = a} :: CreateFleetError)
{-# DEPRECATED cfeErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | The error message that describes why the instance could not be launched. For more information about error messages, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html.html Error Codes> .
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfeErrorMessage :: Lens.Lens' CreateFleetError (Lude.Maybe Lude.Text)
cfeErrorMessage = Lens.lens (errorMessage :: CreateFleetError -> Lude.Maybe Lude.Text) (\s a -> s {errorMessage = a} :: CreateFleetError)
{-# DEPRECATED cfeErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

instance Lude.FromXML CreateFleetError where
  parseXML x =
    CreateFleetError'
      Lude.<$> (x Lude..@? "lifecycle")
      Lude.<*> (x Lude..@? "launchTemplateAndOverrides")
      Lude.<*> (x Lude..@? "errorCode")
      Lude.<*> (x Lude..@? "errorMessage")
